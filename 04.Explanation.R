# This script is written to explain the abnormal returns around announced date
# we have two kind of explanation variables, trading and factor
# and we run two kinds of regression, time series and cross-sectional

library(tidyverse)
library(magrittr)
library(lubridate)

library(modelr)
library(estimatr)
# library(sandwich)
# library(lmtest)
library(Formula)

library(rlang)
library(glue)
library(broom)
library(stargazer)
library(texreg)
library(multipanelfigure)
library(RColorBrewer)
library(ggthemes)
library(hrbrthemes)
library(latex2exp)
library(gt)
suppressMessages(extrafont::loadfonts())

library(DBI)
library(RSQLite)
library(dbplyr)

theme_set(
    theme_ipsum()
)
# self-defined function ----------------------------------------------------
# cluster the stocks in our sample according to the difference value of  
# one accounting index from prior quarter to current quarter
cluster_acc <- function(x = Acc_ind, accounting, grp_col_name,
                        diff_period = c(Accprd,(Accprd+days(1))%m+%months(-3)+days(-1)), 
                        break_point = c(0, 0.3, 0.7, 1),
                        break_label = c("Descend", "Neutral", "Ascend")
                        ) {
    # Input:
    # accounting: which accounting index is used to structure a group variable?
    # grp_col_name: the name of portfolio 
    # diff_period: the date of two quarters to take difference
     
    select(x, c("Stkcd", "Accper", all_of(accounting))) %>% 
    spread(key = 'Accper', value = get(accounting)) %>% 
    mutate("diff" = `-`(!!sym(as.character(diff_period[1])),
                        !!sym(as.character(diff_period[2])))
           ) %>% 
    transmute(Stkcd,
              !!grp_col_name := cut(diff, 
                                    breaks = quantile(diff, break_point, na.rm = TRUE), 
                                    labels = break_label,
                                    ordered_result = TRUE, include.lowest = TRUE)
              )
    
}

# function to calculate the value of factors, 
# imitating the structure process of Fama-French (1993)
calc_fct <- function(cluster, fct_name, class) {
    
    if (class == 't') {  # calculate the value of factor (t)
              
        tbl(QEA_db, "daily") %>%  
        filter(Stkcd %in% stk_sam, 
               TradingDate %in% !!seq(from = as.numeric(Accprd %m+% months(-1)), 
                                      to = as.numeric(Accprd %m+% months(+3)))
               ) %>%
        select(c('Stkcd', 'TradingDate', 'Dretnd')) %>% 
        collect() %>%  # read daily return around event window of stocks
        mutate('TradingDate' = as.Date(TradingDate, origin = "1970-01-01")) %>% 
        right_join(PLSclus, by = 'Stkcd') %>%  # PLS
        right_join(cluster, by = 'Stkcd') %>%  # accounting
        group_by(TradingDate, g_PLS, group) %>% 
        summarise("ptf_Ret" = mean(Dretnd), .groups = "drop") %>% 
        spread(key = 'group', value = 'ptf_Ret') %>% 
        # calculate the mean of portfolio returns (g_PLS)
        group_by(TradingDate) %>% 
        summarise(across(where(is.numeric), .funs = mean), .groups = "drop") %>% 
        # take the minus of the g_PLS mean returns between the portfolios g.RMW
        transmute(TradingDate, !!fct_name := Ascend - Descend)

    } else if (class == 'tau') {  # calculate the value of factor (tau)
        
        transmute(win_stk, Stkcd,
                  'Dret_tau' = map(win_eve, select, c('Timeline', 'Dretnd'))
                  ) %>% 
        unnest(cols = 'Dret_tau') %>% 
        inner_join(cluster, by = "Stkcd") %>% 
        group_by(Timeline, group) %>% 
        summarise("avgDret" = mean(Dretnd), .groups = "drop") %>% 
        spread(key = 'group', value = 'avgDret') %>% 
        transmute(Timeline, !!fct_name := Ascend - Descend)
        
    }
}


# regressions in three aspects
lm_trdff <- function(df, data_structure,  
                     SE_type = "stata", ..., 
                     formula_lhs, formula_rhs  
                     ) {
    # Input: 
    # df: the data will be running
    # data_structure: time series, cross-sectional, 
    #       or take group and time-line information as intersection term
    # SE_type : the type of standard errors
    # formula_lhs: the integer of left hand side in function Formula::formula
    # formula_rhs: the integer of right hand side in function Formula::formula
    
    # Output:
    # the regression result of class function base::lm
    
    if (data_structure == "ts") {  # run panel data regression
        
        reg_formula <- Formula(AbRet | AR_tau ~ 
                # using trading data as explanation variables 
                VMG_t + RMW_t + Amplitude + Turnover + Liquidility
                # take the group information as the interaction term
                | (VMG_t + RMW_t) * g_PLS + Amplitude + Turnover + Liquidility
                # factor, take the tau as the time line
                | VMG_tau + RMW_tau
                | (VMG_tau + RMW_tau) * g_PLS
                )
        
    } else if (data_structure == "cs") {  # run cross-sectional regression 
                
        reg_formula <- Formula(AbRet ~ 
                # run by every tau
                VMG_t * g_PLS + RMW_t * g_PLS  + Amplitude + Turnover + Liquidility 
                # when taking tau as interaction term, 
                # we should run regression just once using panel data
                | (VMG_t + RMW_t) + VMG_t:tau + RMW_t:tau + 
                  VMG_t:tau:g_PLS + RMW_t:tau:g_PLS + 
                  Amplitude + Turnover + Liquidility
                )
            
    } else stop("Please select a model correctly!")
    
    lm_result <- lm_robust(data = df, 
                           formula = formula(reg_formula, 
                                             lhs = formula_lhs, rhs = formula_rhs),
                           se_type = SE_type, ...
                           )

}

# Specifying the basic parameter of data --------------------------------
Accprd <- ymd('2017-12-31')
model_type <- "CH4"
value_base <- 'EPS'
Pretype <- 6L
Markettype <- 21L
subsam <- FALSE
file_char <- paste(Accprd, Pretype, Markettype, model_type, sep = '_')
# terms of factors# Asset pricing model, CAPM, CH3 or FF5? 
if (model_type == "CAPM") {
        ff_term <- c("mkt_rf")
} else if (model_type == 'FF3') {
        ff_term <- c("mkt_rf", "SMB", "HML")
} else if (model_type == "FF4") { 
        ff_term <- c("mkt_rf", "SMB", "HML", "WML")
} else if (model_type == "FF5") {
        ff_term <- c("mkt_rf", "SMB", "HML", "RMW", "CMA")
} else if (model_type == "CH3") {
    if (value_base %in% c("EPS", "CFPS")) {    
        ff_term <- c("mkt_rf", "SMB", "VMG")
    } else stop("Please input the class information of value.")
} else if (model_type == "CH4") {
    if (value_base %in% c("EPS", "CFPS")) {
        ff_term <- c("mkt_rf", "SMB", "VMG", "RMW")
    } else stop("Please input the class information of value factor.")
}

# Part I, join the data of stkeve, daily trading data, and grouped AR  -----
setwd(file.path('~/OneDrive/Data.backup/QEAData', model_type, year(Accprd), Accprd))
# link to SQLite database
QEA_db <- dbConnect(RSQLite::SQLite(), "~/OneDrive/Data.backup/QEAData/QEA_db.sqlite")
# Import data within event window ====
# wanted variables within event window, daily trading data and factors 
variable_name <- c("TradingDate", "Dretnd", "Dret_rf",  
                   "Timeline", "AbRet", ff_term, 
                   "Dnshrtrd", "Dnvaltrd", "Turnover", "Liquidility", 
                   "Amplitude", "Dsmvtll", "Dsmvosd")
                                      
win_stk <- left_join(
        by = c("Stkcd", "TradingDate", "Dretnd", "Dsmvosd", "Nrrdaydt", "Markettype"),
        # Import the data of daily trading and factors within event window
        read_csv(file = paste(file_char, "stkeve.csv", sep = '_'),
                 col_types = cols(Stkcd = col_character(),
                                  Markettype = col_factor(levels = c(1,4,16)),
                                  Indus = col_factor(),  # industry category
                                  Annowk = col_factor())  # the day of week
                 ),
        # Import originally trading data around event window              
        tbl(QEA_db, "daily") %>% 
        filter(# to speed up the procession, take a restrict condition on trading date
               TradingDate %in% !!seq(from = as.numeric(Accprd %m+% months(-1)), 
                                      to = as.numeric(Accprd %m+% months(+3)))
               ) %>%
        collect() %>% 
        mutate(# transform the date format and re-level the market type 
               'TradingDate' = as.Date(TradingDate, origin = "1970-01-01"),
               'Markettype' = factor(Markettype, levels = as.character(c(1, 4, 16)))
               )
        ) %>% 
        group_nest(Stkcd, .key = 'win_eve') %>% 
        # calculate the daily amplitude of stock
        mutate('win_eve' = map(win_eve, mutate, "Amplitude" = 
                                   c(NaN, (Hiprc-Loprc)[-1] / Clsprc[-length(Clsprc)])))

# import the data of abnormal returns calculated in script '03.CalCAR.R'
win_stk %<>% left_join(by = 'Stkcd',
        read_csv(file = paste(file_char, "gAR.csv", sep = '_'),
                 col_types = cols(Stkcd = col_character(),
                                  TradingDate = col_date(format = '%Y-%m-%d'),
                                  tau = col_factor(),  # timeline
                                  g_PLS = col_factor(),  # PLS - cluster result
                                  AR = col_double())  # abnormal return
                 ) %>% 
        rename('Timeline' = tau, 'AbRet' = AR) %>% 
        group_nest(Stkcd, .key = 'win_AR')
        ) %>% 
        transmute(Stkcd, 
                  'win_eve' = map2(win_eve, win_AR,
                                   ~ inner_join(.x, .y, by = "TradingDate") %>% 
                                       select(all_of(variable_name))
                                   )
                  )

# merge window data with group identity
PLSclus <- dir(path = file.path('~/OneDrive/Data.backup/QEAData/Matlab_PLS', year(Accprd)),
               pattern = paste("group", Accprd, Pretype, Markettype, model_type, sep = '_'),
               recursive = TRUE, full.names = TRUE
               ) %>% 
        read_csv(col_types = cols(Stkcd = col_character(),
                                  g_PLS = col_factor())
                 )
# the number of groups by PLS (su, 2016)
grp_num <- levels(PLSclus$g_PLS) %>% length()
# rename the group names rather than the integers
if (grp_num == 2L) {
        grp_name <- c("group one", "group two")
} else if (grp_num == 3L)  {
        grp_name <- c("group one", "group two", "group three")
} else if (grp_num == 4L) {
        grp_name <- c("group one", "group two", "group three", "group four")
} else print("The group number is not included in this script!")
# re-level the order of group levels
PLSclus %<>% mutate('g_PLS' = factor(g_PLS, 
                                     levels = as.character(1:grp_num),
                                     labels = grp_name)
                    )
win_stk %<>% inner_join(PLSclus, ., by = 'Stkcd')  # join procession

# weather to take a subset sample to analysis at below script
if (is.numeric(subsam)) {
   
        set.seed(subsam)
        stk_sam <- sample(pull(win_stk, Stkcd), size = subsam, replace = FALSE)
        win_stk %<>% filter(Stkcd %in% stk_sam)
    
} else {
    
        stk_sam <- pull(win_stk, Stkcd)
        print("Whole sample will be analysis.")

}
# the number of stocks in our sample this quarter
N <- nrow(win_stk)
# the length of estimate window
# should we abandon a part of window for the beauty of CAR path?
# if you want, just change the values of TS manually
if(`==`(length(unique(map_int(win_stk$win_eve, nrow))), 1)) {
    
        TS <- unique(map_int(win_stk$win_eve, nrow))
        # the time-line of event window at the level of tau
        timeline <- seq(-(TS - 1L) / 2, (TS - 1L) / 2, by = 1)
    
} else stop("The time series number of window trading data are not same.")

# Import quarterly accounting data and join to quarterly earnings report ====
load(file = "~/OneDrive/Data.backup/QEAData/ReportInfo.RData"); rm(PreRept)
Acc_ind <- tbl(QEA_db, "quarter") %>% 
        # subset the stocks in our quarterly sample
        filter(Stkcd %in% stk_sam, 
               Accper %in% !!as.numeric(c((Accprd+days(1))%m+%months(-3)+days(-1), Accprd))
               ) %>%
        # focus on parent company report
        filter(Typrep == "A") %>% select(-Typrep) %>%  
        collect() %>% 
        mutate('Accper' = as.Date(Accper, origin = "1970-01-01")) %>% 
        # join with information within quarterly earnings report
        right_join(filter(ReptInfo, Stkcd %in% stk_sam), 
                   by = c("Stkcd", "Accper"))

# statistical properties -------------------------------------------------
# plot the standard deviation of returns ====
title_char <- paste0('The standard deviation of the average daily returns and ',
                     'the average abnormal returns of grouped stocks')
caption_char <- paste('Accounting quarter,', 
                      (Accprd + days(1)) %m+% months(-3), '~', Accprd,
                      sep = ' ')
win_stk %>% 
transmute(g_PLS, 
          'DR_AR' = map(win_eve, select, c('Timeline', 'Dret_rf', 'AbRet'))
          ) %>% 
unnest(cols = 'DR_AR') %>% 
group_by(Timeline, g_PLS) %>% 
summarise("Real" = sd(Dret_rf), "Abnormal" = sd(AbRet), .groups = "drop") %>% 
gather(key = 'sd_type', value = 'sd_value', Real, Abnormal) %>% 
    ggplot(mapping = aes(x = as.integer(as.character(Timeline)), 
                         y = sd_value, colour = g_PLS)) + 
        geom_path(aes(linetype = sd_type)) + 
        scale_color_brewer(palette = "Set1") +
        scale_x_continuous(breaks = seq(-(TS-1L)/2L, (TS-1L)/2L, by = 5)) + 
        labs(title = title_char, caption = caption_char,
             y = "Standard deviation of returns", x = "Time line",
             linetype = 'Return', color = 'Classification')

ggsave(filename = paste(file_char, "DR_AR_gsd.pdf", sep = '_'), 
       width = 16, height = 9,
       scale = 0.75)

# Part II, plot the path figure of returns adjusted by accounting factor ----
# function to look the path of average returns within event window
# under the adjust effect of a accounting indicator
window_path <- function(cluster, ...) {
    # cluster: group result based on accounting indicator
    inner_join(win_stk, cluster, by = "Stkcd") %>% 
    transmute(g_PLS, group, 
             'Dret_tau' = map(win_eve, select, c('Timeline', 'Dretnd'))
             ) %>% 
    unnest(cols = 'Dret_tau') %>% 
    group_by_at(.vars = c('Timeline', 'group', 'g_PLS')) %>% 
    summarise("avgDret" = mean(Dretnd), .groups = "drop") %>% 
        ggplot(data = filter(., group != "Neutral"), 
               mapping = aes(x = as.integer(as.character(Timeline)), 
                             y = avgDret)
               ) + 
            geom_path(aes(linetype = group)) +
                labs(title = title_char, caption = caption_char, 
                     x = TeX("Timeline ($\\tau$)"), y = "Average daily return", 
                     colour = 'Classification',
                     linetype = "Adjustment") +
                facet_wrap(facets = vars(g_PLS), nrow = grp_num) + 
                scale_x_continuous(breaks = seq(-(TS-1L)/2L, (TS-1L)/2L, by = 5)) + 
            geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1.5) + 
            geom_rect(data = rect_index, 
                      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                      alpha = 0.3, show.legend = FALSE, inherit.aes = FALSE)

}
# PE - VMG ====
# cluster the stocks in our sample according to the difference of  
# one accounting index from prior quarter to current quarter
g_VMG <- cluster_acc(accounting = "F100601B", grp_col_name = 'group')
title_char <- paste("The averaged daily returns among stocks within event window")
# the index of the rectangle shadow in plot
rect_index <- tibble::tribble(
        ~tier, ~xmin, ~xmax, ~ymin, ~ymax,
        1,      -Inf,   -14,  -Inf,  +Inf,
        2,       +21,   Inf,  -Inf,  +Inf,)
# plot the adjustment effect of factor EPS (AMD)
window_path(cluster = g_VMG)
ggsave(filename = paste(file_char, "PE-Diff.pdf", sep = '_'), 
       width = 16, height = 9,
       scale = 0.85)
# EPS - RMW ====
g_RMW <- cluster_acc(accounting = "F090101B", grp_col_name = 'group')
title_char <- sub(pattern = "PE", replacement = "EPS", x = title_char)
window_path(cluster = g_RMW)
ggsave(filename = paste(file_char, "EPS-Diff.pdf", sep = '_'),
       width = 16, height = 9,
       scale = 0.85)

# Part III, Ordinary Least Squares with Robust Standard Errors ------------
# calculate the values of explanation factors ====
# trading date, t
factor_t <- inner_join(by = 'TradingDate',
        calc_fct(class = 't', cluster = g_VMG, fct_name = "VMG_t"), 
        calc_fct(class = 't', cluster = g_RMW, fct_name = "RMW_t")
        )
# time-line, tau 
factor_tau <- inner_join(by = 'Timeline',
        calc_fct(class = 'tau', cluster = g_VMG, fct_name = 'VMG_tau'),
        calc_fct(class = 'tau', cluster = g_RMW, fct_name = 'RMW_tau')
        )
dbDisconnect(QEA_db)

# Time series ==== 
# t
mutate(win_stk, 
      'win_eve' = map(win_eve, left_join, factor_t, by = "TradingDate")
      ) %>% 
unnest(cols = 'win_eve') %$% 
list(# run aggregate at once, but using Cluster-Robust variance
    lm_trdff(df = .,
             data_structure = "ts", 
             formula_lhs = 1, formula_rhs = 1,
             clusters = Stkcd), 
    # using group as interaction term
    lm_trdff(df = .,
             data_structure = "ts", 
             formula_lhs = 1, formula_rhs = 2)  
    ) %>% 
texreg(l = ., custom.model.names = c('Panel', 'Interact with group'),
       include.ci = FALSE, dcolumn = TRUE, booktabs = TRUE, digits = 3,
       override.se = map(., 'statistic'), override.pvalues = map(., 'p.value')) %>% 
cat()
## tau (maybe useless to paper)
mutate(win_stk, 
      'win_eve' = map(win_eve, left_join, factor_tau, by = "Timeline")
      ) %>% 
unnest(cols = 'win_eve') %$% 
list(# run aggregate at once, using Cluster-Robust variance
    lm_trdff(df = ., 
             data_structure = "ts", 
             formula_lhs = 1, formula_rhs = 3,
             clusters = Stkcd
             ),  
    # using group as interaction term
    lm_trdff(df = ., 
             data_structure = "ts",
             formula_lhs = 1, formula_rhs = 4
             ),
    # calculate the AR_tau grouped by time-line and g_PLS
    transmute(win_stk, g_PLS,
              'AR' = map(win_eve, ~ select(.x, all_of(c("Timeline", "AbRet"))))
              ) %>% 
    unnest(cols = 'AR') %>% 
    group_by(g_PLS, Timeline) %>% 
    summarise("AR_tau" = mean(AbRet), .groups = "drop") %>% 
    inner_join(factor_tau, by = "Timeline") %>% 
    lm_trdff(data_structure = "ts",
             formula_lhs = 2, formula_rhs = 4
             )
    ) %>% 
texreg(custom.model.names = c("Panel", "Interact with group", "Summarise"),
       include.ci = FALSE, dcolumn = TRUE, booktabs = TRUE, digits = 4,
       override.se = map(., 'statistic'), override.pvalues = map(., 'p.value')) %>% 
cat()

# cross-sectional ====
qtr_term <- c(-5:5)  # just running a part of event window
# the core idea is we employ RMW_t to explain AR_t once at a same tau (cross-sectional) 
# and we will focus on the difference of estimate coefficients among different tau and group
win_stk %>% 
mutate('win_eve' = map(win_eve, left_join, factor_t, by = "TradingDate")) %>%
unnest(cols = 'win_eve') %>% 
# re-level the time line (take the tau = 0 as benchmark in regression)
mutate('Timeline' = fct_relevel(Timeline, as.character(c(0, timeline[timeline != 0])))) %>% 
dplyr::rename(tau = Timeline) %>% 
filter(tau %in% as.character(qtr_term)) %>% 
lm_trdff(data_structure = "cs", formula_lhs = 1, formula_rhs = 2) %>% 
texreg(include.ci = FALSE, dcolumn = TRUE, booktabs = TRUE, digits = 3,
          override.se = .$statistic, override.pvalues = .$p.value) %>% 
gsub(pattern = "VMG\\_t", replacement = "VMG", fixed = T) %>%
gsub(pattern = "RMW\\_t", replacement = "RMM", fixed = T) %>%
gsub(pattern = "tau", replacement = "tau = ") %>%
gsub(pattern = "g\\_PLSgroup", replacement = "group", fixed = T) %>%
cat()

# Parameter visualization -------------------------------------------------
# function to visualize estimate values using graphic of path and point
stats_plot <- function(reg, inter_type, factor_name, ...) {

    # function to extract coefficients and p-value of regression
    data_extract <- function(str_grep) {
      
        df <- vector(mode = 'list', length = length(factor_name)) %>% 
            `names<-`(factor_name)
        
        for (i in seq_along(factor_name)) {
            str_factor <- paste0(factor_name[i], str_grep)
            df[[i]] <- tibble(
                'coef' = reg$coefficients %>% `[`(grep(str_factor, x = names(.))),
                'pv' =  reg$p.value %>% `[`(grep(str_factor, x = names(.)))
                )
        }
          
        if (any(map_int(df, nrow) != length(tau))) {
            stop(paste("The length of parameters is not same with tau's,",
                       "\nPlease revise your regular expression!")
                 )
        } else map_dfr(df, add_column, 'tau' = tau, .id = 'Factor') %>% 
                          arrange(Factor, tau)
        
    }
    
    # function to visualize parameters 
    draw_plot <- function(x) {
        ggplot(data = x, mapping = aes(x = tau, y = coef)) +
        geom_path(aes(linetype = Factor)) +
        geom_point(aes(colour = cut(pv, breaks = c(0, 0.05, 0.1, 1)))) +
        labs(title = title_char, caption = caption_char,
             y = 'The value of coefficients', x = TeX("Timeline ($\\tau$)"),
             colour = 'P-value') +
        scale_color_brewer(palette = 'Set1') + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1.5) 
    }
    
    if (inter_type == "tau") {  
        # focus on the parameters of factors that only interact with tau
        tau <- setdiff(qtr_term, 0)
        regexp_char <- "[[:punct:]]t[[:punct:]]tau[[:punct:]]?[[:digit:]]+$"
        title_char <- TeX('Factor interact with $\\tau$')
        data_extract(regexp_char) %>% draw_plot()
            
    } else if (inter_type == "grp") {  
        # compare the parameters value between groups (diff-in-diff-in-diff)
        tau <- c(0, setdiff(qtr_term, 0))
        regexp_char <- paste0("[[:punct:]]t[[:punct:]]tau[[:punct:]]?[[:digit:]]+", 
                              "[[:punct:]]g[[:punct:]]PLSgroup[[:blank:]]")
        reg_est <- data_extract(paste0(regexp_char, 'two'))
        
        if (grp_num == 2) {
          
            title_char <- TeX('Factor interact with $\\tau$, and as contrast to group one')
            draw_plot(reg_est)
        
        } else {  title_char <- TeX('Factor interact with $\\tau$ and multiple groups')
          
            if (grp_num == 3) {
                
                rbind(reg_est, data_extract(paste0(regexp_char, 'three'))) %>% 
                add_column('group' = rep(c('group two', 'group three'), 
                                         each = nrow(reg_est)
                                         )
                           ) %>% 
                draw_plot() + 
                facet_wrap(facets = vars(group), ncol = grp_num - 1)     
              
            } else if (grp_num == 4) {
                
                rbind(reg_est, 
                      data_extract(paste0(regexp_char, 'three')),
                      data_extract(paste0(regexp_char, 'four'))
                      ) %>% 
                add_column('group' = rep(c('group two', 'group three', 'group four'), 
                                         each = nrow(reg_est)
                                         )
                           ) %>% 
                draw_plot() + 
                facet_wrap(facets = vars(group), nrow = grp_num - 1)     
                
            }
        }
    }
}

qtr_term <- c(-15:10)  # just visual a part of event window
reg_panel <- win_stk %>% 
        mutate('win_eve' = map(win_eve, left_join, factor_t, by = "TradingDate")) %>%
        unnest(cols = 'win_eve') %>% 
        # re-level the time line (take the tau = 0 as benchmark in regression)
        mutate('Timeline' = fct_relevel(Timeline, as.character(c(0, timeline[timeline != 0])))) %>% 
        dplyr::rename(tau = Timeline) %>% 
        filter(tau %in% as.character(qtr_term)) %>% 
        lm_trdff(data_structure = "cs", formula_lhs = 1, formula_rhs = 2)

# factor:tau ====
stats_plot(reg = reg_panel, 
           inter_type = 'tau',
           factor_name = c('VMG', 'RMW')
           )

ggsave(filename = paste(file_char, "factor-tau.pdf", sep = '_'),
       width = 16, height = 9, scale = 0.75)
                            
# factor:tau:g_PLS, diff-diff-diff ====
stats_plot(reg = reg_panel, 
           inter_type = 'grp', 
           factor_name = c('VMG', 'RMW')
           )

ggsave(filename = paste(file_char, "factor-tau-grp.pdf", sep = '_'),
       width = 16, height = 9, scale = 0.75)
