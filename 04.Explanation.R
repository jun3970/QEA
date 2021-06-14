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
                        diff_period = c(Accprd[i],(Accprd[i]+days(1))%m+%months(-3)+days(-1)), 
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
                        !!sym(as.character(diff_period[2])))) %>% 
    transmute(Stkcd,
              !!grp_col_name := cut(diff, 
                                    breaks = quantile(diff, break_point, na.rm = TRUE), 
                                    labels = break_label,
                                    ordered_result = TRUE, include.lowest = TRUE)
              )
}

# function to calculate the value of factors, 
# imitating the structure process of Fama-French (1993)
calc_fct <- function(cluster, fct_name, class, PLSgroup = FALSE) {
    
    if (class == 't') {  # calculate the value of factor (t)
        
        df <- tbl(QEA_db, "daily") %>%  
            filter(Stkcd %in% stk_sam, 
                   TradingDate %in% !!seq(from = as.numeric(Accprd[i] %m+% months(-1)), 
                                          to = as.numeric(Accprd[i] %m+% months(+3)))
                   ) %>%
            select(c('Stkcd', 'TradingDate', 'Dretnd')) %>% 
            collect() %>%  # read daily return around event window of stocks
            mutate('TradingDate' = as.Date(TradingDate, origin = "1970-01-01"))
      
        if (PLSgroup) {
          
            right_join(df, PLSclus, by = 'Stkcd') %>%  # PLS
            right_join(cluster, by = 'Stkcd') %>%  # accounting
            group_by(TradingDate, g_PLS, group) %>% 
            summarise("ptf_Ret" = mean(Dretnd), .groups = "drop") %>% 
            spread(key = 'group', value = 'ptf_Ret') %>% 
            # calculate the mean of portfolio returns (g_PLS)
            group_by(TradingDate) %>% 
            summarise(across(where(is.numeric),  mean), .groups = "drop") %>% 
            # take the minus of the g_PLS mean returns between the portfolios g.RMW
            transmute(TradingDate, !!fct_name := Ascend - Descend)
              
        } else {
          
            right_join(df, cluster, by = 'Stkcd') %>%
            group_by(TradingDate, group) %>% 
            summarise("ptf_Ret" = mean(Dretnd), .groups = "drop") %>% 
            spread(key = 'group', value = 'ptf_Ret') %>% 
            transmute(TradingDate, !!fct_name := Ascend - Descend)
        }

    } else if (class == 'tau') {  # calculate the value of factor (tau)
        
        transmute(win_stk, Stkcd,
                  'Dret_tau' = map(win_eve, select, c('Timeline', 'Dretnd'))) %>% 
        unnest(cols = 'Dret_tau') %>% 
        inner_join(cluster, by = "Stkcd") %>% 
        group_by(Timeline, group) %>% 
        summarise("avgDret" = mean(Dretnd), .groups = "drop") %>% 
        spread(key = 'group', value = 'avgDret') %>% 
        transmute(Timeline, !!fct_name := Ascend - Descend)
    }
}

# using library `Formula` to form multiple regressions
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
                | (VMG_tau + RMW_tau) * g_PLS)
        
    } else if (data_structure == "cs") {  # run cross-sectional regression 
        # when we taking quarter or tau as interaction term, 
        # we should run regression just once using panel data
        reg_formula <- Formula(AbRet ~ 
                VMG_t + RMW_t + (VMG_t + RMW_t) : tau + Amplitude + Turnover + Liquidility
                | (VMG_t + RMW_t) + (VMG_t + RMW_t):tau + (VMG_t + RMW_t):tau:g_PLS + 
                      Amplitude + Turnover + Liquidility
                | (VMG_t + RMW_t) + (VMG_t + RMW_t):tau + (VMG_t + RMW_t):tau:Q + 
                      Amplitude + Turnover + Liquidility)
            
    } else stop("Please select a model correctly!")
    
    lm_result <- lm_robust(data = df, 
                           formula = formula(reg_formula, 
                                             lhs = formula_lhs, rhs = formula_rhs),
                           se_type = SE_type, ...)

}

# function to look the path of average returns within event window
# under the adjust effect of a accounting indicator
window_path <- function(cluster, ...) {
    # cluster: group result based on accounting indicator
    df <- inner_join(win_stk, cluster, by = "Stkcd") %>% 
          transmute(g_PLS, group, 
                   'Dret_tau' = map(win_eve, select, c('Timeline', 'Dretnd'))) %>% 
          unnest(cols = 'Dret_tau') %$% 
          rbind(# grouped in accounting indicator and PLS
                group_by_at(., .vars = c('Timeline', 'group', 'g_PLS')) %>% 
                summarise("avgDret" = mean(Dretnd), .groups = "drop"),
                # grouped just in accounting indicator
                group_by_at(., .vars = c('Timeline', 'group')) %>% 
                summarise("avgDret" = mean(Dretnd), .groups = "drop") %>% 
                add_column('g_PLS' = c("Unclassified"), .before = 1)) %>% 
          mutate('g_PLS' = fct_relevel(g_PLS, c('Unclassified', grp_name)))
    
    ggplot(data = filter(df, group != "Neutral"), 
           mapping = aes(x = as.integer(as.character(Timeline)), y = avgDret)) + 
    geom_path(aes(linetype = group)) +
        labs(# title = title_char, 
             caption = caption_char, 
             x = TeX("Timeline ($\\tau$)"), y = "Average daily return", 
             colour = 'Classification', linetype = "Adjustment") +
        facet_wrap(facets = vars(g_PLS), nrow = grp_num + 1) + 
        scale_x_continuous(breaks = seq(-(TS-1L)/2L, (TS-1L)/2L, by = 5)) + 
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1.5) + 
    geom_rect(data = rect_index, 
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              alpha = 0.3, show.legend = FALSE, inherit.aes = FALSE) + 
    theme(legend.position = "bottom")
}

# function to visualize estimate values using graphic of path and point
stats_plot <- function(reg, inter_type, factor_name, ...) {
    # function to extract coefficients and p-value of regression
    data_extract <- function(str_grep) {
        df <- vector(mode = 'list', length = length(factor_name)) %>% `names<-`(factor_name)
        for (i in seq_along(factor_name)) {
            df[[i]] <- tibble('coef' = reg$coefficients %>% 
                                  `[`(grep(paste0(factor_name[i], str_grep), x = names(.))),
                              'pv' =  reg$p.value %>% 
                                  `[`(grep(paste0(factor_name[i], str_grep), x = names(.))))
        }
        if (any(map_int(df, nrow) != length(tau))) {
            stop(paste("The length of parameters is not same with tau's,",
                       "\nPlease revise your regular expression!"))
        } else map_dfr(df, add_column, 'tau' = tau, .id = 'Factor')
    }
    # function to visualize parameters 
    draw_plot <- function(x) {
        ggplot(data = x, mapping = aes(x = tau, y = coef)) +
        geom_path(aes(linetype = Factor)) +
        geom_point(aes(colour = cut(pv, breaks = c(0, 0.05, 1)))) +
        labs(# title = title_char, 
             caption = caption_char,
             y = 'The value of coefficients', x = TeX("Timeline ($\\tau$)"),
             colour = 'P-value') +
        scale_color_brewer(palette = 'Set1') + 
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1.2) 
    }
    # body 
    tau <- setdiff(qtr_term, 0)
    if (inter_type == "tau") {
        # focus on the parameters of factors that only interact with tau
        title_char <- TeX('Coefficients value of factors interacted with $\\tau$')
        data_extract(str_grep = "[[:punct:]]t[[:punct:]]tau[[:punct:]]?[[:digit:]]+$") %>% 
        # # add tau as intercepts
        # rbind(tibble('Factor' = 'tau (Intercept)', 
        #              'tau' = tau, 
        #              'coef' = reg$coefficients %>% 
        #                         `[`(grep('^tau[[:punct:]]?[[:digit:]]+$', x = names(.))),
        #              'pv' =  reg$p.value %>% 
        #                         `[`(grep('^tau[[:punct:]]?[[:digit:]]+$', x = names(.)))
        #              )
        #       ) %>% 
        mutate('Factor' = factor(Factor, levels = c(# 'tau (Intercept)',
                                                    factor_name))
               ) %>% 
        arrange(Factor, tau) %>%
        draw_plot()
        
        ggsave(filename = paste(file_char, "factor-tau.pdf", sep = '_'),
               width = 16, height = 9, scale = 0.65)
            
    } else if (inter_type == "grp") {  
        # compare the parameters value between groups (diff-in-diff-in-diff)
        reg_est <- data_extract(str_grep = "[[:punct:]]t[[:punct:]]tau[[:punct:]]?[[:digit:]]+$") 

        if (grp_num == 2) {
          
            plot_list <- vector(mode = 'list', length = grp_num) %>% `names<-`(grp_name)
            
            title_char <- TeX('Coefficients value of factors interacted with $\\tau$ and group, Group A')
            plot_list$`Group A` <- reg_est %>% 
            mutate('Factor' = factor(Factor, levels = c(# 'tau (Intercept)',
                                                        factor_name))
                   ) %>%
            arrange(Factor, tau) %>%
            draw_plot()
            
            ggsave(plot = plot_list$`Group A`, 
                   filename = paste(file_char, "factor-tau-grp-A.pdf", sep = '_'),
                   width = 16, height = 9, scale = 0.65)
            
            tau <- c(0, setdiff(qtr_term, 0))
            title_char <- TeX(paste0('Coefficients value of factors interacted with $\\tau$ and group, ', 
                                     'as contrast to group A\'s'))
            plot_list$`Group B` <- 
            paste0('[[:punct:]]t[[:punct:]]tau[[:punct:]]?[[:digit:]]+', 
                   "[[:punct:]]g[[:punct:]]PLSGroup[[:blank:]]B") %>%
            data_extract() %>%
            # rbind(tibble('Factor' = 'tau (Intercept)',
            #              'tau' = tau,
            #              'coef' = reg$coefficients %>% `[`(grep(regexp_char, x = names(.))),
            #              'pv' =  reg$p.value %>% `[`(grep(regexp_char, x = names(.))))
            #       ) %>%
            mutate('Factor' = factor(Factor, levels = c(# 'tau (Intercept)',
                                                        factor_name))
                   ) %>%
            arrange(Factor, tau) %>%
            draw_plot()
            
            ggsave(plot = plot_list$`Group B`,
                   filename = paste(file_char, "factor-tau-grp-B.pdf", sep = '_'),
                   width = 16, height = 9, scale = 0.65)
            
        } else {  title_char <- TeX('Factors had interacted with $\\tau$ and multiple groups')
          
            if (grp_num == 3) {

            regexp_char_tau <- paste0('^tau[[:punct:]]?[[:digit:]]+', regexp_char, '[two|three]')
            # factor:tau:group
            rbind(reg_est,
                  data_extract(paste0("[[:punct:]]t[[:punct:]]tau[[:punct:]]?[[:digit:]]+",
                                      regexp_char, 'three'))
                  ) %>%
            add_column('group' = rep(c('group two', 'group three'),
                                     each = length(tau) * length(factor_name)),
                       .before = 1) %>%
            # tau:group(intercept)
            rbind(tibble('group' = rep(c('group two', 'group three'), each = length(tau)),
                         'Factor' = rep('tau (Intercept)', each = length(tau), times = grp_num - 1),
                         'tau' = rep(tau, times = grp_num - 1),
                         'coef' = reg$coefficients %>% `[`(grep(regexp_char_tau, x = names(.))),
                         'pv' =  reg$p.value %>% `[`(grep(regexp_char_tau, x = names(.))))
                  ) %>%
            mutate('Factor' = factor(Factor, levels = c(factor_name, 'tau (Intercept)'))) %>%
            arrange(group, Factor, tau) %>%
            draw_plot() +
            facet_wrap(facets = vars(group), nrow = grp_num - 1)
              
            } else stop(paste(Accprd[i], 'The number of groups is greater than two.'))
        }
    }
}

# Specifying the basic parameter of data --------------------------------
Accprd <- months(seq(from = 0, by = 3, length = 4*5)) %>% 
        mapply('%m+%', ymd('2017-03-31'), .) %>% # `[`(.!=ymd("2016-06-30")) %>% 
        base::as.Date(origin = '1970-01-01')
model_type <- "CH4"
value_base <- 'EPS'
Pretype <- 6L
Markettype <- 21L
subsam <- FALSE
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
# the index of the rectangle shadow in adjest effect plot
rect_index <- tibble::tribble(
        ~tier, ~xmin, ~xmax, ~ymin, ~ymax,
        1,      -Inf,   -14,  -Inf,  +Inf,
        2,       +21,   Inf,  -Inf,  +Inf,)

# Part I, join the data of stkeve, daily trading data, and grouped AR  -----
# link to SQLite database
QEA_db <- dbConnect(RSQLite::SQLite(), "~/OneDrive/Data.backup/QEAData/QEA_db.sqlite")
# wanted variables within event window, daily trading data and factors 
variable_name <- c("TradingDate", "Dretnd", "Dret_rf",  
                   "Timeline", "AbRet", ff_term, 
                   "Dnshrtrd", "Dnvaltrd", "Turnover", "Liquidility", 
                   "Amplitude", "Dsmvtll", "Dsmvosd")

reg_Accprd <- vector(mode = 'list', length = length(Accprd)) %>% 
    `names<-`(Accprd) %>% 
    map(~ vector(mode = 'list', length = 2L) %>% 
          set_names(c('data', 'regression')))

for (i in seq_along(Accprd)) {
    
    print(Accprd[i])
    setwd(file.path('~/OneDrive/Data.backup/QEAData', model_type, 
                    year(Accprd[i]), Accprd[i])
          )
    file_char <- paste(Accprd[i], Pretype, Markettype, model_type, sep = '_')
    
    # Import data within event window ====
    win_stk <- left_join(
            by = c("Stkcd", "TradingDate", "Dretnd", "Dsmvosd", "Nrrdaydt", "Markettype"),
            # Import the data of daily trading and factors within event window
            read_csv(file = paste(file_char, "stkeve.csv", sep = '_'),
                     col_types = cols(Stkcd = col_character(),
                                      Markettype = col_factor(levels = c('1','4','16')),
                                      Indus = col_factor(),  # industry category
                                      Annowk = col_factor())  # the day of week
                     ),
            # Import originally trading data around event window              
            tbl(QEA_db, "daily") %>% 
            filter(# to speed up the procession, take a restrict condition on trading date
                   TradingDate %in% !!seq(from = as.numeric(Accprd[i] %m+% months(-1)), 
                                          to = as.numeric(Accprd[i] %m+% months(+3)))
                   ) %>%
            collect() %>% 
            mutate(# transform the date format and re-level the market type 
                   'TradingDate' = as.Date(TradingDate, origin = "1970-01-01"),
                   'Markettype' = factor(Markettype, levels = as.character(c(1, 4, 16)))
                   )
            ) %>% 
            group_nest(Stkcd, .key = 'win_eve') %>% 
            # calculate the daily amplitude of stock
            mutate('win_eve' = map(win_eve, mutate, 
                                   "Amplitude" =  c(NaN, (Hiprc-Loprc)[-1] / Clsprc[-length(Clsprc)])
                                   )
                   )
    
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
                                         select(all_of(variable_name)))
                      )
    
    # merge window data with group identity
    PLSclus <- dir(path = file.path('~/OneDrive/Data.backup/QEAData/Matlab_PLS', year(Accprd[i])),
                   pattern = paste("group", file_char, sep = '_'),
                   recursive = TRUE, full.names = TRUE) %>% 
            read_csv(col_types = cols(Stkcd = col_character(),
                                      g_PLS = col_factor()))
    # the number of groups by PLS (su, 2016)
    grp_num <- levels(PLSclus$g_PLS) %>% length()
    # rename the group names rather than the integers
    if (grp_num == 2L) {
            grp_name <- c("Group A", "Group B")
    } else if (grp_num == 3L)  {
            grp_name <- c("Group A", "Group B", "Group C")
    } else if (grp_num == 4L) {
            grp_name <- c("Group A", "Group B", "Group C", "Group D")
    } else print("The group number is not included in this script!")
    # re-level the order of group levels
    PLSclus %<>% mutate('g_PLS' = factor(g_PLS, 
                                         levels = as.character(1:grp_num),
                                         labels = grp_name))
    win_stk %<>% inner_join(PLSclus, ., by = 'Stkcd')  # join procession
    
    # weather to take a subset sample to analysis at below script
    if (is.numeric(subsam)) {
            set.seed(subsam)
            stk_sam <- sample(pull(win_stk, Stkcd), size = subsam, replace = FALSE)
            win_stk %<>% filter(Stkcd %in% stk_sam)
    } else stk_sam <- pull(win_stk, Stkcd)  # Whole sample will be analysis
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
                   Accper %in% !!as.numeric(c((Accprd[i]+days(1))%m+%months(-3)+days(-1), Accprd[i]))
                   ) %>%
            # focus on parent company report
            filter(Typrep == "A") %>% select(-Typrep) %>%  
            collect() %>% 
            mutate('Accper' = as.Date(Accper, origin = "1970-01-01")) %>% 
            # join with information within quarterly earnings report
            right_join(filter(ReptInfo, Stkcd %in% stk_sam), by = c("Stkcd", "Accper"))
    
    # statistical properties -------------------------------------------------
    # plot the standard deviation of returns ====
    title_char <- paste0('The standard deviation of the average daily returns and ',
                         'the average abnormal returns of grouped stocks')
    caption_char <- paste('Accounting quarter,', 
                          (Accprd[i] + days(1)) %m+% months(-3), '~', Accprd[i],
                          sep = ' ')
    win_stk %>% 
    transmute(g_PLS, 'DR_AR' = map(win_eve, select, c('Timeline', 'Dret_rf', 'AbRet'))) %>% 
    unnest(cols = 'DR_AR') %>% 
    group_by(Timeline, g_PLS) %>% 
    summarise("Real" = sd(Dret_rf), "Abnormal" = sd(AbRet), .groups = "drop") %>% 
    gather(key = 'sd_type', value = 'sd_value', Real, Abnormal) %>% 
        ggplot(mapping = aes(x = as.integer(as.character(Timeline)), 
                             y = sd_value, colour = g_PLS)
               ) + 
            geom_path(aes(linetype = sd_type)) + 
            scale_color_brewer(palette = "Set1") +
            scale_x_continuous(breaks = seq(-(TS-1L)/2L, (TS-1L)/2L, by = 5)) + 
            labs(caption = caption_char, # title = title_char, 
                 y = "Standard deviation of returns", x = TeX("Time line ($\\tau$)"),
                 linetype = 'Return', color = 'Classification')
    
    ggsave(filename = paste(file_char, "DR_AR_gsd.pdf", sep = '_'), 
           width = 16, height = 9, scale = 0.60)
    
    # Part II, plot the path figure of returns adjusted by accounting factor ----
    # PE - VMG ====
    # cluster the stocks in our sample according to the difference of  
    # one accounting index from prior quarter to current quarter
    g_VMG <- cluster_acc(accounting = "F100601B", grp_col_name = 'group')
    title_char <- paste("The averaged daily returns of PE-portfolios within event window")
    # plot the adjustment effect of factor EPS (AMD)
    window_path(cluster = g_VMG)
    ggsave(filename = paste(file_char, "PE-Diff.pdf", sep = '_'), 
           width = 16, height = 12, scale = 0.60)
    # EPS - RMW ====
    g_RMW <- cluster_acc(accounting = "F090101B", grp_col_name = 'group')
    title_char <- sub(pattern = "PE", replacement = "EPS", x = title_char)
    window_path(cluster = g_RMW)
    ggsave(filename = paste(file_char, "EPS-Diff.pdf", sep = '_'),
           width = 16, height = 12, scale = 0.60)
    
    # Part III, Ordinary Least Squares with Robust Standard Errors ------------
    # calculate the values of explanation factors ====
    # trading date, t
    factor_t <- inner_join(by = 'TradingDate',
            calc_fct(class = 't', cluster = g_VMG, fct_name = "VMG_t"), 
            calc_fct(class = 't', cluster = g_RMW, fct_name = "RMW_t"))
    # time-line, tau 
    factor_tau <- inner_join(by = 'Timeline',
            calc_fct(class = 'tau', cluster = g_VMG, fct_name = 'VMG_tau'),
            calc_fct(class = 'tau', cluster = g_RMW, fct_name = 'RMW_tau'))
    
    # Time series ==== 
    # t
    mutate(win_stk, 'win_eve' = map(win_eve, left_join, factor_t, by = "TradingDate")) %>% 
    unnest(cols = 'win_eve') %$% 
    list(# run aggregate at once, but using Cluster-Robust variance
        lm_trdff(df = .,
                 data_structure = "ts", 
                 formula_lhs = 1, formula_rhs = 1,
                 clusters = Stkcd), 
        # using group as interaction term
        lm_trdff(df = .,
                 data_structure = "ts", 
                 formula_lhs = 1, formula_rhs = 2,
                 clusters = Stkcd)  
        ) %T>% 
    htmlreg(l = ., file = paste(file_char, 'TimeSeries_t.html', sep = '_'),
            caption = "OLS-regression of abnormal return (t) on factors",
            custom.model.names = c('Panel', 'Classification'),
            caption.above = TRUE, include.ci = FALSE, 
            dcolumn = TRUE, booktabs = TRUE, digits = 3,
            override.se = map(., 'statistic'), override.pvalues = map(., 'p.value')
            ) %$% 
    capture.output(
            texreg(l = ., caption = "OLS-regression of abnormal return (t) on factors",
                   custom.model.names = c('Panel', 'Classification'),
                   caption.above = TRUE, include.ci = FALSE, 
                   dcolumn = TRUE, booktabs = TRUE, digits = 3,
                   override.se = map(., 'statistic'), override.pvalues = map(., 'p.value')
                   ) %>% 
            gsub(pattern = "VMG\\_t", replacement = "VMG^{ts}_t", fixed = T) %>%
            gsub(pattern = "RMW\\_t", replacement = "RMM^{ts}_t", fixed = T) %>%
            gsub(pattern = "g\\_PLSgroup", replacement = "group", fixed = T),
            file = paste(file_char, 'TimeSeries_t.tex', sep = '_'), 
            append = FALSE, type = "output")
    ## tau (maybe useless to paper)
    mutate(win_stk, 'win_eve' = map(win_eve, left_join, factor_tau, by = "Timeline")) %>% 
    unnest(cols = 'win_eve') %$% 
    list(# run aggregate at once, using Cluster-Robust variance
        lm_trdff(df = ., 
                 data_structure = "ts", 
                 formula_lhs = 1, formula_rhs = 3,
                 clusters = Stkcd),  
        # using group as interaction term
        lm_trdff(df = ., 
                 data_structure = "ts",
                 formula_lhs = 1, formula_rhs = 4,
                 clusters = Stkcd),
        # calculate the AR_tau grouped by time-line and g_PLS
        transmute(win_stk, g_PLS,
                  'AR' = map(win_eve, ~ select(.x, all_of(c("Timeline", "AbRet"))))
                  ) %>% 
        unnest(cols = 'AR') %>% 
        group_by(g_PLS, Timeline) %>% 
        summarise("AR_tau" = mean(AbRet), .groups = "drop") %>% 
        inner_join(factor_tau, by = "Timeline") %>% 
        lm_trdff(data_structure = "ts", formula_lhs = 2, formula_rhs = 4)
        ) %T>% 
    htmlreg(l = ., file = paste(file_char, 'TimeSeries_tau.html', sep = '_'),
            caption = "OLS-regression of abnormal return (tau) on factors",
            custom.model.names = c("Panel", "Classificaton", "Summarise"),
            caption.above = TRUE, include.ci = FALSE, 
            dcolumn = TRUE, booktabs = TRUE, digits = 4,
            override.se = map(., 'statistic'), override.pvalues = map(., 'p.value')
            ) %$% 
    capture.output(
            texreg(., custom.model.names = c("Panel", "Classificaton", "Summarise"),
                   caption = "OLS-regression of abnormal return (tau) on factors",
                   caption.above = TRUE, include.ci = FALSE, 
                   dcolumn = TRUE, booktabs = TRUE, digits = 4,
                   override.se = map(., 'statistic'), override.pvalues = map(., 'p.value')
                   ) %>% 
            gsub(pattern = "VMG\\_tau", replacement = "VMG^{ts}_{\\tau}", fixed = T) %>%
            gsub(pattern = "RMW\\_tau", replacement = "RMM^{ts}_{\\tau}", fixed = T) %>%
            gsub(pattern = "g\\_PLSgroup", replacement = "group", fixed = T),
            file = paste(file_char, 'TimeSeries_tau.tex', sep = '_'), 
            append = FALSE, type = "output"
    )
    
    # cross-sectional ====
    # the core idea is we employ RMW_t to explain AR_t once at a same tau (cross-sectional) 
    # and we will focus on the difference of estimate coefficients among different tau and group
    win_stk %<>% 
        mutate('win_eve' = map(win_eve, left_join, factor_t, by = "TradingDate")) %>%
        unnest(cols = 'win_eve') %>% 
        # re-level the time line (take the tau = 0 as benchmark in regression)
        mutate('Timeline' = fct_relevel(Timeline, as.character(c(0, timeline[timeline != 0])))) %>% 
        dplyr::rename(tau = Timeline)
    # summary
    
    qtr_term <- c(-30:30)  # just running a part of event window
    # running regression - interact with tau
    reg_Accprd[[i]]$regression_tau <- win_stk %>% 
            filter(tau %in% as.character(qtr_term)) %>% 
            lm_trdff(data_structure = "cs", formula_lhs = 1, formula_rhs = 1)
    
    reg_Accprd[[i]]$regression_tau %T>% 
    htmlreg(file = paste(file_char, 'CrossSection_tau.html', sep = '_'),
            caption = "OLS-regression of cross-sectional abnormal return (t) on factors",
            custom.model.names = 'Dependent variable: abnormal return (t)',
            caption.above = TRUE, include.ci = FALSE, 
            dcolumn = TRUE, booktabs = TRUE, digits = 3,
            override.se = .$statistic, override.pvalues = .$p.value) %$% 
    capture.output(
        texreg(l = ., 
               caption = "OLS-regression of cross-sectional abnormal return (t) on factors",
               custom.model.names = 'Dependent variable: abnormal return (t)',
               caption.above = TRUE, include.ci = FALSE, 
               dcolumn = TRUE, booktabs = TRUE, digits = 3,
               override.se = .$statistic, override.pvalues = .$p.value
               ) %>% 
        gsub(pattern = "VMG\\_t", replacement = "VMG^{ts}_t", fixed = T) %>%
        gsub(pattern = "RMW\\_t", replacement = "RMM^{ts}_t", fixed = T) %>%
        gsub(pattern = "tau", replacement = "\\tau = ", fixed = T), 
        file = paste(file_char, 'CrossSection_tau.tex', sep = '_'), 
        append = FALSE, type = "output")
        
    # running regression - interact with tau and group
    qtr_term <- c(-5:5)
    reg_Accprd[[i]]$regression_tau_grp <- win_stk %>% 
            filter(tau %in% as.character(qtr_term)) %>% 
            lm_trdff(data_structure = "cs", formula_lhs = 1, formula_rhs = 2)
    
    # export html and tex file
    reg_Accprd[[i]]$regression_tau_grp %T>% 
    htmlreg(file = paste(file_char, 'CrossSection_tau_grp.html', sep = '_'),
            caption = "OLS-regression of cross-sectional abnormal return (t) on factors",
            custom.model.names = 'Dependent variable: abnormal return (t)',
            caption.above = TRUE, include.ci = FALSE, 
            dcolumn = TRUE, booktabs = TRUE, digits = 3,
            override.se = .$statistic, override.pvalues = .$p.value) %$% 
    capture.output(
        texreg(l = ., 
               caption = "OLS-regression of cross-sectional abnormal return (t) on factors",
               custom.model.names = 'Dependent variable: abnormal return (t)',
               caption.above = TRUE, include.ci = FALSE, 
               dcolumn = TRUE, booktabs = TRUE, digits = 3,
               override.se = .$statistic, override.pvalues = .$p.value
               ) %>% 
        gsub(pattern = "VMG\\_t", replacement = "VMG^{ts}_t", fixed = T) %>%
        gsub(pattern = "RMW\\_t", replacement = "RMM^{ts}_t", fixed = T) %>%
        gsub(pattern = "tau", replacement = "\\tau = ", fixed = T) %>%
        gsub(pattern = "g\\_PLSgroup", replacement = "Group", fixed = T), 
        file = paste(file_char, 'CrossSection_tau_grp.tex', sep = '_'), 
        append = FALSE, type = "output")
    # save
    reg_Accprd[[i]]$data <- win_stk        
    reg_Accprd[i] %>% save(file = paste(file_char, "data_reg.RData", sep = '_'))
    
    # Parameter visualization -------------------------------------------------
    qtr_term <- c(-15:10)  # a shorten event window
    # factor:tau ====
    filter(win_stk, tau %in% as.character(qtr_term)) %>%
    lm_trdff(data_structure = "cs", formula_lhs = 1, formula_rhs = 1) %>% 
    stats_plot(inter_type = 'tau',
               factor_name = c('VMG', 'RMW'))
    # factor:tau:g_PLS, diff-diff-diff ====
    filter(win_stk, tau %in% as.character(qtr_term)) %>%
    lm_trdff(data_structure = "cs", formula_lhs = 1, formula_rhs = 2) %>% 
    stats_plot(inter_type = 'grp',
               factor_name = c('VMG', 'RMW'))
}

dbDisconnect(QEA_db)

setwd(file.path('~/OneDrive/Data.backup/QEAData', model_type))
# save the model results of final explanation regression 
save(reg_Accprd, file = 'Data_Regression_by_quarter.RData')
map(reg_Accprd, 'regression') %>% save(file = 'Regression_Explain_AR_TG.RData')

# Part IV, running regression on data that combined multiple quarters ----
html_tex <- function(df, file_name, type, ...) {
  
    reg_gsub <- function(df) {
        df %>% 
        gsub(pattern = "tau", replacement = "tau=") %>% 
        gsub(pattern = "g\\_PLSgroup", replacement = "group", fixed = T) # %>% 
        # gsub(pattern = "Q1", replacement = "first quarter", fixed = T) %>%  
        # gsub(pattern = "Q2", replacement = "second quarter", fixed = T) %>%  
        # gsub(pattern = "Q3", replacement = "third quarter", fixed = T) %>%  
        # gsub(pattern = "Q4", replacement = "fourth quarter", fixed = T)
    }
  
    print(df %>% 
          screenreg(caption = "OLS-regression of cross-sectional abnormal return (t) on factors",
                  custom.model.names = 'Dependent variable: AR',
                  caption.above = TRUE, include.ci = FALSE, 
                  dcolumn = TRUE, booktabs = TRUE, digits = 3,
                  override.se = .$statistic, override.pvalues = .$p.value) %>% 
          gsub(pattern = "VMG_t", replacement = "VMG", fixed = T) %>%
          gsub(pattern = "RMW_t", replacement = "RMM", fixed = T) %>%
          reg_gsub()
          )
    
    if (type == 'html') {
      
        df %>% 
        htmlreg(file = paste0(file_name, '.html'),
                caption = "OLS-regression of cross-sectional abnormal return (t) on factors",
                custom.model.names = 'Dependent variable: AR',
                caption.above = TRUE, include.ci = FALSE, 
                dcolumn = TRUE, booktabs = TRUE, digits = 3,
                override.se = .$statistic, override.pvalues = .$p.value)
      
    } else if (type == 'tex') {
      
        df %$% 
        capture.output(
            texreg(l = ., 
                   caption = "OLS-regression of cross-sectional abnormal return (t) on factors",
                   custom.model.names = 'Dependent variable: AR',
                   caption.above = TRUE, include.ci = FALSE, 
                   dcolumn = TRUE, booktabs = TRUE, digits = 3,
                   override.se = .$statistic, override.pvalues = .$p.value
                   ) %>% 
            gsub(pattern = "VMG\\_t", replacement = "VMG^{ts}_t", fixed = T) %>%
            gsub(pattern = "RMW\\_t", replacement = "RMM^{ts}_t", fixed = T) %>%
            reg_gsub(), 
            file = paste0(file_name, '.tex'),
            append = FALSE, type = "output", ...
        )
    }
}

qtr_term <- c(-5:5)  # the window of tau 
# for view the regression result in R
reg_QT <- reg_Accprd %>% 
    map_dfr('data', .id = 'quarter') %>% 
    mutate('Q' = case_when(grepl('03-31', quarter) ~ 1, 
                           grepl('06-30', quarter) ~ 2, 
                           grepl('09-30', quarter) ~ 3,
                           grepl('12-31', quarter) ~ 4, 
                           TRUE ~ NA_real_
                           ) %>% 
                   factor(levels = c(1:4))
           ) %>% 
    filter(tau %in% as.character(qtr_term))

# output
# one intercept
lm_trdff(df = reg_QT, data_structure = "cs", formula_lhs = 1, formula_rhs = 3) %>% 
    html_tex(file_name = 'Regression_Explain_AR_QT', type = 'html') # type = 'tex'
# multiple intercepts
lm_trdff(df = reg_QT, data_structure = "cs", formula_lhs = 1, formula_rhs = 4) %>% 
    html_tex(file_name = 'Regression_Explain_AR_QT_alpha', type = 'html') # type = 'tex'
