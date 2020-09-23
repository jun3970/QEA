# This script is written to explain the abnormal returns around announced date
# we have two kind of explanation variables, trading and factor
# and we run two kind of regression, time series and cross-sectional

library(tidyverse)
library(lubridate)

library(modelr)
library(estimatr)
# library(sandwich)
# library(lmtest)

library(Formula)
library(magrittr)
library(rlang)
library(glue)

library(broom)
library(stargazer)
library(texreg)
library(multipanelfigure)
library(RColorBrewer)
library(ggthemes)
library(gt)

library(DBI)
library(RSQLite)
library(dbplyr)

# Specifying the basic attributes of data ----------------------------------
setwd('~/OneDrive/Data.backup/QEAData')

Accprd <- ymd('2017-09-30')
model_type <- "CH4"
Pretype <- 6L
Markettype <- 21L
subsam <- FALSE

# self-defined function ----------------------------------------------------
# cluster the stocks in our sample according to the difference value of  
# one accounting index from prior quarter to current quarter
cluster_acc <- function(x = ReptInfo, 
                        accounting,
                        grp_col_name,
                        diff_period = qtr_term, 
                        break_point = c(0, 0.3, 0.7, 1),
                        break_label = c("Descend", "Neutral", "Ascend")
                        ) {
        
    # Input:
    # accounting: which accounting index is used to structure a group variable?
    # grp_col_name: the name of portfolio 
    # diff_period: the date of two quarters to take difference
     
    select(x, c("Stkcd", "Accper", all_of(accounting))) %>% 
    spread(key = 'Accper', value = get(accounting)) %>% 
    mutate("diff" = `-`(!!sym(as.character(diff_period[2])),
                        !!sym(as.character(diff_period[1]))
                        )
           ) %>% 
    transmute(Stkcd,
              !!grp_col_name := cut(diff, 
                                    breaks = quantile(diff, 
                                                      break_point, 
                                                      na.rm = TRUE
                                                      ), 
                                    labels = break_label,
                                    ordered_result = TRUE, 
                                    include.lowest = TRUE,
                                    )
              )
    
}

# function to calculate the value of factor (t) 
calc_fct_t <- function(x, fct_name) {
    
    map_dfr(trddat, select, c('Stkcd', 'TradingDate', 'Dretnd', 'Dsmvosd')) %>% 
    left_join(PLSclus, by = "Stkcd") %>%  # PLS
    left_join(x, by = "Stkcd") %>%  # AMD portfolio
    group_by(TradingDate, g_PLS, group) %>%  # t
    # imitating the structure process of Fama-French factors,
    # we group and take intersection with stocks by g_PLS and g.RMW,
    # and to calculate the weighted portfolio daily returns
    summarise("ptf_Ret" = Dretnd %*% (Dsmvosd /sum(Dsmvosd)),
              .groups = "drop"
              ) %>% 
    spread(key = 'group', value = 'ptf_Ret') %>% 
    # calculate the mean of portfolio returns (g_PLS)
    group_by(TradingDate) %>% 
    summarise_if(.predicate = is.numeric, .funs = mean) %>% 
    # take the minus of the g_PLS mean returns between the portfolios g.RMW
    transmute(TradingDate, !!fct_name := Ascend - Descend)

}

# function to calculate the value of factor (tau)
calc_fct_tau <- function(x, fct_name) {
    
        inner_join(eve_tbl, x, by = "Stkcd") %>% 
        group_by(group, Timeline) %>% 
        summarise("avgDret" = Dretnd %*% (Dsmvosd / sum(Dsmvosd)), 
                  .groups = "drop"
                  ) %>% 
        spread(key = 'group', value = 'avgDret') %>% 
        transmute(Timeline, !!fct_name := Ascend - Descend)

}

# regressions in three aspects
# 1. trading
# 2. factor_tau
# 3. factor_t
lm_trdff <- function(df, ...,
                     data_structure,  
                     SE_type = "stata", 
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
    
    if (data_structure == "ts") {  # running all data at once 
        
            lm_formula <- Formula(
                    AbRet | AR_tau ~  # using trading data as explanation variables 
                                      Amplitude + Turnover + Liquidility + Dnshrtrd
                                      # take the group information as the interaction term
                                      | (Amplitude + Turnover + Liquidility + Dnshrtrd) * g_PLS
                                      # factor, take the calendar date as the time line
                                      | VMG_t + RMW_t
                                      | (VMG_t + RMW_t) * g_PLS
                                      # factor, take the tau as the time line
                                      | VMG_tau + RMW_tau  
                                      | (VMG_tau + RMW_tau) * g_PLS
                    )
        
    } else if (data_structure == "cs") {  # cross-sectional regression 
        
            # note that class factors and trading data of stocks are combined
            lm_formula <- Formula(
                    AbRet ~ (VMG_t + RMW_t) * g_PLS + Turnover + Liquidility + Dnshrtrd 
                            # note that when we using tau as interaction term 
                            # we must running the panel data in regression
                            | (VMG_t + RMW_t) * Timeline + Turnover + Liquidility + Dnshrtrd
                            | (VMG_t + RMW_t) + VMG_t:Timeline + RMW_t:Timeline + 
                                  VMG_t:Timeline:g_PLS + RMW_t:Timeline:g_PLS + 
                                  Turnover + Liquidility + Dnshrtrd
                    )
            
    } else stop("Please select a model correctly!")
    
    lm_result <- lm_robust(data = df, ...,
                           se_type = SE_type, 
                           formula = formula(lm_formula, lhs = formula_lhs, rhs = formula_rhs)
                           )

}

# generate a summary table of our variable (feature) by package gt
gt_tbl_sum <- function(x = eve_tbl_summary, statistic, group_var = NULL) {
    
    gt_title <- glue("The {statistic} value of explanation variables.")
    
    gt_subtitle <- glue("At accounting quarter {(Accprd+days(1))%m+%months(-3)} to {Accprd}")
                   
    select(x, c("Timeline", "g_PLS") | ends_with(statistic)) %>% 
    group_by_at(group_var) %>% 
        gt::gt() %>% 
        tab_header(title = gt_title, subtitle = gt_subtitle) %>% 
        fmt_number(columns = names(x)[str_ends(names(x), pattern = statistic)],
                   decimals = 5, suffixing = TRUE
                   ) %>% 
        cols_label()
    
}

# function to look the path of average returns within event window
# under the adjust effect of a accounting indicator
window_path <- function(x = eve_tbl, y, ...) {
    
    inner_join(x, y, by = "Stkcd") %>% 
    group_by_at(.vars = c('group', 'Timeline')) %>% 
    summarise("avgDret" = mean(Dretnd), .groups = "drop") %>% 
        ggplot(data = filter(., group != "Neutral"), 
               mapping = aes(x = as.integer(as.character(Timeline)), 
                             y = avgDret
                             )
               ) +
            geom_path(aes(linetype = group)) +
                labs(x = "Time line",
                     y = "Average daily return", 
                     title = title_char, 
                     caption = caption_char
                     ) +
                scale_x_continuous(breaks = seq(-(TS-1L)/2L, (TS-1L)/2L, by = 5)) +
                theme_economist() +
                theme(legend.title = element_blank(), 
                      axis.title = element_text(margin = margin(r = 10, t = 10)),
                      ...
                      ) +
            geom_ref_line(h = 0, colour = "#999999") +
            geom_rect(data = rect_index, 
                      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
                      alpha = 0.7, show.legend = FALSE, inherit.aes = FALSE
                      )

}

# function to plot the estimate value with graph of path and point
stats_plot <- function(df, 
                       str_grep, 
                       stats_name = c("estimate", "statistic"),
                       inter_type,
                       axis_x_title
                       ) {

    df_est <- df[grep(pattern = str_grep, x = df$term), ]
    
    if (inter_type == "tau") { 
        
        df_est %<>% mutate("tau" = timeline[timeline != median(timeline)]) 
        
    } else if (inter_type == "grp") { # compare between with groups 
        
        df_est %<>% mutate("tau" = c(median(timeline), 
                                     timeline[timeline != median(timeline)]
                                     )
                           )
        
    }
    
    figure_esti <- ggplot(df_est, aes(x = tau, y = get(stats_name[1]))) +
            geom_point() +
            labs(y = stats_name[1], x = axis_x_title,
                 title = 'The estimate coefficients of explanation variables'
                 ) +
            scale_x_continuous(breaks = seq(-(TS-1L)/2L, (TS-1L)/2L, by = 5)) + 
            geom_ref_line(h = 0, colour = "grey") +
            theme_bw()
    
    figure_stat <- ggplot(df_est, aes(x = tau, y = get(stats_name[2]))) +
            geom_point() +
            labs(y = stats_name[2], x = axis_x_title,
                 title = 'The t-value of estimeate coefficients'
                 ) +
            scale_x_continuous(breaks = seq(-(TS-1L)/2L, (TS-1L)/2L, by = 5)) + 
            geom_ref_line(h = +1.96, colour = "grey") +
            geom_ref_line(h = -1.96, colour = "grey") +
            theme_bw()
    
    return(list("estimate" = figure_esti, "statistic" = figure_stat))

}
    
# generate the tex code or table at console of existing model (regression)
mod_texreg <- function(df, tex = TRUE, ...) {
    
    if (tex == FALSE) {  # display the table on the console
        
        screenreg(df, ...,
                  caption.above = TRUE, dcolumn = TRUE, booktabs = TRUE, 
                  include.ci = FALSE, digits = 3, 
                  override.se = map(df, "statistic"),
                  
                  ) %>%
        gsub("g_PLS2", "group two", ., fixed = TRUE) %>%
        gsub("g_PLS3", "group three", ., fixed = TRUE)
        
    } else {  # print the tex code of our table
        
        texreg(df, ...,
               caption.above = TRUE, dcolumn = TRUE, booktabs = TRUE, 
               include.ci = FALSE, digits = 3, 
               override.se = map(df, "statistic"),
               
               ) %>%
        gsub("g\\_PLS2","group two", ., fixed = TRUE) %>%
        gsub("g\\_PLS3","group three", ., fixed = TRUE)
        
    }
    
}

# Part I, read and tidy data -----------------------------------------------
setwd(file.path(model_type, year(Accprd), Accprd))

# PLS result
PLSclus <- dir(path = file.path('~/OneDrive/Data.backup/QEAData',
                                'Matlab_PLS', 
                                year(Accprd)
                                ),
               pattern = paste("group", Accprd, Pretype, Markettype, 
                               model_type,
                               sep = '_'
                               ),
               recursive = TRUE,
               full.names = TRUE
               ) %>% 
        read_csv(file = .,
                 col_types = cols(Stkcd = col_character(),
                                  g_PLS = col_factor()
                                  )
                 )
# the number of groups by PLS (su, 2016)
grp_num <- levels(PLSclus$g_PLS) %>% length()
# re-level the order of group levels
PLSclus$g_PLS %<>% fct_relevel(as.character(1:grp_num))
# rename the group names rather than the integers
if (grp_num == 2L) {
        grp_name <- c("group one", "group two")
} else if (grp_num == 3L)  {
        grp_name <- c("group one", "group two", "group three")
} else if (grp_num == 4L) {
        grp_name <- c("group one", "group two", "group three", "group four")
} else print("The group number is not included in this script!")

# Import the data of daily trading and factors within event window ====
stkeve <- dir(pattern = paste(Accprd, Pretype, Markettype, model_type, 
                              "stkeve", 
                              sep = '_'
                              ),
              full.names = TRUE
              ) %>%
        read_csv(na = '',
                 col_types = cols(
                         Stkcd = col_character(),
                         Markettype = col_factor(levels = c(1,4,16)),
                         Indus = col_factor(),  # industry category
                         Annowk = col_factor()  # the day of week
                         )
                 ) %>% 
        split(.$Stkcd)
# weather to take a subset sample to analysis at below script
if (is.numeric(subsam)) {  # take a subset sample of stocks pool
   
        set.seed(subsam)
        stk_sam <- sample(names(stkeve), size = subsam, replace = FALSE)
        stkeve %<>% `[`(stk_sam)
    
} else if (`==`(subsam, FALSE)) {  # analysis whole sample 
    
        stk_sam <- names(stkeve)
        print("The whole sample will be analysis.")

}
# the number of stocks in our sample this quarter
N <- length(stkeve)
# the length of estimate window
if(`==`(length(unique(map_int(stkeve, nrow))), 1)) {
    
        TS <- unique(map_int(stkeve, nrow))
    
} else stop("The time series number of window trading data are not same.")
# the time-line of event window at the level of tau
timeline <- seq(-(TS - 1L) / 2, (TS - 1L) / 2, by = 1)
# should we abandon a part of window for the beauty of CAR path?
# if you want, just change the values of TS manually

# Import the original daily trading data ====
# link to SQLite database
QEA_db <- dbConnect(RSQLite::SQLite(), "~/OneDrive/Data.backup/QEAData/QEA_db.sqlite")
# to speed up the procession, take a restrict condition on trading date
qtr_term <- seq(from = as.numeric(Accprd %m+% months(-9)), 
                to = as.numeric(Accprd %m+% months(+9)), 
                by = 1
                )
# Import the daily trading data of stocks in quarterly sample
trddat <- tbl(QEA_db, "daily") %>% 
        filter(Stkcd %in% !!stk_sam,
               TradingDate %in% !!qtr_term
               ) %>%
        collect()
# calculate the daily amplitude of stock
trddat %<>% 
        split(x = ., f = .$Stkcd) %>%  # split the data frame by stock
        lapply(mutate, 
               "Amplitude" = c(NaN, 
                               `/`((Hiprc - Loprc)[-1], Clsprc[-length(Clsprc)])
                               ),
               # transform the date format and re-level the market type 
               'TradingDate' = as.Date(TradingDate, origin = "1970-01-01"),
               'Markettype' = factor(Markettype, levels = as.character(c(1, 4, 16)))
               )

# Import quarterly accounting data and join to quarterly earnings report ====
qtr_term <- c((Accprd + days(1)) %m+% months(-3) + days(-1), Accprd)
Acc_ind <- tbl(QEA_db, "quarter") %>% 
        # subset the stocks in our quarterly sample
        filter(Stkcd %in% stk_sam,
               Accper %in% !!as.numeric(qtr_term)
               ) %>%
        # focus on parent company report
        filter(Typrep == "A") %>% select(-Typrep) %>%  
        collect()
# join accounting indicators to quarterly earnings report
load(file = "~/OneDrive/Data.backup/QEAData/ReportInfo.RData"); rm(PreRept)
ReptInfo %<>% right_join(mutate(Acc_ind, 'Accper' = as.Date(Accper, origin = "1970-01-01")),
                         by = c("Stkcd", "Accper")
                         )

dbDisconnect(QEA_db)

# join the data of stkeve, trddat, and grouped AR ====
# import the data of abnormal returns calculated in script '03.CalCAR.R'
QEA_gAR <- read_csv(file = paste(Accprd, Pretype, Markettype, model_type, grp_num, 
                                 "gAR.csv", sep = '_'
                                 ),
                    col_types = cols(
                            Stkcd = col_character(),
                            TradingDate = col_date("%Y-%m-%d"),
                            # PLS - cluster result
                            g_PLS = col_factor(levels = c(1:grp_num)),
                            # abnormal return
                            AbRet = col_double()
                            )
                    ) %>% 
        # add the time-line index to AR of every stock
        split(x = ., f = .$Stkcd) %>% 
        map(~ arrange(.x, TradingDate) %>% 
              add_column("Timeline" = factor(timeline, ordered = TRUE))
            )

# we place two if-else statement at there to avoid the situation that 
# merge process produce NULL list (stock order are not matching)
if (!all.equal(names(stkeve), names(trddat))) {
    
        stop("Please correct the order of stocks in lists `stkeve` and `trddat`.")
    
} else if (!all.equal(names(stkeve), names(QEA_gAR))) {
    
        stop("Please correct the order of stocks in lists `stkeve` and `QEA_gAR`.")
    
} else {  # join above three tables using function `map2`

        eve_tbl <- map2(stkeve, trddat, 
                        left_join, 
                        by = c("Stkcd", "TradingDate", "Dretnd", "Dsmvosd", "Markettype")
                        ) %>% 
                map(~ arrange(.x, TradingDate) %>% 
                      add_column("Timeline" = factor(timeline, ordered = TRUE))
                    )
        
        eve_tbl <- map2(eve_tbl, QEA_gAR[stk_sam], 
                        left_join, 
                        by = c("Stkcd", "TradingDate", "Timeline")
                        ) 
            
        if (!all.equal(rep(TS, N), unname(sapply(eve_tbl, nrow)))) {
            
            extra_stk <- names(eve_tbl)[`!=`(sapply(eve_tbl, nrow), TS)]
            
            stop(glue("{extra_stk}, the length of event window not equal to {TS}."))
            
        } else {  # select explanation variables and transform the lists data to a single tibble
                        
            eve_tbl %<>% map_dfr(select,  
                                 c("Stkcd", "g_PLS", "TradingDate", "Timeline",
                                   "Dretnd", "AbRet", "Dret_rf", "mkt_rf", "SMB", "VMG",
                                   "Dnshrtrd", "Dnvaltrd", "Turnover", "Liquidility", 
                                   "Amplitude", "Dsmvtll", "Dsmvosd","PE", "PB", "PS"
                                   )
                                 )   
                                    
        }
}

if (# detect NULL value for calculation accuracy
    
    all.equal(rowSums(is.na.data.frame(select(eve_tbl, -c("PE", "PB", "PS")))), 
              rep(0L, nrow(eve_tbl))
              ) 
    
    ) { # calculate the AR_tau grouped by time-line and g_PLS
    
        AR_tau <- summarise(group_by(eve_tbl, g_PLS, Timeline),
                            "AR_tau" = AbRet %*% (Dsmvosd / sum(Dsmvosd)), 
                            .groups = "drop"
                            )
        
        eve_tbl %<>% left_join(AR_tau, by = c("Timeline", "g_PLS"))
        
        } else stop("joinging produce too many NA value!")
        

# statistical properties -------------------------------------------------

# make a summary of the mean and variance of variables' value ====
eve_tbl_summary <- select(eve_tbl, -AR_tau) %>%
        group_by(Timeline, g_PLS) %>%
        summarise_if(.predicate = is.numeric,
                     .funs = list(~ mean(., na.rm = TRUE),
                                  ~ var(., na.rm = TRUE)
                                  )
                     )
# take a look at the mean value of the value of variables
gt_tbl_sum(eve_tbl_summary, statistic = "mean", group_var = 'g_PLS')
# take a look at the variance of the value of variables
gt_tbl_sum(eve_tbl_summary, statistic = "var", group_var = 'g_PLS')

# plot the standard deviation of returns ====
title_char <- paste('The standard deviation of the weighted daily returns and',
                    'the weighted abnormal returns of stocks \nwithin event window',
                    glue('{(Accprd + days(1)) %m+% months(-3)} to {Accprd}'),
                    sep = " "
                    )

group_by(eve_tbl, Timeline, g_PLS) %>% 
    summarise("real return" = sd(Dret_rf * (Dsmvosd/sum(Dsmvosd))),
              "abnormal return" = sd(AbRet * (Dsmvosd/sum(Dsmvosd))),
              .groups = "drop"
              ) %>% 
    gather(key = 'sd_type', value = 'sd_value', 
           "real return", "abnormal return"
           ) %>% 
    ggplot(data = .,
           mapping = aes(x = as.integer(as.character(Timeline)), 
                         y = sd_value, 
                         colour = g_PLS
                         )
           ) + 
        geom_path(aes(linetype = sd_type)) + 
        geom_point() +
        scale_x_continuous(breaks = seq(-(TS-1L)/2L, (TS-1L)/2L, by = 5)) + 
        scale_color_manual(labels = grp_name, 
                           values = brewer.pal(grp_num, "Set1")
                           ) +
        labs(x = "Time line", 
             y = "Standard deviation of returns", 
             title = title_char
             ) + 
        theme_economist() +
        theme(axis.ticks = element_blank(),
              axis.title = element_text(margin = margin(r = 10, t = 10)),
              legend.title = element_blank()
              )

ggsave(filename = paste(figure_term, "Dret_sd.pdf", sep = '_'), 
       width = 16, height = 9, 
       scale = 0.8
       )


# Part II, Calculate the explanation factor --------------------------------

# first plot the adjustment effect of someone accounting indicator ====
figure_term <- paste(Accprd, Pretype, Markettype, model_type, sep = '_')

# PE - VMG 
# cluster the stocks in our sample according to the difference of  
# one accounting index from prior quarter to current quarter
g_VMG <- cluster_acc(accounting = "F100601B", grp_col_name = 'group')

title_char <- paste("The average daily returns within event window of portfolios",
                    "\nwhich are structured by the difference of PE among two quarters",
                    sep = " "
                    )
caption_char <- glue("{(Accprd + days(1)) %m+% months(-3)} to {Accprd}")

# the index of the rectangle shadow in plot
rect_index <- tibble::tribble(
        ~tier, ~xmin, ~xmax, ~ymin, ~ymax,
        1, -Inf,  -14,  -Inf,  +Inf,
        2,  +21,  Inf,  -Inf,  +Inf,
        )

# plot the adjustment effect of factor EPS (AMD)
window_path(eve_tbl, g_VMG)
ggsave(filename = paste(figure_term, "PE-Diff.pdf", sep = '_'), 
       width = 16, height = 9, 
       scale = 0.6
       )

# EPS - RMW 
g_RMW <- cluster_acc(accounting = "F090101B", grp_col_name = 'group')
title_char <- sub(pattern = "PE", replacement = "EPS", x = title_char)
window_path(x = eve_tbl, y = g_RMW)
ggsave(filename = paste(figure_term, "EPS-Diff.pdf", sep = '_'),
       width = 16, height = 9, 
       scale = 0.6
       )

# then calculate the values of explanation factors ====

# trading date, t
VMG_t <- calc_fct_t(x = g_VMG, fct_name = "VMG_t")
RMW_t <- calc_fct_t(x = g_RMW, fct_name = "RMW_t")

rm(trddat); gc()

# time-line, tau 
VMG_tau <- calc_fct_tau(g_VMG, fct_name = 'VMG_tau')
RMW_tau <- calc_fct_tau(g_RMW, fct_name = 'RMW_tau')

# merge the data of factor AMD with aggregate regression data
eve_tbl %<>% left_join(RMW_t, by = "TradingDate") %>% 
        left_join(VMG_t, by = "TradingDate") %>% 
        left_join(RMW_tau, by = "Timeline") %>% 
        left_join(VMG_tau, by = "Timeline") 


# Part III, Ordinary Least Squares with Robust Standard Errors ------------

# Time series ==== 

eve_clst_grp <- eve_tbl %>% 
        group_nest(g_PLS) %>%
        mutate("ts_trd" = map(data, lm_trdff, data_structure = "ts", 
                              formula_lhs = 1, formula_rhs = 1  # trading
                              ),  
               "ts_ff_t" = map(data, lm_trdff, data_structure = "ts", 
                               formula_lhs = 1, formula_rhs = 3  # factor_t
                               ),  
               # "ts_trd_resids" = map2(data, ts_trd, add_residuals) 
               )

# generate the tables of regression results

    ## trading explanation
    list(lm_trdff(df = eve_tbl, 
                  data_structure = "ts", 
                  formula_lhs = 1, formula_rhs = 1,
                  clusters = g_PLS  
                  ), # run aggregate at once, Cluster-Robust variance
         eve_clst_grp$ts_trd,  # separate by group
         lm_trdff(eve_tbl, 
                  data_structure = "ts", 
                  formula_lhs = 1, formula_rhs = 2
                  )  # add group interaction
         ) %>% 
    flatten() %>% 
    mod_texreg(tex = TRUE, 
               custom.model.names = c("Unclassified", grp_name, "inter-group"),
               # custom.header = list("Abnormal Return (t)" = 1:5),
               # reorder.coef = c(1, 6, 7, 2:5, seq(8, 15, 2), seq(9, 15, 2))
               ) %>% 
    cat()
    
    ## factor explanation - t
    list(lm_trdff(eve_tbl, 
                  data_structure = "ts", 
                  formula_lhs = 1, formula_rhs = 3,
                  clusters = g_PLS
                  ),  
         eve_clst_grp$ts_ff_t,
         lm_trdff(eve_tbl, 
                  data_structure = "ts",
                  formula_lhs = 1, formula_rhs = 4
                  )
         ) %>% 
    flatten() %>% 
    mod_texreg(tex = TRUE, 
               # custom.header = list("Abnormal Return (t)" = 1:5),
               custom.model.names = c("Unclassified", grp_name, "inter-group")
               ) %>% 
    cat()
    
    ## factor explanation - tau
    AR_RMW_tau <- inner_join(AR_tau, RMW_tau, by = "Timeline") %>% 
            inner_join(VMG_tau, by = "Timeline")
    
    mutate(group_nest(AR_RMW_tau, g_PLS),
           "ts_ff_tau" = map(data, 
                             lm_trdff, 
                             data_structure = "ts", 
                             formula_lhs = 2, formula_rhs = 5
                             )
           ) %>%
    pull("ts_ff_tau") %>% 
    list(., 
         lm_trdff(AR_RMW_tau, 
                  data_structure = "ts",
                  formula_lhs = 2, formula_rhs = 6
                  )
         ) %>% 
    flatten() %>% 
    mod_texreg(tex = TRUE, 
               custom.model.names = c(grp_name, "inter-group"),
               # custom.header = list("Abnormal Return (tau)" = 1:4),
               ) %>% 
    cat()

    ## stock dimension - trading
    group_nest(eve_tbl, g_PLS, Stkcd) %>% 
        sample_frac(0.01) %>% 
        transmute(g_PLS, Stkcd,
                  "ts_trd_stk" = map(data, 
                                     ~ lm_trdff(.x, 
                                                data_structure = 'ts',
                                                formula_lhs = 1, formula_rhs = 1
                                                ) %>% 
                                       broom::tidy()
                                     )
                  ) %>% 
        unnest(cols = ts_trd_stk) %>% 
        # as.data.frame() %>% stargazer(summary = FALSE, rownames = FALSE)
        gt::gt(group_by(., g_PLS, Stkcd)) %>% 
            tab_header(title = "Estimates of each coefficient in model (trading)",
                       subtitle = glue("{(Accprd+days(1))%m+%months(-3)} to {Accprd}")
                       ) %>% 
            fmt_number(columns = c("estimate", "std.error", "statistic", "p.value", 
                                   'conf.low', 'conf.high'
                                   ),
                       decimals = 4,
                       suffixing = TRUE
                       )


# cross-sectional ====
    
# the core idea is we employ RMW_t to explain AR_t at a same tau (cross-sectional) 
# Note that we will focus on the difference of estimate coefficients among different tau

# re-level the time line (take the tau = 0 as benchmark in regression) 
eve_tbl$Timeline %<>% fct_relevel(as.character(c(0, timeline[timeline != 0])))

# RMW_t:g_PLS, we take the PLS cluster result g_PLS as interaction term ====
eve_clst_tau <- eve_tbl %>% 
        group_nest(Timeline) %>% 
        mutate("cs_grp" = map(data, 
                              .f = lm_trdff, 
                              data_structure = "cs", 
                              formula_lhs = 1, formula_rhs = 1
                              )
               )

# select a part of time line for tidiness, too may time periods to display at one page 
qtr_term <- as.character(c(-3:3))

eve_clst_tex <- list(subset(eve_clst_tau, drop = TRUE,
                            Timeline %in% qtr_term, 
                            select = 'cs_grp'
                            ),
                    # in contrast with using panel data but employ group-robust-variance
                    lm_trdff(filter(eve_tbl, Timeline %in% qtr_term), 
                             data_structure = "cs",
                             clusters = g_PLS,
                             # time-line tau as interaction term
                             formula_lhs = 1, formula_rhs = 2
                             )
                    ) %>% 
        flatten() %>% 
        mod_texreg(tex = TRUE,
                   no.margin = TRUE, scalebox = 0.8, # sideways = TRUE,
                   custom.header = list("Abnormal Return (t)" = 1:length(.)),
                   custom.model.names = c(str_c("tau = ", qtr_term), "inter-timeline")
                   )

for (i in seq_len(length(qtr_term)-1)) {
    eve_clst_tex %<>% 
            gsub(pattern = c("Timeline\\.L", "Timeline\\.Q", "Timeline\\.C", 
                             "Timeline[[:punct:]]+4[[:punct:]]", 
                             "Timeline[[:punct:]]+5[[:punct:]]", 
                             "Timeline[[:punct:]]6"
                             )[i],
                 replacement = str_c("tau = ", qtr_term[qtr_term != median(qtr_term)])[i]
                 )
}

cat(eve_clst_tex)


# RMW_t:tau, we take the time-line tau as interaction term ====
eve_tbl_cs <- lm_trdff(df = eve_tbl,
                       data_structure = "cs",
                       formula_lhs = 1, formula_rhs = 2,
                       clusters = g_PLS
                       ) %>% 
        broom::tidy()

# intercept
cs_inter <- stats_plot(df = eve_tbl_cs, 
                       inter_type = 'tau',
                       str_grep = "^Timeline([[:punct:]]\\d+|\\.[LQC])",
                       axis_x_title = 'The intercept of regression'
                       )

multi_panel_figure(width = 160, height = 180, columns = 1, rows = 2) %>% 
    fill_panel(cs_inter[[1]], column = 1, row = 1) %>% 
    fill_panel(cs_inter[[2]], column = 1, row = 2) %>% 
        save_multi_panel_figure(filename = glue("{Accprd}_intercept_path.pdf"))

term_str <- '[[:punct:]]t[[:punct:]]Timeline([[:punct:]]\\d+|\\.[LQC])'

# the difference value of estimate coefficient of factor RMW_t among tau and themselves t-value 
cs_VMG <- stats_plot(df = eve_tbl_cs,
                     inter_type = 'tau',
                     str_grep = paste0('^VMG', term_str),
                     axis_x_title = 'The term of factor VMG_t interact with tau'
                     )

cs_RMW <- stats_plot(df = eve_tbl_cs,
                     inter_type = 'tau',
                     str_grep = paste0('^RMW', term_str),
                     axis_x_title = 'The term of factor RMW_t interact with tau'
                     )

multi_panel_figure(width = 160, height = 180, columns = 1, rows = 2) %>% 
    fill_panel(cs_VMG[[1]], column = 1, row = 1) %>% 
    fill_panel(cs_RMW[[1]], column = 1, row = 2) %>% 
        save_multi_panel_figure(filename = glue("{Accprd}_estimator_tau.pdf"))

multi_panel_figure(width = 160, height = 180, columns = 1, rows = 2) %>% 
    fill_panel(cs_VMG[[2]], column = 1, row = 1) %>% 
    fill_panel(cs_RMW[[2]], column = 1, row = 2) %>% 
        save_multi_panel_figure(filename = glue("{Accprd}_statistic_tau.pdf"))

# RMW_t:tau:g_PLS, we take the time-line tau and g_PLS as twin-interaction terms ====
cs_gt <- lm_trdff(df = eve_tbl, 
                  data_structure = "cs",
                  formula_lhs = 1, formula_rhs = 3
                  ) %>% 
        broom::tidy()

# the difference value of estimate coefficient of factors among tau 
cs_gt_VMG_timeline <- stats_plot(
        df = cs_gt,
        inter_type = 'tau',
        str_grep = paste0('^VMG', term_str, '$'),
        axis_x_title = 'The term of factor VMG_t interacte with tau and g_PLS'
        )

cs_gt_RMW_timeline <- stats_plot(
        df = cs_gt, 
        inter_type = 'tau',
        str_grep = paste0('^RMW', term_str, '$'),
        axis_x_title = 'The term of factor RMW_t interacte with tau and g_PLS'
        )

multi_panel_figure(width = 160, height = 180, columns = 1, rows = 2) %>% 
    fill_panel(cs_gt_VMG_timeline[[1]], column = 1, row = 1) %>% 
    fill_panel(cs_gt_RMW_timeline[[1]], column = 1, row = 2) %>% 
        save_multi_panel_figure(filename = glue("{Accprd}_estimator_tau_grp.pdf"))

multi_panel_figure(width = 160, height = 180, columns = 1, rows = 2) %>% 
    fill_panel(cs_gt_VMG_timeline[[2]], column = 1, row = 1) %>% 
    fill_panel(cs_gt_RMW_timeline[[2]], column = 1, row = 2) %>% 
        save_multi_panel_figure(filename = glue("{Accprd}_statistic_tau_grp.pdf"))

# the difference value of estimate coefficient of factors among groups
term_str <- "[[:punct:]]t[[:punct:]]Timeline[[:punct:]]?\\d+[[:punct:]]g[[:punct:]]"

if (grp_num == 2L) {
    
    cs_gt_VMG_group2 <- stats_plot(
            df = cs_gt,
            inter_type = 'grp',
            str_grep = paste0('^VMG', term_str, 'PLS2$'),
            axis_x_title = 'The term of factor VMG_t interacte with tau and group 2'
            )

    cs_gt_RMW_group2 <- stats_plot(
            df = cs_gt,
            inter_type = 'grp',
            str_grep = paste0('^RMW', term_str, 'PLS2$'),
            axis_x_title = 'The term of factor RMW_t interacte with tau and group 2'
            )
    
    multi_panel_figure(width = 160, height = 180, columns = 1, rows = 2) %>% 
        fill_panel(cs_gt_VMG_group2[[1]], column = 1, row = 1) %>% 
        fill_panel(cs_gt_RMW_group2[[1]], column = 1, row = 2) %>% 
            save_multi_panel_figure(filename = glue("{Accprd}_estimator_tau_grp_g2.pdf"))
    
    multi_panel_figure(width = 160, height = 180, columns = 1, rows = 2) %>% 
        fill_panel(cs_gt_VMG_group2[[2]], column = 1, row = 1) %>% 
        fill_panel(cs_gt_RMW_group2[[2]], column = 1, row = 2) %>% 
            save_multi_panel_figure(filename = glue("{Accprd}_statistic_tau_grp_g2.pdf"))
    
}

if (grp_num == 3L) {
    
    cs_gt_VMG_group2 <- stats_plot(
            df = cs_gt,
            inter_type = 'grp',
            str_grep = paste0('^VMG', term_str, 'PLS2$'),
            axis_x_title = 'The term of factor VMG_t interacte with tau and group 2'
            )

    cs_gt_VMG_group3 <- stats_plot(
            df = cs_gt,
            inter_type = 'grp',
            str_grep = paste0('^VMG', term_str, 'PLS3$'),
            axis_x_title = 'The term of factor VMG_t interacte with tau and group 3'
            )
    
    multi_panel_figure(width = 160, height = 180, columns = 1, rows = 2) %>% 
        fill_panel(cs_gt_VMG_group2[[1]], column = 1, row = 1) %>% 
        fill_panel(cs_gt_VMG_group3[[1]], column = 1, row = 2) %>% 
            save_multi_panel_figure(filename = glue("{Accprd}_estimator_tau_grp_VMG.pdf"))
    
    multi_panel_figure(width = 160, height = 180, columns = 1, rows = 2) %>% 
        fill_panel(cs_gt_VMG_group2[[2]], column = 1, row = 1) %>% 
        fill_panel(cs_gt_VMG_group3[[2]], column = 1, row = 2) %>% 
            save_multi_panel_figure(filename = glue("{Accprd}_statistic_tau_grp_VMG.pdf"))
    
    cs_gt_RMW_group2 <- stats_plot(
            df = cs_gt,
            inter_type = 'grp',
            str_grep = paste0('^RMW', term_str, 'PLS2$'),
            axis_x_title = 'The term of factor RMW_t interacte with tau and group 2'
            )
    
    cs_gt_RMW_group3 <- stats_plot(
            df = cs_gt,
            inter_type = 'grp',
            str_grep = paste0('^RMW', term_str, 'PLS3$'),
            axis_x_title = 'The term of factor RMW_t interacte with tau and group 2'
            )
    
    multi_panel_figure(width = 160, height = 180, columns = 1, rows = 2) %>% 
        fill_panel(cs_gt_RMW_group2[[1]], column = 1, row = 1) %>% 
        fill_panel(cs_gt_RMW_group3[[1]], column = 1, row = 2) %>% 
            save_multi_panel_figure(filename = glue("{Accprd}_estimator_tau_grp_RMW.pdf"))
    
    multi_panel_figure(width = 160, height = 180, columns = 1, rows = 2) %>% 
        fill_panel(cs_gt_RMW_group2[[2]], column = 1, row = 1) %>% 
        fill_panel(cs_gt_RMW_group3[[2]], column = 1, row = 2) %>% 
            save_multi_panel_figure(filename = glue("{Accprd}_statistic_tau_grp_RMW.pdf"))

}

# load package DT to generate a interactive table to look the crucial regression results
gt_term <- c("estimate", "std.error", "statistic", "p.value")

cs_gt %>% `[`(grep(pattern = "^[[:upper:]]*[[:punct:]]t[[:punct:]]Timeline", 
                   x = .$term, 
                   invert = TRUE
                   ), # select rows
              ) %T>% 
        print() %>% 
        select('term', all_of(gt_term)) %>% 
        DT::datatable(rownames = FALSE) %>% 
        DT::formatRound(columns = gt_term, digits = 3)
