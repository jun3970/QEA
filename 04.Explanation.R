library(tidyverse)
library(lubridate)

library(DBI)
library(RSQLite)
library(dbplyr)

library(magrittr)
library(modelr)
library(broom)
library(gt)
library(stargazer)

library(rlang)
library(glue)
library(RColorBrewer)
library(ggthemes)

source('~/R/QEA/QEA.R/function_QEA.R')

# Specifying basical trem ====
Modeltype <- as.character('CH3')
Accprd <- ymd('2017-09-30')
Pretype <- 6L
Markettype <- 21L
# if we just want to analysis a sub-sample?
subsam <- FALSE
# subsam <- 300L

###### Part I, read and tidy data ######
setwd( file.path('~/OneDrive/Data.backup/QEAData', Modeltype, year(Accprd)) )
# export directory
datdir <- file.path(getwd(), Accprd)

# PLS result
PLSclus <- '~/OneDrive/Data.backup/QEAData/Matlab_PLS' %>% 
        file.path(paste("group", Accprd, Pretype, Markettype,
                        'CH3.csv', sep = '_')) %>% 
        read_csv(col_types = cols( g_PLS = col_factor() ))

# the number of groups by PLS (su, 2016)
grp_num <- PLSclus$g_PLS %>% levels() %>% length()
# relevel the prder of group levels
PLSclus$g_PLS %<>% fct_relevel( as.character(1:grp_num) )

if (grp_num == 2) {
    grp_name <- c("group one", "group two")
} else if (grp_num == 3)  {
    grp_name <- c("group one", "group two", "group three")
} else if (grp_num == 4) {
    grp_name <- c("group one", "group two", "group three", "group four")
} else print("The group number is not included in script!")

# Import the daily trading data (with explanation factors) within event window
stkeve <- mod_read("stkeve") %>% split(.$Stkcd)
if (is.numeric(subsam)) { 
    set.seed(subsam)
    stk_sam <- names(stkeve) %>% sample(size = subsam)
    stkeve %<>% `[`(stk_sam)
} else if (`==`(subsam, FALSE)) {
    print("We will analysis the whole sample.")
    stk_sam <- names(stkeve)
    }

# the number of stocks in our sample this quarter
N <- length(stkeve)
# the length of estimate window
TS <- nrow(stkeve[[N]])
# the timeline of event window
timeline <- (-(TS - 1L) / 2):((TS - 1L) / 2)
## for the beauty of CAR path, should we abandon a part of window?
## if you want, Change the values of TS

# read the original daily trading data
QEA_db <- dbConnect(SQLite(), "../../QEA_db.sqlite")

TS_period <- seq(as.numeric(Accprd %m+% months(-6)), 
                 as.numeric(Accprd %m+% months(+6)), 
                 by = 1)
# Import the daily trading in China A-Share markets
trddat <- tbl(QEA_db, "daily") %>% 
        # subset the stocks in our quarterly sample
        filter(`&`(Stkcd %in% !!stk_sam, 
                   TradingDate %in% !!TS_period)) %>%
        collect() 

# transform to date
trddat$TradingDate %<>% as.Date(origin = "1970-01-01")
trddat$Markettype %<>% factor(levels = as.character(c(1, 4, 16)))
# calculate the daily amplitude of stocks
trddat %<>% split(.$Stkcd) %>% 
    # calculate the amplitude of stocks on trading day
    lapply(mutate, "Amplitude" = c(NaN, `/`((Hiprc - Loprc)[-1], 
                                            Clsprc[-length(Clsprc)])
                                   )
           )

# Import quarterly accounting data
qtr_term <- c((Accprd + days(1)) %m+% months(-6) + days(-1), 
              (Accprd + days(1)) %m+% months(-3) + days(-1))

# Acc_ind <- tbl(QEA_db, "quarter") %>% 
#         # subset the stocks in our quarterly sample
#         filter(`&`(Stkcd %in% !!stk_sam, 
#                    Accper %in% !!as.numeric(qtr_term))
#                ) %>% 
#         filter(Typrep == "A") %>% select(-Typrep) %>% 
#         collect() %>% 
#         arrange(Stkcd, Accper)
# 
# Acc_ind$Accper %<>% as.Date(origin = "1970-01-01")

dbDisconnect(QEA_db)

# Import the accounting data within quarterly financial report 
ReptInfo <- read_delim('~/OneDrive/Data.backup/QEAData/Acc_Quarter/IAR_Rept.txt', 
        delim = '\t', na = '', col_types = cols_only(Stkcd = col_character(),
                # the deadline of accounting cycle
                Accper = col_date("%Y-%m-%d"),
                # the date when report was discolsed
                Annodt = col_date("%Y-%m-%d"),
                # net profits and earnings per share
                Profita = col_double(), Erana = col_double())
        ) %>% 
    filter(Stkcd %in% stk_sam) %>% 
    filter(Accper %in% qtr_term) %>% 
    arrange(Stkcd, Accper)
# Attention! there has some problem observations, we choose to delete them
if (nrow(problems(ReptInfo)) >= 0L)  ReptInfo %<>% `[`(-problems(.)$row, ) 
# # add explanation variables about quarterly accounting status of stocks
# left_join(ReptInfo, Acc_ind, by = c("Stkcd", "Accper"))


# import the data of abnormal returns
QEA_gAR <- file.path(datdir, 
    paste(Accprd, Pretype, Markettype, Modeltype, grp_num, "gAR.csv", sep = '_')) %>% 
    read_csv(col_types = cols(Stkcd = col_character(),
            TradingDate = col_date("%Y-%m-%d"),
            g_PLS = col_factor(levels = c(1:grp_num)),
            AbRet = col_double())  ## abnormal return
        ) %>% 
    split(.$Stkcd) %>% 
    lapply(arrange, TradingDate) %>% 
    lapply(add_column, "Timeline" = factor(timeline, ordered = TRUE))

# join above three tables using function `map2`
# we place two if statement at there, avoiding the situation that 
# merge procee produce NULL list (stock), when the order of stocks in two lists is unpaired
if (!all_equal(names(stkeve), names(trddat))) {
    stop("Please correct the order of stocks in lists `stkeve` and `trddat`. ")
} else if (!all_equal(names(stkeve), names(QEA_gAR))) {
    stop("Please correct the order of stocks in lists `stkeve` and `QEA_gAR`. ")
} else {

    eve_tbl <- map2(stkeve, trddat, left_join, 
            by = c("Stkcd", "TradingDate", "Dretnd", "Dsmvosd", "Markettype")) %>% 
        lapply(add_column, "Timeline" = factor(timeline, ordered = TRUE)) %>% 
        map2(QEA_gAR[stk_sam], left_join,
             by = c("Stkcd", "TradingDate", "Timeline")) 
    
    if (!all_equal(rep(TS, N), sapply(eve_tbl, nrow) %>% unname())) {
        extra_stk <- names(eve_tbl)[`!=`(sapply(eve_tbl, nrow), TS)]
        glue("{extra_stk}, unusual! it's number of time periods not equal to {TS}.")
        } else eve_tbl %<>% bind_rows() 

}

rm(QEA_gAR, stkeve); gc()

print(colnames(eve_tbl))
table_eve_summary <- group_by(eve_tbl, Timeline, g_PLS) %>% 
        summarise_if(.predicate = is.numeric, 
                     .funs = list(~mean(., na.rm = TRUE), ~var(., na.rm = TRUE)) 
                     )

print(names(table_eve_summary))
table_variable <- c("AbRet", "Dretnd", "Dnshrtrd", "Dnvaltrd",
                    "Turnover", "Liquidility", "Amplitude",
                    "Dret_rf", "mkt_rf", "SMB", "VMG",
                    "Dsmvtll", "PE", "PB", "PS", "PE"
                    )
table_variable_mean <- c("Timeline", "g_PLS", 
                         paste(table_variable, "mean", sep = "_")
                         )
table_variable_var <- c("Timeline", "g_PLS", 
                         paste(table_variable, "var", sep = "_")
                         )

table_eve_summary %>% 
    dplyr::select(!!table_variable_mean) %>% 
    gt::gt() %>% 
    tab_header(
        title = "The mean value of our explanation variables",
        subtitle = glue("around the announced earnings report,
                        {(Accprd + days(1)) %m+% months(-3)} to {Accprd}")) %>% 
    fmt_number(columns = table_variable_mean[-c(1, 2)],
                decimals = 4,
                suffixing = TRUE) %>% 
    cols_label()

table_eve_summary %>% 
    dplyr::select(!!table_variable_var) %>% 
    gt::gt() %>% 
    tab_header(
        title = "The variance of our explanation variables",
        subtitle = glue("around the announced earnings report,
                        {(Accprd + days(1)) %m+% months(-3)} to {Accprd}")) %>% 
    fmt_number(columns = table_variable_var[-c(1, 2)],
        decimals = 5,
        suffixing = TRUE) %>% 
    cols_label()


# plot the standard deviation of returns ====
# Standard deviation of group stock's daily returns (event window)
group_by(eve_tbl, g_PLS, Timeline) %>% 
    summarise("Dret_rf_sd" = sd(Dret_rf)) %>% 
    # spread(g_PLS, Dret_rf_sd) %>% 
    mutate("Timeline" = as.character(timeline) %>% as.integer()) %>% 
    ggplot(aes(x = Timeline, y = Dret_rf_sd, colour = g_PLS)) +
        geom_path() + 
        geom_point(aes(shape = g_PLS), size = 3) +
        scale_x_continuous(breaks = seq(-30, 30, by = 5)) + 
        scale_color_manual(labels = grp_name, values = brewer.pal(grp_num, "Set1")) +
        labs(x = "Time line", y = "standard deviation",
             title = 'The standard deviation of the real daily returns of stocks') + 
        theme_economist() +
        theme(axis.ticks = element_blank(),
              axis.title = element_text(margin = margin(r = 10, t = 10)),
              legend.title = element_blank()
              )

mod_figure("Dret_sd", 3L, 0.85)

# Standard deviation of group stock's abnormal returns (event window)
group_by(eve_tbl, g_PLS, Timeline) %>% 
    summarise("AR_sd" = sd(AbRet)) %>% 
    # spread(g_PLS, AR_sd) %>% 
    mutate("Timeline" = as.character(timeline) %>% as.integer()) %>% 
    ggplot(aes(x = Timeline, y = AR_sd, colour = g_PLS)) +
        geom_path() + 
        geom_point(aes(shape = g_PLS), size = 3) +
        scale_x_continuous(breaks = seq(-30, 30, by = 5)) + 
        scale_color_manual(labels = grp_name, values = brewer.pal(grp_num, "Set1")) +
        labs(x = "Time line",  y = "standard deviation",
title = 'The standard deviation of the daily abnormal returns of stocks within event window') + 
        theme_economist() +
        theme(axis.ticks = element_blank(),
              axis.title = element_text(margin = margin(r = 10, t = 10)),
              legend.title = element_blank()
              )

mod_figure("AR_sd", 3L, 0.85)


######## Regression (statistical properties) ########
# All models are wrong, but some are useful.
# The goal of a model is not to uncover truth, 
# but to discover a simple approximation that is still useful.


#### Time series ####
# trading ====
lm_trd <- function(df) {
    lm( AbRet ~ Amplitude + Turnover + Liquidility + Dnshrtrd + Dnvaltrd, data = df)
    }

## unclassified
stargazer(lm_trd(eve_tbl), title = "Results of Aggregate data", 
          dep.var.labels = c("Abnormal Return"), 
          align = TRUE, type = "text")

## PLS
eve_clst <- group_nest(eve_tbl, g_PLS) %>%
    mutate("ts_trd" = map(data, lm_trd)) %T>%
    stargazer(.$ts_trd, align = TRUE, no.space = TRUE, type = "text", 
              dep.var.labels.include = FALSE, model.numbers = FALSE,
              column.labels = c("Group one", "Group two", "Group three"),
              dep.var.caption = c("Dependent variable: Abnormal Return")) %>%
    mutate("ts_trd_tidy" = map(ts_trd, broom::tidy),
           "ts_trd_glance" = map(ts_trd, broom::glance)) %>% 
    mutate("resids" = map2(data, ts_trd, add_residuals))

## test (DID)
lm(AbRet ~ (Amplitude + Turnover + Liquidility + Dnshrtrd + Dnvaltrd) * g_PLS, data = eve_tbl) %>% 
    stargazer(align = TRUE, type = "text", title = "DID (group test)", 
              dep.var.labels.include = FALSE,
              column.labels = c("Abnormal Return"))

## model performace among groups
select(eve_clst, g_PLS, ts_trd_tidy) %>% 
    unnest(cols = c(ts_trd_tidy)) %>% 
    gt::gt() %>% 
    tab_header(
        title = "Estimate and variablity of each coefficient in the model",
        subtitle = glue("around the announced earnings report,
                        {(Accprd + days(1)) %m+% months(-3)} to {Accprd}")) %>% 
    fmt_number(columns = c("estimate", "std.error", "statistic", "p.value"),
                decimals = 4,
                suffixing = TRUE)

## stock dimension
group_nest(eve_tbl, g_PLS, Stkcd) %>% 
    mutate("ts_trd_stk" = map(data, lm_trd)) %>% 
    mutate("ts_trd_tidy" = map(ts_trd_stk, tidy)) %>% 
    unnest(cols = ts_trd_tidy) %>% 
    select(-c(data, ts_trd_stk)) %>% 
    group_by(g_PLS) %>% 
    sample_frac(0.01) %>% 
    gt::gt() %>% 
    fmt_number(columns = c("estimate", "std.error", "statistic", "p.value"),
                decimals = 4,
                suffixing = TRUE)
    
eve_tbl %>% split(.$g_PLS) %>% 
    lapply(function(x) { split(x, x$Stkcd) %>% lapply(lm_trd) }) %>% 
        lapply(sample, 3) %>% 
            stargazer(align = TRUE, type = "text")


# factor =====
lm_ff <- function(df) lm(AbRet ~ AMD, data = df)

## cluster the stocks according to the difference of EPS between two quarters
g_AMD <- select(ReptInfo, -c(Annodt, Profita)) %>% 
    spread(Accper, Erana) %>% 
    mutate("Eran_diff" = `-`(!!sym(as.character(qtr_term[2])),
                             !!sym(as.character(qtr_term[1])))
           ) %>% 
    mutate('g.AMD' = cut(Eran_diff, quantile(Eran_diff, c(0, 0.3, 0.7, 1)), 
                         labels = c("Descend", "EPS.Neutral", "Ascend"),
                         ordered_result = TRUE, include.lowest = TRUE)
           ) %>% 
    select(Stkcd, g.AMD)

# plot the adjustment effect of EPS (AMD)
inner_join(eve_tbl, g_AMD, by = "Stkcd") %>% 
    group_by(g.AMD, Timeline) %>% 
    summarise("avgDret" = mean(Dretnd)) %>% 
    filter(g.AMD != "EPS.Neutral") %>% 
        ggplot(aes(x = as.integer(as.character(Timeline)), 
                   y = avgDret, group = g.AMD, colour = g.AMD)) +
        geom_path() +
        labs(y = "Average daily return", x = "Time line") +
        scale_x_continuous(breaks = seq(-(TS-1L)/2L, (TS-1L)/2L, by = 5)) +
        scale_color_brewer(palette = "Set1") +
        theme_economist() +
        theme(legend.title = element_blank(),
              axis.title = element_text(margin = margin(r = 10, t = 10))) +
        geom_ref_line(h = 0, colour = "#999999")

# calculate the factor AMD 
## (tau)    
AMD_tau <- inner_join(eve_tbl, g_AMD, by = "Stkcd") %>% 
    group_by(g.AMD, Timeline) %>% 
    summarise("avgDret" = mean(Dretnd)) %>% 
    spread(g.AMD, avgDret) %>% 
    mutate("AMD" = Ascend - Descend) %>% 
    select(Timeline, AMD) 

## (trading date)
AMD_t <- bind_rows(trddat) %>% 
    select(Stkcd, TradingDate, Dretnd, Dsmvosd) %>% 
    left_join(PLSclus, by = "Stkcd") %>% 
    left_join(g_AMD, by = "Stkcd") %>% 
    group_nest(TradingDate) %>%
    mutate("p.Ret" = map(data, 
~ summarise(group_by(.x, g_PLS, g.AMD), "ptf_Ret" = Dretnd %*% (Dsmvosd /sum(Dsmvosd)))
                         )
           ) %>% 
    select(-data) %>% 
    unnest(cols = "p.Ret") %>% 
    spread(g.AMD, ptf_Ret) %>% 
    group_by(TradingDate) %>% 
    summarise_if(.predicate = is.numeric, .funs = mean) %>% 
    transmute(TradingDate, "AMD" = Ascend - Descend)
    
rm(trddat); gc()

# merge the AMD_t with regression data
left_join(eve_tbl, AMD_t, by = "TradingDate") %>% 
    split(.$g_PLS) %>%
    map(lm_ff) %>% 
    stargazer(align = TRUE, type = "text")

# left_join(eve_tbl, AMD_tau, by = "Timeline") %>% 
#     split(.$g_PLS) %>%
#     map(lm_ff) %>% 
#     stargazer(align = TRUE, type = "text")

left_join(eve_tbl, AMD_t, by = "TradingDate") %>% 
    lm(AbRet ~ AMD * g_PLS, data = .) %>% 
    stargazer(align = TRUE, type = "text")

# # stock dimension
# ## data frame
# ff_ts <- group_nest(eve_tbl, g_PLS, Stkcd) %>% 
#     mutate("ff_ts_model" = map(data, ~ tidy(lm_ff(.x)))) %>% 
#     select(-data) %>% 
#     unnest(cols = c(ff_ts_model))
# 
# ## table
# eve_tbl %>% split(.$g_PLS) %>% 
#     lapply(function(x) { split(x, x$Stkcd) %>% lapply(lm_ff)}) %>% 
#         lapply(sample, 3) %>% 
#             stargazer(align = TRUE, type = "text")


###### cross-section ######
lm_ff_cs <- function(df) lm(AbRet ~ AMD * Timeline, data = df)

left_join(eve_tbl, AMD_t, by = "TradingDate") %>% 
    group_nest(g_PLS, Timeline) %>% 
    mutate("cs_trd_tidy" = map(data, ~ tidy(lm_trd(.x)))) %>%
    select(-data) %>% 
    unnest(cols = c(cs_trd_tidy)) %>% 
    gt::gt() %>% 
    fmt_number(columns = c("estimate", "std.error", "statistic", "p.value"),
            decimals = 4,
            suffixing = TRUE)


left_join(eve_tbl, AMD_t, by = "TradingDate") %>%
    split(.$g_PLS) %>% 
    lapply(lm_ff_cs) %>% 
    stargazer(align = TRUE, type = "text")

