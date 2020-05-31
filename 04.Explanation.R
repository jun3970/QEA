library(tidyverse)
library(lubridate)

library(DBI)
library(RSQLite)
library(dbplyr)

library(sandwich)
library(lmtest)
library(modelr)

library(broom)
library(stargazer)
library(gt)
library(RColorBrewer)
library(ggthemes)

library(magrittr)
library(rlang)
library(glue)

source('~/R/QEA/QEA.R/function_QEA.R')

# Specifying basical trem ====
Modeltype <- as.character('CH3')
Accprd <- ymd('2017-09-30')
Pretype <- 6L
Markettype <- 21L
# just want to analysis a sub-sample?
# subsam <- 300L
subsam <- FALSE

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
# re-level the order of group levels
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
    # take a subset of stocks sample
    stk_sam <- sample(names(stkeve), size = subsam, replace = FALSE)
    stkeve %<>% `[`(stk_sam)
    
} else if (`==`(subsam, FALSE)) {
    stk_sam <- names(stkeve)
    print("We will analysis the whole sample.")
}

# the number of stocks in our sample this quarter
N <- length(stkeve)
# the length of estimate window
TS <- nrow(stkeve[[N]])
# the timeline of event window
## should we abandon a part of window for the beauty of CAR path?
## if you want, Change the values of TS manually
timeline <- (-(TS - 1L) / 2):((TS - 1L) / 2)

# read the original daily trading data
QEA_db <- dbConnect(SQLite(), "../../QEA_db.sqlite")

TS_period <- seq(as.numeric(Accprd %m+% months(-9)), 
                 as.numeric(Accprd %m+% months(+9)), 
                 by = 1)
# Import the daily trading in China A-Share markets
trddat <- tbl(QEA_db, "daily") %>% 
        # subset the stocks in our quarterly sample
        filter(`&`(Stkcd %in% !!stk_sam, 
                   TradingDate %in% !!TS_period)) %>%
        collect() 

# transform the date format and re-level the market type 
trddat$TradingDate %<>% as.Date(origin = "1970-01-01")
trddat$Markettype %<>% factor(levels = as.character(c(1, 4, 16)))

# calculate the daily amplitude of stock
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
if (nrow(problems(ReptInfo)) >= 0L)  ReptInfo %<>% `[`(-unique(problems(.)$row), ) 

# # add explanation variables about quarterly accounting status of stocks
# ReptInfo %<>% left_join(Acc_ind, by = c("Stkcd", "Accper"))

dbDisconnect(QEA_db)

# import the data of abnormal returns from `03.CalCar`
QEA_gAR <- file.path(datdir, 
                     paste(Accprd, Pretype, Markettype, Modeltype, grp_num, "gAR.csv", sep = '_')
                     ) %>% 
    read_csv(col_types = cols(
                Stkcd = col_character(),
                TradingDate = col_date("%Y-%m-%d"),
                g_PLS = col_factor(levels = c(1:grp_num)),  ## PLS
                AbRet = col_double())  ## abnormal return
             ) %>% 
    split(.$Stkcd) %>% 
    lapply(arrange, TradingDate) %>% 
    lapply(add_column, "Timeline" = factor(timeline, ordered = TRUE))

# join above three tables using function `map2`
# we place two if statement at there, avoiding the situation that 
# merge procee produce NULL list (stock), when the order of stocks in two lists is unpaired
if (!all.equal(names(stkeve), names(trddat))) {
    stop("Please correct the order of stocks in lists `stkeve` and `trddat`. ")
} else if (!all.equal(names(stkeve), names(QEA_gAR))) {
    stop("Please correct the order of stocks in lists `stkeve` and `QEA_gAR`. ")
} else {

    eve_tbl <- map2(stkeve, trddat, left_join, 
                    by = c("Stkcd", "TradingDate", "Dretnd", "Dsmvosd", "Markettype")
                    ) %>% 
        lapply(add_column, "Timeline" = factor(timeline, ordered = TRUE)) %>% 
        map2(QEA_gAR[stk_sam], left_join,
             by = c("Stkcd", "TradingDate", "Timeline")) 
    
    if (!all.equal(rep(TS, N), unname(sapply(eve_tbl, nrow)))) {
        extra_stk <- names(eve_tbl)[`!=`(sapply(eve_tbl, nrow), TS)]
        glue("{extra_stk}, unusual! it's number of time periods not equal to {TS}.")
        } else {eve_tbl %<>% bind_rows() %>% 
                    # the names of our explanation variables (ordered by types)
                    select(c("Stkcd", "g_PLS", "TradingDate", "Timeline",
                           "Dretnd", "AbRet", "Dret_rf", "mkt_rf", "SMB", "VMG",
                           "Dnshrtrd", "Dnvaltrd", "Turnover", "Liquidility", "Amplitude",
                           "Dsmvtll", "Dsmvosd","PE", "PB", "PS")
                           ) 
                }

}

if (all.equal(rowSums(is.na.data.frame(select(eve_tbl, -c("PE", "PB", "PS")))), 
              rep(0L, nrow(eve_tbl))
              ) 
    ) {
    print(summary(eve_tbl))
    rm(QEA_gAR, stkeve); gc()
} else {print("joinging process had errors (NA)!")}

# summarise, mean and variance  
eve_tbl_summary <- group_by(eve_tbl, Timeline, g_PLS) %>% 
        summarise_if(.predicate = is.numeric, 
                     .funs = list(~mean(., na.rm = TRUE), ~var(., na.rm = TRUE)) 
                     )

gt_var_sum <- function(statistic) {
    
    eve_tbl_summary %>% 
        dplyr::select(c("Timeline", "g_PLS") | ends_with(statistic)) %>% 
        group_by(g_PLS) %>% 
        gt::gt() %>% 
        tab_header(
            title = glue("The {statistic} of explanation variables"),
            subtitle = glue("around the announced earnings report,
                            {(Accprd + days(1)) %m+% months(-3)} to {Accprd}")) %>% 
        fmt_number(columns = names(eve_tbl_summary) %>% 
                       `[`(., str_ends(., pattern = statistic)),
            decimals = 5,
            suffixing = TRUE) %>% 
        cols_label()
    
}
    
gt_var_sum("var")
gt_var_sum("mean")


# plot the standard deviation of returns ====
title_char <- paste0('The standard deviation of the weighted daily returns and ',
    'the weighted abnormal returns of stocks within event window')

group_by(eve_tbl, Timeline, g_PLS) %>% 
    summarise("real return" = sd( Dret_rf * (Dsmvosd/sum(Dsmvosd)) ),
              "abnormal return" = sd(AbRet * (Dsmvosd/sum(Dsmvosd)) ),
              .groups = "keep") %>% 
    gather(sd_type, sd_value, "real return", "abnormal return") %>% 
    mutate("Timeline" = as.character(Timeline) %>% as.integer()) %>% 
    ggplot(aes(x = Timeline, y = sd_value, colour = g_PLS)) +
        geom_path(aes(linetype = sd_type)) + 
        geom_point() +
        scale_x_continuous(breaks = seq(-30, 30, by = 5)) + 
        scale_color_manual(labels = grp_name, values = brewer.pal(grp_num, "Set1")) +
        labs(x = "Time line", y = "Standard deviation of returns", title = title_char, 
subtitle = glue('accounting period: {(Accprd + days(1)) %m+% months(-3)} to {Accprd}')) + 
        theme_economist() +
        theme(axis.ticks = element_blank(),
              axis.title = element_text(margin = margin(r = 10, t = 10)),
              legend.title = element_blank()
              )

mod_figure("Dret_sd", 3L, 1)


######## Regression (statistical properties) ########

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
title_char <- paste("The average daily returns within event window of portfolios",
        "which are structured by the difference of EPS among quarters",
        sep = " ")

# the index of the rect in plot of CAR
rect_index <- tibble::tribble(
    ~tier, ~xmin, ~xmax, ~ymin, ~ymax,
    1, -Inf,  -14,  -Inf,  +Inf,
    2,  +21,  Inf,  -Inf,  +Inf,
)

inner_join(eve_tbl, g_AMD, by = "Stkcd") %>% 
    group_by(g.AMD, Timeline) %>% 
    summarise("avgDret" = mean(Dretnd), .groups = "keep") %>% 
    filter(g.AMD != "EPS.Neutral") %>% 
        ggplot(aes(x = as.integer(as.character(Timeline)), 
                   y = avgDret, group = g.AMD)) +
        geom_path(aes(linetype = g.AMD)) +
        labs(y = "Average daily return", x = "Time line",
             title = title_char,
             subtitle = glue("{(Accprd + days(1)) %m+% months(-3)} to {Accprd}")) +
        scale_x_continuous(breaks = seq(-(TS-1L)/2L, (TS-1L)/2L, by = 5)) +
        scale_color_brewer(palette = "Set1") +
        theme_economist() +
        theme(legend.title = element_blank(),
              axis.title = element_text(margin = margin(r = 10, t = 10))) +
        geom_ref_line(h = 0, colour = "#999999") +
    geom_rect(data = rect_index, inherit.aes = FALSE, 
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
        alpha = 0.7, show.legend = FALSE
              ) 

mod_figure("EPSDiff", 3L, 1)


#### Time series ####

# liner model ====
# All models are wrong, but some are useful.
# The goal of a model is not to uncover truth, 
# but to discover a simple approximation that is still useful.

## trading
lm_trd <- function(df) {
    lm( AbRet ~ Amplitude + Turnover + Liquidility + Dnshrtrd + Dnvaltrd, data = df)
}
## factor 
lm_ff_t <- function(df) {lm(AbRet ~ AMD_t, data = df)}
lm_ff_tau <- function(df) {lm(AR_tau ~ AMD_tau, data = df)}


# calculate the factor AMD 
## trading date
AMD_t <- bind_rows(trddat) %>% 
    select(Stkcd, TradingDate, Dretnd, Dsmvosd) %>% 
    left_join(PLSclus, by = "Stkcd") %>%  ## PLS
    left_join(g_AMD, by = "Stkcd") %>%  ## AMD portfolio
    group_nest(TradingDate) %>%  ## t
    # Note that we will take the intersection portfolio of g_PLS and g.AMD
    # imitate the structure process of Fama-French factors
    mutate("AMD_Ret" = map(data, ~ summarise(group_by(.x, g_PLS, g.AMD), 
                            "ptf_Ret" = Dretnd %*% (Dsmvosd /sum(Dsmvosd)),
                            .groups = "keep")
                           )
           ) %>% 
    select(-data) %>% unnest(cols = "AMD_Ret") %>% 
    spread(g.AMD, ptf_Ret) %>% 
    # calculate the mean of portfolio returns (g_PLS)
    group_by(TradingDate) %>% 
    summarise_if(.predicate = is.numeric, .funs = mean) %>% 
    # take the minus of the g_PLS mean returns between the portfolios g.AMD
    transmute(TradingDate, "AMD_t" = Ascend - Descend)
    
rm(trddat); gc()

## tau 
### timeline 
AMD_tau <- inner_join(eve_tbl, g_AMD, by = "Stkcd") %>% 
    group_by(g.AMD, Timeline) %>% 
    summarise("avgDret" = Dretnd %*% (Dsmvosd / sum(Dsmvosd)), .groups = "keep") %>% 
    spread(g.AMD, avgDret) %>% 
    mutate("AMD_tau" = Ascend - Descend) %>% 
    select(Timeline, AMD_tau) 

### timeline * g_PLS
AR_tau <- group_by(eve_tbl, g_PLS, Timeline) %>% 
    ## calculate the AR_tau 
    summarise("AR_tau" = AbRet %*% (Dsmvosd / sum(Dsmvosd)), .groups = "keep")


# merge the factor AMD with regression data
eve_tbl %<>% left_join(AMD_t, by = "TradingDate") %>% 
    left_join(AMD_tau, by = "Timeline") %>% 
    left_join(AR_tau, by = c("Timeline", "g_PLS"))

eve_clst_grp <- group_nest(eve_tbl, g_PLS, keep = TRUE) %>%
    ## factor
    mutate("ts_ff_t" = map(data, lm_ff_t),
           "ts_ff_tau" = map(data, lm_ff_tau)
           ) %>%
    ## trading
    mutate("ts_trd" = map(data, lm_trd)) # %>%
    # mutate("ts_trd_resids" = map2(data, ts_trd, add_residuals)) %>% 
    # mutate("ts_ff_t_tidy" = map(ts_ff_t, broom::tidy),
    #        "ts_ff_tau_tidy" = map(ts_ff_tau, broom::tidy)
    #        ) %>% 
    # mutate("ts_trd_tidy" = map(ts_trd, broom::tidy),
    #        "ts_trd_glance" = map(ts_trd, broom::glance))

    ## table - trading explanation
    stargazer(lm_trd(eve_tbl),  # aggregate
              eve_clst_grp$ts_trd,  # group by group (PLS)
              # take the group relationship as a dummy variable (DID)
              lm(AbRet ~ (Amplitude + Turnover + Liquidility + Dnshrtrd + Dnvaltrd) * g_PLS, data = eve_tbl),
              align = TRUE, no.space = TRUE, type = "text", 
              dep.var.labels.include = FALSE, model.numbers = FALSE,
              column.labels = c("Unclassified", "Group one", "Group two", "Group three", "Dummy"),
              dep.var.caption = c("Dependent variable: Abnormal Return"))

    ## table - factor explanation - tau
    stargazer(eve_clst_grp$ts_ff_tau,  # group by group (PLS)
              # take the group relationship as a dummy variable (DID)
              lm(AR_tau ~ AMD_tau * g_PLS, data = eve_tbl),
              align = TRUE, no.space = TRUE, type = "text", 
              dep.var.labels.include = FALSE, model.numbers = FALSE,
              column.labels = c("Group one", "Group two", "Group three", "Dummy"),
              dep.var.caption = c("Dependent variable: Abnormal Return"))
    
    ## table - factor explanation - t
    stargazer(eve_clst_grp$ts_ff_t,  # group by group (PLS)
              # take the group relationship as a dummy variable (DID)
              lm(AbRet ~ AMD_t * g_PLS, data = eve_tbl),
              align = TRUE, no.space = TRUE, type = "text", 
              dep.var.labels.include = FALSE, model.numbers = FALSE,
              column.labels = c("Group one", "Group two", "Group three", "Dummy"),
              dep.var.caption = c("Dependent variable: Abnormal Return"))
    
    # TS - stock dimension ====
    ## trading explanation
    group_nest(eve_tbl, g_PLS, Stkcd) %>% 
        group_by(g_PLS) %>% sample_frac(0.005) %>% 
        mutate("ts_trd_stk" = map(data, ~ lm_trd(.x) %>% broom::tidy())) %>% 
        select(-data) %>% unnest(cols = ts_trd_stk) %>% 
        group_by(g_PLS, Stkcd) %>%
        gt::gt() %>% 
        tab_header(title = "Estimates of each coefficient in model (trading)",
            subtitle = glue("{(Accprd + days(1)) %m+% months(-3)} to {Accprd}")
            ) %>% 
        fmt_number(columns = c("estimate", "std.error", "statistic", "p.value"),
            decimals = 4,
            suffixing = TRUE)
    
    ## factor explanation - Robust Standard Errors
    ### owing the values of dependent and explanation variables 
    ### at every tau is same among stocks, so we just need to run one regression
    ### for all stocks at this part
    lm_ff_tau(eve_tbl) %>% 
        coeftest(., vcov = vcovHC(., type = "HC1")) 
    # take intersection with group
    lm(AR_tau ~ AMD_tau * g_PLS, data = eve_tbl) %>% 
        coeftest(., vcov = vcovHC(., type = "HC1"))

    
###### cross-section ######

lm_cs_grp <- function(df) {
    lm(AbRet ~ (AMD_t * g_PLS) + Turnover + Liquidility + Dnshrtrd, data = df)
}
    
lm_cs_gt <- function(df) {
    lm(AbRet ~ (AMD_t * Timeline) + Turnover + Liquidility + Dnshrtrd, data = df)
}

# take a sub-time-line,
# too may time periods (tau) to display tidiness
TS_period <- c(-3:3)
eve_tbl_sub <- filter(eve_tbl, Timeline %in% as.character(TS_period))

eve_clst_tau <- eve_tbl_sub %>% 
    group_nest(Timeline, keep = TRUE) %>% 
    mutate("cs_grp" = map(data, lm_cs_grp)) 

pull(eve_clst_tau, cs_grp) %>% 
    stargazer(lm_cs_gt(eve_tbl_sub), 
              align = TRUE, no.space = TRUE, type = "text",
              dep.var.labels.include = FALSE, model.numbers = FALSE,
              column.labels = c(str_c("tau = ", TS_period), "inter-timeline"),
              dep.var.caption = c("Dependent variable: Abnormal Return")) 
    
lm_cs_gt(eve_tbl_sub) %>% 
    coeftest(., vcov = vcovHC(., type = "HC1")) 
