# This script is written to explain the abnormal returns around announced date
# we have two kind of explanation variables, trading and factor
# and we run two kind of regression, time series and cross-sectional

# All models are wrong, but some are useful.
# The goal of a model is not to uncover truth, 
# but to discover a simple approximation that is still useful.

library(tidyverse)
library(lubridate)

library(modelr)
library(estimatr)
# library(sandwich)
# library(lmtest)

library(magrittr)
library(rlang)
library(glue)

library(broom)
library(stargazer)
library(texreg)
library(gt)

library(multipanelfigure)
library(RColorBrewer)
library(ggthemes)

library(DBI)
library(RSQLite)
library(dbplyr)


# function ----------------------------------------------------------------
source('~/R/QEA/QEA.R/function_QEA.R')

# generate a summary table of our variable (feature) by package gt
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

# generate the tex code or table at console of existing model (regression)
mod_texreg <- function(df, tex = TRUE, ...) {
    
    if (tex == FALSE) {  # display the table on the console
        
        screenreg(df,
            caption.above = TRUE, dcolumn = TRUE, booktabs = TRUE, 
            include.ci = FALSE, digits = 3, 
            override.se = map(df, "statistic"), 
               ...,) %>%
        gsub("g_PLS2","group two", ., fixed = TRUE) %>%
        gsub("g_PLS3","group three", ., fixed = TRUE)
        
    } else {  # print the tex code of our table
        
        texreg(df,
            caption.above = TRUE, dcolumn = TRUE, booktabs = TRUE, 
            include.ci = FALSE, digits = 3, 
            override.se = map(df, "statistic"),
               ...,) %>%
        gsub("g\\_PLS2","group two", ., fixed = TRUE) %>%
        gsub("g\\_PLS3","group three", ., fixed = TRUE)
    }
    
}   


# read and tidy data ----------------------------------------------------
# Specifying basic term 
Accprd <- ymd('2017-09-30')
Modeltype <- as.character('CH3')
Pretype <- 6L
Markettype <- 21L
# If we just want to analysis a part of sample?
# subsam <- 300L
subsam <- FALSE

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
# the time line of event window
## should we abandon a part of window for the beauty of CAR path?
## if you want, Change the values of TS manually
timeline <- (-(TS - 1L) / 2):((TS - 1L) / 2)

# read the original daily trading data
QEA_db <- dbConnect(SQLite(), "../../QEA_db.sqlite")

qtr_term <- seq(as.numeric(Accprd %m+% months(-9)), 
                 as.numeric(Accprd %m+% months(+9)), 
                 by = 1)
# Import the daily trading in China A-Share markets
trddat <- tbl(QEA_db, "daily") %>% 
        # subset the stocks in our quarterly sample
        filter(`&`(Stkcd %in% !!stk_sam, 
                   TradingDate %in% !!qtr_term)) %>%
        collect() 
# transform the date format and re-level the market type 
trddat$TradingDate %<>% as.Date(origin = "1970-01-01")
trddat$Markettype %<>% factor(levels = as.character(c(1, 4, 16)))

# calculate the daily amplitude of stock
trddat %<>% split(.$Stkcd) %>% 
    # calculate the amplitude of stocks on trading day
    lapply(mutate, 
           "Amplitude" = c(NaN, `/`((Hiprc - Loprc)[-1], Clsprc[-length(Clsprc)]))
           )

# Import quarterly accounting data
qtr_term <- c((Accprd + days(1)) %m+% months(-6) + days(-1), 
              (Accprd + days(1)) %m+% months(-3) + days(-1))

Acc_ind <- tbl(QEA_db, "quarter") %>%
        # subset the stocks in our quarterly sample
        filter(`&`(Stkcd %in% !!stk_sam,
                   Accper %in% !!as.numeric(qtr_term))
               ) %>%
        filter(Typrep == "A") %>% select(-Typrep) %>%  # parent company report
        collect() %>%
        arrange(Stkcd, Accper)

Acc_ind$Accper %<>% as.Date(origin = "1970-01-01")

# Import the accounting data within quarterly financial report 
ReptInfo <- read_delim('~/OneDrive/Data.backup/QEAData/Acc_Quarter/IAR_Rept.txt', 
        delim = '\t', na = '', col_types = cols_only(Stkcd = col_character(),
                # the deadline of accounting cycle
                Accper = col_date("%Y-%m-%d"),
                # the date when report was disclosure
                Annodt = col_date("%Y-%m-%d"),
                # net profits and earnings per share
                Profita = col_double(), Erana = col_double())
        ) %>% 
    filter(Stkcd %in% stk_sam) %>%  # stocks in sample
    filter(Accper %in% qtr_term) %>%  # specific quarter
    arrange(Stkcd, Accper)
# Attention! there are some problem observations, we choose to delete them
if (nrow(problems(ReptInfo)) >= 0L)  ReptInfo %<>% `[`(-unique(problems(.)$row), ) 

# add explanation variables about quarterly accounting status of stocks
ReptInfo %<>% left_join(Acc_ind, by = c("Stkcd", "Accper"))

dbDisconnect(QEA_db)

# import the data of abnormal returns from `03.CalCar`
QEA_gAR <- file.path(datdir, 
        paste(Accprd, Pretype, Markettype, Modeltype, grp_num, "gAR.csv", sep = '_')) %>% 
    read_csv(col_types = cols(
                Stkcd = col_character(),
                TradingDate = col_date("%Y-%m-%d"),
                g_PLS = col_factor(levels = c(1:grp_num)),  ## PLS - classification 
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
                by = c("Stkcd", "TradingDate", "Dretnd", "Dsmvosd", "Markettype")) %>% 
        lapply(arrange, TradingDate) %>%
        lapply(add_column, "Timeline" = factor(timeline, ordered = TRUE)) %>% 
        map2(QEA_gAR[stk_sam], left_join,
             by = c("Stkcd", "TradingDate", "Timeline")) 
    
    if (!all.equal(rep(TS, N), unname(sapply(eve_tbl, nrow)))) {
        
        extra_stk <- names(eve_tbl)[`!=`(sapply(eve_tbl, nrow), TS)]
        glue("{extra_stk}, unusual! it's number of time periods not equal to {TS}.")
        
        } else {  # transform the lists of stocks tibble to one tibble and select variables
                    
            eve_tbl %<>% bind_rows() %>% 
            # the names of our explanation variables (ordered by types)
            select(c("Stkcd", "g_PLS", "TradingDate", "Timeline",
                   "Dretnd", "AbRet", "Dret_rf", "mkt_rf", "SMB", "VMG",
                   "Dnshrtrd", "Dnvaltrd", "Turnover", "Liquidility", "Amplitude",
                   "Dsmvtll", "Dsmvosd","PE", "PB", "PS")) 
                    
                }
}

if (all.equal(rowSums(is.na.data.frame(select(eve_tbl, -c("PE", "PB", "PS")))), 
              rep(0L, nrow(eve_tbl))
              ) 
    ) {print(summary(eve_tbl))
       rm(QEA_gAR, stkeve); gc()
} else {print("joinging process had errors (NA)!")}

# summarise, mean and variance  
eve_tbl_summary <- group_by(eve_tbl, Timeline, g_PLS) %>% 
        summarise_if(.predicate = is.numeric, 
                     .funs = list(~mean(., na.rm = TRUE), ~var(., na.rm = TRUE)) 
                     )

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


# Regression (statistical properties) -------------------------------------

# which accounting index is used to structure the group?
g_Acc <- "Erana"

## cluster the stocks in sample according to the difference of the 
## accounting index (EPS) in the two quarters prior to this quarter
g_AMD <- select(ReptInfo, c("Stkcd", "Accper", g_Acc)) %>% 
    spread(Accper, g_Acc) %>% 
    mutate("diff" = `-`(!!sym(as.character(qtr_term[2])),
                             !!sym(as.character(qtr_term[1])))
           ) %>% 
    mutate('g.AMD' = cut(diff, quantile(diff, c(0, 0.3, 0.7, 1)), 
                         labels = c("Descend", "Neutral", "Ascend"),
                         ordered_result = TRUE, include.lowest = TRUE)
           ) %>% 
    select(Stkcd, g.AMD)

# plot the adjustment effect of EPS (AMD)
title_char <- paste0("The average daily returns within event window of portfolios ",
        "which are structured by the difference of EPS among quarters")

# the index of the rectangle in plot of CAR
rect_index <- tibble::tribble(
    ~tier, ~xmin, ~xmax, ~ymin, ~ymax,
    1, -Inf,  -14,  -Inf,  +Inf,
    2,  +21,  Inf,  -Inf,  +Inf,
)

inner_join(eve_tbl, g_AMD, by = "Stkcd") %>% 
    group_by(g.AMD, Timeline) %>% 
    summarise("avgDret" = mean(Dretnd), .groups = "keep") %>% 
    filter(g.AMD != "Neutral") %>% 
        ggplot(aes(x = as.integer(as.character(Timeline)), 
                   y = avgDret, group = g.AMD)) +
            geom_path(aes(linetype = g.AMD)) +
            labs(y = "Average daily return", x = "Time line",
                 title = title_char,
subtitle = glue("Grouped by {g_Acc}, {(Accprd + days(1)) %m+% months(-3)} to {Accprd}")) +
            scale_x_continuous(breaks = seq(-(TS-1L)/2L, (TS-1L)/2L, by = 5)) +
            scale_color_brewer(palette = "Set1") +
            theme_economist() +
            theme(legend.title = element_blank(),
                  axis.title = element_text(margin = margin(r = 10, t = 10))) +
            geom_ref_line(h = 0, colour = "#999999") +
        geom_rect(data = rect_index, inherit.aes = FALSE, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            alpha = 0.7, show.legend = FALSE) 

mod_figure("Diff", 3L, 1)



# Time series - Ordinary Least Squares with Robust Standard Errors ---------

    ## trading
    lm_trd <- function(df, model_type, SE_type = "stata", ...) {
        
        if (model_type == "agg") {
        ### running all data at once (Cluster-Robust Variance)
        lm_result <- lm_robust(data = df, clusters = g_PLS, se_type = SE_type, ...,
                formula = AbRet ~ Amplitude + Turnover + Liquidility + Dnshrtrd)
        } else if (model_type == "lst") {
        ### running separate by group (list-column)
        lm_result <- lm_robust(data = df, se_type = SE_type, ...,
                formula = AbRet ~ Amplitude + Turnover + Liquidility + Dnshrtrd)
        } else if (model_type == "inter") {
        ### take the group information as the interaction
        lm_result <- lm_robust(data = df, se_type = SE_type, ...,
                formula = AbRet ~ (Amplitude + Turnover + Liquidility + Dnshrtrd) * g_PLS)
        } else stop("Please select a model correctly!")
        
        return(lm_result)
    }


    ## factor
    #### owing the values of dependent and explanation variables of any stock
    #### within a group at every tau is same with others, 
    #### so we just need to run one regression for all stocks at this part
    lm_ff <- function(df, ts_type, model_type, SE_type = "stata", ...) {
        
        if (ts_type == "t") {  ### take the calendar date as the time line
    
            if (model_type == "agg") {  ### aggregate
                    lm_result <- lm_robust(data = df, clusters = g_PLS, se_type = SE_type,
                            formula = AbRet ~ AMD_t, ...)
            } else if (model_type == "lst") {  ### separate by group (list-column)
                    lm_result <- lm_robust(data = df, se_type = SE_type, ...,
                            formula = AbRet ~ AMD_t)
            } else if (model_type == "inter") {  ### interaction with group relationship
                    lm_result <- lm_robust(data = df, se_type = SE_type, ...,
                            formula = AbRet ~ AMD_t * g_PLS)
            } else stop("Please select a model correctly!")
            
        } else if (ts_type == "tau") {  
            ### take the tau (be front of the event happened date) as the time line
            
            if (model_type == "lst") {  ### separate by group (list-column)
                    lm_result <- lm_robust(data = df, se_type = SE_type, ...,
                            formula = AR_tau ~ AMD_tau)
            } else if (model_type == "inter") {  ### interaction with group relationship
                    lm_result <- lm_robust(data = df, se_type = SE_type, ...,
                            formula = AR_tau ~ AMD_tau * g_PLS)
            } else stop("Please select a model correctly!")
            
        } else stop("Please select a correct time series type!")
        
        return(lm_result)
    }


# Calculate the factor AMD ------------------------------------------------

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
### group by time-line 
AMD_tau <- inner_join(eve_tbl, g_AMD, by = "Stkcd") %>% 
    group_by(g.AMD, Timeline) %>% 
    summarise("avgDret" = Dretnd %*% (Dsmvosd / sum(Dsmvosd)), .groups = "keep") %>% 
    spread(g.AMD, avgDret) %>% 
    mutate("AMD_tau" = Ascend - Descend) %>% 
    select(Timeline, AMD_tau) 

### group by time-line and g_PLS classification result
AR_tau <- group_by(eve_tbl, g_PLS, Timeline) %>% 
    ## calculate the AR_tau 
    summarise("AR_tau" = AbRet %*% (Dsmvosd / sum(Dsmvosd)), .groups = "keep")

# merge the factor AMD with regression data
eve_tbl %<>% left_join(AMD_t, by = "TradingDate") %>% 
    left_join(AMD_tau, by = "Timeline") %>% 
    left_join(AR_tau, by = c("Timeline", "g_PLS"))


# Running time series regression ------------------------------------------

eve_clst_grp <- group_nest(eve_tbl, g_PLS) %>%
    ## factor
    mutate("ts_ff_t" = map(data, lm_ff, ts_type = "t", model_type = "lst")) %>%
    ## trading
    mutate("ts_trd" = map(data, lm_trd, model_type = "lst")) # %>%
    # mutate("ts_trd_resids" = map2(data, ts_trd, add_residuals)) 


## table - trading explanation
tbl_ts_trd <- list(lm_trd(eve_tbl, model_type = "agg"),  # aggregate
                   eve_clst_grp$ts_trd,  # separate by group
                   lm_trd(eve_tbl, model_type = "inter")  # ads group interaction
                   ) %>% flatten() 

mod_texreg(tbl_ts_trd, tex = TRUE, 
           custom.header = list("Abnormal Return (t)" = 1:5),
           custom.model.names = c("Unclassified", grp_name, "inter-group"),
           reorder.coef = c(1, 6, 7, 2:5, seq(8, 15, 2), seq(9, 15, 2))
           ) %>% 
    cat()


## table - factor explanation - t
tbl_ts_ff_t <- list(lm_ff(eve_tbl, ts_type = "t", model_type = "agg"),  
                    eve_clst_grp$ts_ff_t,
                    lm_ff(eve_tbl, ts_type = "t", model_type = "inter")
                    ) %>% flatten()

mod_texreg(tbl_ts_ff_t, tex = TRUE, 
           custom.header = list("Abnormal Return (t)" = 1:5),
           custom.model.names = c("Unclassified", grp_name, "inter-group"))


## table - factor explanation - tau
tbl_ts_ff_tau <- inner_join(AR_tau, AMD_tau, by = "Timeline") %>% 
    group_by(g_PLS) %>% nest() %>% 
    transmute("ts_ff_tau" = map(data, lm_ff, ts_type = "tau", model_type = "lst")) %>%
    pluck("ts_ff_tau") %>% 
    list(., lm_ff(inner_join(AR_tau, AMD_tau, by = "Timeline"), 
                  ts_type = "tau", model_type = "inter")
         ) %>% flatten()
    
mod_texreg(tbl_ts_ff_tau, tex = TRUE, 
           custom.header = list("Abnormal Return (tau)" = 1:4),
           custom.model.names = c(grp_name, "inter-group"))
     

    # stock dimension - trading
    group_nest(eve_tbl, g_PLS, Stkcd) %>% 
        sample_frac(0.005) %>% arrange(g_PLS, Stkcd) %>% 
        mutate("ts_trd_stk" = map(data, ~ broom::tidy(lm_trd(.x, model_type = 'lst')))
               ) %>% 
        select(-data) %>% unnest(cols = ts_trd_stk) %>% 
        group_by(g_PLS, Stkcd) %>% 
            # as.data.frame() %>% stargazer(summary = FALSE, rownames = FALSE)
            gt::gt() %>% 
            tab_header(title = "Estimates of each coefficient in model (trading)",
                subtitle = glue("{(Accprd + days(1)) %m+% months(-3)} to {Accprd}")
                ) %>% 
            fmt_number(columns = c("estimate", "std.error", "statistic", "p.value"),
                decimals = 4,
                suffixing = TRUE)


    
# Cross-sectional ---------------------------------------------------------
    ## factor and trading explanation are combined
    lm_cs <- function(df, model_type, SE_type = "stata", ...) {
        
        if (model_type == "grp") { # interact with group (list-column)
                lm_result <- lm_robust(data = df, se_type = SE_type, ...,
                    formula = AbRet ~ (AMD_t * g_PLS) + Turnover + Liquidility + Dnshrtrd)
        } else if (model_type == "tlm") { # interact with time line (aggregate)
                lm_result <- lm_robust(data = df, se_type = SE_type, ..., # clusters = g_PLS,
                    formula = AbRet ~ (AMD_t * Timeline) + Turnover + Liquidility + Dnshrtrd)
        } else if (model_type == "gt") { # interact with time line and group (aggregate)
                lm_result <- lm_robust(data = df, se_type = SE_type, ...,
                    formula = AbRet ~ AMD_t + AMD_t:Timeline + AMD_t:Timeline:g_PLS + 
                                        Turnover + Liquidility + Dnshrtrd)
        } else stop("Please select a model correctly!")
            
        return(lm_result)
    }

# re-level the time line (take the tau = 0 as benchmark in regression) 
eve_tbl$Timeline %<>% fct_relevel(as.character(c(0, timeline[-(TS+1L)/2L])))

eve_clst_tau <- group_nest(eve_tbl, Timeline) %>% 
    mutate("cs_grp" = map(data, lm_cs, model_type = "grp")
           ) 

# select a part of time line for tidiness, too may time periods to display at one page 
qtr_term <- as.character(c(-3:3))

tbl_cs <- list(filter(eve_clst_tau, Timeline %in% qtr_term) %>% pull(cs_grp),
               lm_cs(filter(eve_tbl, Timeline %in% qtr_term), model_type = "tlm")
               ) %>% flatten()

tex_cs <- mod_texreg(tbl_cs, tex = TRUE,
           no.margin = TRUE, scalebox = 0.8, # sideways = TRUE,
           custom.header = list("Abnormal Return (t)" = 1:length(tbl_cs)),
           custom.model.names = c(str_c("tau = ", qtr_term), "inter-timeline")
           )

for (i in seq_len(length(qtr_term)-1)) {
    tex_cs <- gsub(x = tex_cs,
        pattern = c("Timeline.L", "Timeline.Q", "Timeline.C", 
            "Timeline[:$:][:^:]4[:$:]", "Timeline[:$:][:^:]5[:$:]", "Timeline[:^:]6")[i],
        replacement = str_c("tau = ", qtr_term)[c(1:3,5:7)][i])
}


# model visualization -----------------------------------------------------
    ## plot the estimate value in path and point form
    plot_tau_inter <- function(df, str_grep, 
                               ylim1 = "estimate", ylim2 = "statistic") {
   
        df_est <- df %>% `[`(grep(str_grep, .$term), ) %>% 
            mutate("tau" = timeline[-(TS+1L)/2L]) 

        df_est_esti <- ggplot(df_est, aes(x = tau, y = get(ylim1))) +
            geom_path() + 
            geom_point() +
            labs(y = ylim1, title = str_grep) +
            geom_ref_line(h = 0, colour = "grey") +
            theme_bw()
    
        df_est_stat <- ggplot(df_est, aes(x = tau, y = get(ylim2))) +
            geom_path() + 
            geom_point() +
            labs(y = ylim2) +
            geom_ref_line(h = +1.96, colour = "grey") +
            geom_ref_line(h = -1.96, colour = "grey") +
            theme_bw()
        
        return(list(df_est_esti, df_est_stat))
    
    }
    
# AMD-timeline
cs_tlm <- broom::tidy(lm_cs(eve_tbl, model_type = "tlm")) 

cs_tlm_tau_inter <- plot_tau_inter(cs_tlm,
               str_grep = "^Timeline([:^:]\\d+|\\.[LQC])")
    
multi_panel_figure(width = 160, height = 180, columns = 1, rows = 2) %>% 
    fill_panel(cs_tlm_tau_inter[[1]], column = 1, row = 1) %>% 
    fill_panel(cs_tlm_tau_inter[[2]], column = 1, row = 2) %>% 
        save_multi_panel_figure(dpi = 300, limitsize = F,
                glue("{datdir}/{Accprd}_Figure_path-intercept.pdf"))

cs_tlm_tau_AMD <- plot_tau_inter(cs_tlm,
               str_grep = "^AMD[:_:]t[:::]Timeline([:^:]\\d+|\\.[LQC])")

multi_panel_figure(width = 160, height = 180, columns = 1, rows = 2) %>% 
    fill_panel(cs_tlm_tau_AMD[[1]], column = 1, row = 1) %>% 
    fill_panel(cs_tlm_tau_AMD[[2]], column = 1, row = 2) %>% 
        save_multi_panel_figure(dpi = 300, limitsize = F, 
                filename = glue("{datdir}/{Accprd}_Figure_path-AMD-tau.pdf"))

# AMD-timeline-group
cs_gt <- broom::tidy(lm_cs(eve_tbl, model_type = "gt")) 

cs_tlm_tau_grp <- plot_tau_inter(cs_gt,
               str_grep = "^AMD[:_:]t[:::]Timeline([:^:]\\d+|\\.[LQC])")

multi_panel_figure(width = 160, height = 180, columns = 1, rows = 2) %>% 
    fill_panel(cs_tlm_tau_grp[[1]], column = 1, row = 1) %>% 
    fill_panel(cs_tlm_tau_grp[[2]], column = 1, row = 2) %>% 
        save_multi_panel_figure(dpi = 300, limitsize = F,
                glue("{datdir}/{Accprd}_Figure_path-tau-grp.pdf"))

gt_term <- c("term", "estimate", "std.error", "statistic", "p.value")
cs_gt[grep("^AMD[:_:]t[:::]Timeline([:^:]\\d+|\\.[LQC])", cs_gt$term, invert = TRUE), ] %>%
    select(gt_term) %>% 
    DT::datatable(rownames = FALSE) %>% 
    DT::formatRound(columns = gt_term[-1], digits = 3)

