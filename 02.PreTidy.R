# This script is written for structuring the event window and estimate window
# nearby the disclosure date of earnings report under framework of event study. 

library(tidyverse)
library(lubridate)
library(magrittr)
library(modelr)

library(glue)
library(Formula)
library(multipanelfigure)
library(RColorBrewer)
library(ggthemes)

# function to extract daily trading data of a specific time range ====
window_extract <- function(df, type, behind, ahead, peirod = Accprd[[y]][q]) {
    
    annodt <- unique(subset(df, Accper == peirod, select = Annodt, drop = T))
    
    if(length(annodt) != 1L) {
        print(glue("{unique(df$Stkcd)}, The annoncement date isn't a singe value"))
        return(NULL)
    } 
    
    while(!annodt %in% trdday) annodt <- annodt + days(1L)
    
    row_index <- which( pull(df, TradingDate) == annodt )
    
    if(length(row_index) != 1L) {
        print(glue("{unique(df$Stkcd)}, trading status exist anomalous situation."))
        return(NULL)
    }
    
    row_behind <- row_index + behind
    row_ahead <- row_index + ahead
    
    # be sure that all of the row index is including in the data frame 
    if( `|`(row_behind <= 0L, row_ahead > nrow(df)) ) {
        print(glue("{unique(df$Stkcd)}, time periods aren't enough!"))
        return(NULL)
    } 
    
    # the body code of this function
    if (type == "inner") {
        # the length of c(row_behind:row_ahead) is the sum of absolute value 
        # of constant variables behind and ahead plus 1
        return(df[row_behind:row_ahead, ])
    } else if (type == "outer") {
        # Notice the add or minus with number at window starting and ending point
        return(df[c(1:(row_behind-1L), (row_ahead+1L):nrow(df)), ])
    } else print("Please input the parameter of window type!")
} 
# Assign the window time index ====
# It is about twenty trading dates in a calendar month, 
# and 240 trading dates in a calendar year. 
#
# structure the whole window: 321 days
# using data that behind and fore to report announcement date
wind_behind <- -160L
wind_ahead <- +160L
# # If we want to use the history trading data to contract window,
# # we just need to assign the window index with
# wind_behind <- -280
# wind_ahead <- +40
#
# structure the estimate window: 240 days
wind_est_behind <- -40L
wind_est_ahead <- +40L
# when we split the trading data around report announcement data into 
# estimate window and event window, we need two parameters to 
# define the time periods range
#
# structure the event window; 61 days
wind_eve_behind <- -30L
wind_eve_ahead <- +30L
# Note that we discard ten (two weeks) daily trading data before announcement date
# and ten (two weeks) daily trading data after announcement date

# Part I, setup the value of critical parameters ---------------------------
# how much accounting periods will be running?
len_term <- 4*5
# the ending date of the first accounting period
start_term <- ymd('2013-03-31')
# factor model will be running
model_type <- "CH4"
# set up the market class
# 1=SHA, 4=SZA, 16=start-up, 5=1+4, 21=1+4+16
Markettype <- 21L 
# select stocks whether had published ex-earnings report or not
Pre_type <- 6L
## 0, select the companies that released the **performance forecast**
## 1, Select the companies that issued the **Regular Announcement**
## 2, Select the enterprises that has published information, either 
##    pre-announcement or announcement, equal to the sum of the enterprises, 0 and 1.
## 3, Select the enterprises that has no previous information release, 
##    that is, remove the type of 0 and 1 enterprises from the total sample.
## 4, Remove the companies that released the performance forecast in the total sample
## 5, Remove the companies that published regular announcements in the total sample
## If including all of stocks, enter 6 (actually, all numbers except above is OK)

# automatically create directories and import some essential data ====
setwd('~/OneDrive/Data.backup/QEAData/')  # setup the working directory 
# the sequence of accounting periods
Accprd <- months(seq(from = 0, by = 3, length = len_term)) %>% 
        mapply('%m+%', start_term, .) %>% 
        base::as.Date(origin = '1970-01-01') %>% # `[`(.!=ymd("2016-06-30")) %>% 
        split(f = year(.))  # split by year
# create export directory
if (!all(dir.exists(file.path(model_type, names(Accprd))))  # annual
    ) mapply(dir.create, file.path(model_type, names(Accprd)))
for (i in seq_along(Accprd)) {  # quarterly
    if (!all(dir.exists(file.path(model_type, names(Accprd)[i], Accprd[[i]])))) 
        mapply(dir.create, file.path(model_type, names(Accprd)[i], Accprd[[i]]))
}
# import daily trading data of stocks, trddat
# and trading date of China A-share markets, trdday
load(file = './PrePotfol.RData')
# import accounting information from reports
load(file = "./ReportInfo.RData")
# import Fama-French factors data (CH3, CH4, or FF5)
load(list.files(path = file.path(model_type),
                pattern =  glue(".+Fama-French.+{model_type}\\.RData$"),
                full.names = TRUE)
     )
# model type
if (model_type == "CH3") {
    
        ff_term <- c("mkt_rf", "SMB", "VMG")
        model_formula <- Formula(I(Dretnd - Nrrdaydt) ~ mkt_rf + SMB + VMG)

} else if (model_type == "FF3") {
    
        ff_term <- c("mkt_rf", "SMB", "HML")
        model_formula <- Formula(I(Dretnd - Nrrdaydt) ~ mkt_rf + SMB + HML)

} else if (model_type == "CH4") {
    
        ff_term <- c("mkt_rf", "SMB", "VMG", "RMW")
        model_formula <- Formula(I(Dretnd - Nrrdaydt) ~ mkt_rf + SMB + VMG + RMW)

} else if (model_type == "FF4") {
    
        ff_term <- c("mkt_rf", "SMB", "HML", "WML")
        model_formula <- Formula(I(Dretnd - Nrrdaydt) ~ mkt_rf + SMB + HML + WML)

} else if (model_type == "FF5") {

        ff_term <- c("mkt_rf", "SMB", "HML", "RMW", "CMA")
        model_formula <- Formula(I(Dretnd - Nrrdaydt) ~ mkt_rf + SMB + HML + RMW + CMA)
}
# withdraw the data of factors 
# the factors are same among portfolios at every quarter
ff_factor <- map_dfr(potfolreg, ~ .x$data[[1]]) %>% 
        select(TradingDate, all_of(ff_term))
# the color of category data used for plotting
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual', ]
col_vector <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

# Part II, contract estimate window and event window ----------------------
for (y in seq_along(Accprd)) {  # loop in year
  
    for (q in seq_along(Accprd[[y]])) {  # loop in quarter
      
        print(Accprd[[y]][q]) 
        # restore the working directory
        setwd(file.path('~/OneDrive/Data.backup/QEAData/', model_type, 
                        names(Accprd)[y], Accprd[[y]][q])
              )
        # select the stocks that had published earnings report at currently quarter
        trdwin <- filter(trddat, Stkcd %in% pull(filter(ReptInfo, Accper == Accprd[[y]][q]), Stkcd))
        # for saving memory and speed up the tidy process, we only reserve the 
        # trading data in recent two years before and after calendar accounting date
        trdwin %<>% mutate('data' = map(data, filter, TradingDate %within% 
                                        interval(Accprd[[y]][q] %m+% months(-11L), 
                                                 Accprd[[y]][q] %m+% months(+13L))
                                        )
                           ) %>% 
                unnest(cols = "data") %>% 
                split(f = .$Stkcd)
        # eliminating the stocks which date trading records is lesser than 2*12*15 
        # note that there is not the before and after at report announcement date
        trdwin %<>% map(~ ifelse(nrow(.x) <= 360L, return(NULL), return(.x))) %>% compact()
        # Merge the trading data, fama-french factors and earnings report
        trdwin %<>% map(~ inner_join(.x, inner_join(ff_factor, Nrrate, by = "TradingDate"), 
                                     by = "TradingDate") %>% 
                          inner_join(filter(ReptInfo, Accper == Accprd[[y]][q]), 
                                     by = "Stkcd") %>% 
                          # calculate the daily real return
                          mutate('Dret_rf' = Dretnd - Nrrdaydt)
                        ) 
        
        ###### Matching the time-line using self-function window_extract #######
        
        # construct the total times window, including estimate and event window 
        trdwin %<>% lapply(FUN = window_extract, 
                           behind = wind_behind, ahead = wind_ahead, 
                           type = "inner"
                           ) %>% 
                compact()
        # confirm the length of time series data have not existed error and 
        # aren't NaN value in data frame of stocks
        if ( any(`==`(map_dbl(trdwin, nrow), wind_ahead - wind_behind + 1L)) ) {

                trdwin %<>% map(
                        ~ ifelse("=="(rowSums(is.na(.x)), 0L) %>% any(), 
                                 return(select(.x, Stkcd, TradingDate, Dretnd, Dsmvosd, 
                                               Dret_rf, all_of(ff_term), Nrrdaydt,
                                               Accper, Annodt, Annowk, Markettype, Indus, Listdt)
                                        ),
                                 return(NULL) 
                                 )
                        ) %>% 
                        compact() 
        
        } else stop(glue("{Accprd[[y]][q]}, The whole window's period exist errors!"))
        
        # filter by market type ====
        if (Markettype %in% c(1L, 4L, 16L)) {
            
                trdwin %<>% lapply(filter, Markettype == Markettype) 
            
        } else if (Markettype == 5L) {
    
                trdwin %<>% lapply(filter, Markettype %in% c(1,4)) 
        }
        
        # stocks whether had published ex-report or not ====
        if (Pre_type %in% c(0L, 1L)) {
            
                trdwin %<>% `[`(pull(filter(PreRept, AccPeriod == Accprd[[y]][q], Source == Pre_type),
                                     Stkcd)
                                )
                
        } else if (Pre_type == 2L) {
    
                trdwin %<>% `[`(pull(filter(PreRept, AccPeriod == Accprd[[y]][q], Source %in% c(0, 1)), 
                                     Stkcd)
                                )
                
        } else if (Pre_type == 3L) {
            
                trdwin %<>% `[`(!names(.) %in% pull(filter(PreRept, AccPeriod == Accprd[[y]][q]), 
                                                    Stkcd)
                                )
            
        } else if (Pre_type == 4L) {
     
                trdwin %<>% `[`(!(names(.) %in% pull(filter(PreRept, AccPeriod == Accprd[[y]][q], 
                                                            Source == "0"), 
                                                     Stkcd)
                                  )
                                )
                
        } else if (Pre_type == 5L) {
            
                trdwin %<>% `[`(!(names(.) %in% pull(filter(PreRept, AccPeriod == Accprd[[y]][q],
                                                            Source == "1"), 
                                                     Stkcd)
                                  )
                                )
        }
        
        # filter by industry, leave to develop in future ====
          
          
                
        # The stocks in our sample
        stkcd <- names(trdwin)
        # export the symbols of stocks as a csv file
        glue("{Accprd[[y]][q]}_{Pre_type}_{Markettype}_stkcd.csv") %>%
        write.csv(list("Stkcd" = stkcd), file = ., quote = FALSE, row.names = F)
        
        # construct the estimate and event window =====
        # structure the estimation window
        trdest <- lapply(trdwin, 
                         FUN = window_extract, 
                         behind = wind_est_behind, ahead = wind_est_ahead, 
                         type = "outer")
        # the width of window
        wind_est_len <- ((wind_ahead - wind_behind) - (wind_est_ahead - wind_est_behind))
        if ( any(map_dbl(trdest, nrow) == wind_est_len) ) {
            
                glue("{Accprd[[y]][q]}_{Pre_type}_{Markettype}_{model_type}_stkest.csv") %>%
                write_csv(bind_rows(trdest), path = .)
                # saving the stock symbols and date sequence index as a csv file,
                # it will be used in MATLAB, PLS(su, 2016) 
                tibble("Stkcd" = rep(1:length(stkcd), each = wind_est_len),
                       "day" = rep(1:wind_est_len, times = length(stkcd))
                       ) %>% 
                write_csv(path = paste(Accprd[[y]][q], Pre_type, Markettype, 
                                       "index_MATLAB.csv", sep = "_")
                          )
                
        } else stop(glue("{Accprd[[y]][q]}, errors existed in the estimate-window!"))
        
        # structure the event window
        trdeve <- lapply(trdwin, 
                         FUN = window_extract, 
                         behind = wind_eve_behind, ahead = wind_eve_ahead, 
                         type = "inner")
        wind_eve_len <- wind_eve_ahead - wind_eve_behind + 1L
        if ( any(map_dbl(trdeve, nrow) == wind_eve_len) ) {
            
                glue("{Accprd[[y]][q]}_{Pre_type}_{Markettype}_{model_type}_stkeve.csv") %>%
                write_csv(bind_rows(trdeve), path = .)
            
        } else stop(glue("{Accprd[[y]][q]}, errors existed in the event-window!"))
    
        # data visualization ---------------------------------------------
        # cluster the stocks based on circulated market value
        g.stk <- group_by(bind_rows(trdeve), Stkcd) %>% 
                summarise("avgDsmvosd" = mean(Dsmvosd), .groups = 'drop') %>%
                mutate("g.Size" = cut(avgDsmvosd, 
                                      quantile(avgDsmvosd, c(0, 0.5, 1)), 
                                      labels = c("Small", "Big"), 
                                      include.lowest = TRUE)
                       )
        g.Dret <- lapply(trdeve, inner_join, g.stk, by = c("Stkcd")) %>% 
                map_dfr(add_column, "timeline" = c(wind_eve_behind:wind_eve_ahead)) %>% 
                mutate("Markettype" = factor(Markettype, 
                                             levels = c("1", "4", "16"),
                                             labels = c("Shanghai A Market", 
                                                        "Shenzhen A Market", 
                                                        "Growth Enterprise Market")
                                             )
                       )
        # compare the average returns among small and big portfolios by markets ====
        title_char <- paste("The path of average daily returns", 
                            "around earnings report announcement date", sep = ' ')
        subtitle_char <- glue("from {(Accprd[[y]][q]+days(1))%m+%months(-3)} to {Accprd[[y]][q]}")
        # average daily returns grouped by Market type and time line
        figure_Dret <- group_by(g.Dret, timeline, g.Size) %>% 
                summarise("avgDret" = Dretnd %*% (avgDsmvosd / sum(avgDsmvosd)),
                          .groups = 'drop') %>% 
                ggplot() + geom_path(aes(x = timeline, y = avgDret, colour = g.Size)) +
                        labs(y = "Average return of potfolio different in Size",
                             title = title_char, subtitle = subtitle_char) +
                        theme_bw() +
                        theme(legend.position = "bottom", legend.title = element_blank()) 
        figure_Dret_Market <- group_by(g.Dret, timeline, Markettype, g.Size) %>% 
                summarise("avgDret" = Dretnd %*% (avgDsmvosd / sum(avgDsmvosd)),
                          .groups = 'drop') %>% 
                ggplot() + geom_path(aes(x = timeline, y = avgDret, colour = g.Size)) +
                        facet_wrap(~ Markettype, nrow = 3) +
                            labs(y = "Average return of potfolio different in Size",
                                 title = "Facets by market type") +
                            theme_bw() + theme(legend.position = "none")
        # fill and save the result figure
        multi_panel_figure(width = 240, height = 270, columns = 1, rows = 3) %>% 
        fill_panel(figure_Dret, column = 1, row = 1) %>%
        fill_panel(figure_Dret_Market, column = 1, row = 2:3) %>% 
        save_multi_panel_figure(
            filename = paste(Accprd[[y]][q], Pre_type, Markettype, "Retrun_Market.pdf", sep = '_'))
            
        # compare the average return around announcement date among industries ====
        title_char <- "Average returns of stocks around announcement date across industries"
        group_by(g.Dret, timeline, "Indus" = str_sub(Indus, 1L, 1L)) %>% 
        summarise("avgDret" = Dretnd %*% (avgDsmvosd / sum(avgDsmvosd)), .groups = 'drop') %>% 
            ggplot() + geom_path(aes(x = timeline, y = avgDret, color = Indus)) +
                labs(y = "Average return", title = title_char) +
                scale_color_manual(values = col_vector) +
                theme_economist() +
                theme(legend.position = "bottom")
        
        ggsave(glue("{Accprd[[y]][q]}_{Pre_type}_{Markettype}_Retrun_Industry.pdf"),
               width = 16, height = 9)
    }
}

rm(list = setdiff(ls(), c("model_type", "model_formula", 'ff_term', "Pre_type", "Markettype",
                          "Accprd", "trddat", "trdday", "ff_factor", "Nrrate"))
   )
gc()

# Part III, Path of stock returns when earnings report are released -------
summarise <- purrr::partial(summarise, .groups = 'drop')
setwd('~/OneDrive/Data.backup/QEAData/')
# function to add the time line, tau, for every stocks 
add_tau_stk <- function(df) {
  
    timeline_length <- map_dbl(df, nrow) %>% unique()
    
    if (length(timeline_length) == 1L) {
        
        time_line <- seq(-(timeline_length-1)/2, (timeline_length-1)/2, by = 1)
        
        df %<>% map_dfr(~ mutate(arrange(.x, TradingDate), 
                                 "tau" = factor(time_line, ordered = TRUE)
                                 )
                        )
        
    } else stop("Error! The length of time line of stocks are not all equal to a value.")
    
    return(df)
}

for (y in seq_along(Accprd)) { # loop in years
    # import trading data within event window
    Dret_stk_eve <- dir(pattern = 'stkeve\\.csv$', 
                        path = file.path(model_type, names(Accprd)[y]), 
                        recursive = T, full.names = TRUE) %>% 
            lapply(read_csv, na = "") %>% 
            `names<-`(Accprd[[y]])
    # calculate the average daily returns of all stocks by tau 
    Avg_Dret <- Dret_stk_eve %>% 
            map(~ split(.x, .x$Stkcd) %>% 
                  add_tau_stk()
                ) %>% 
            map_dfr(~ group_by(.x, tau) %>% 
                      summarise("avg_Dret" =  Dretnd %*% (Dsmvosd / sum(Dsmvosd))), 
                    .id = "quarter"
                    ) %>% 
            mutate("tau" = as.integer(as.character(tau)))
    
    title_char <- paste("Path of stock returns when earnings report are released",
                        names(Accprd)[y], sep = ", ")
    ggplot(data = Avg_Dret, mapping = aes(x = tau, y = avg_Dret)) +
        geom_line() + 
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1.5) + 
            labs(y = "Average daily return", x = latex2exp::TeX("Timeline ($\\tau$)"),
                 title = title_char) +
            scale_x_continuous(breaks = seq(-30, 30, by = 5)) + 
            facet_wrap(facets =  ~ quarter) +
            theme_bw() +
            theme(legend.position = "none")
        
    ggsave(filename = paste(names(Accprd)[y], "avgDret_around_tau.pdf", sep = "_"),
           width = 16, height = 9, scale = 0.75,
           path = file.path(model_type))
}

# Part IV, Fama–MacBeth regression -----------------------------------------
# This method estimates the betas and risk premia for any risk factors 
# that are expected to determine asset prices. 
# The parameters are estimated in two steps,
# 1. regress each asset against the proposed risk factors 
#    to determine that asset's beta for that risk factor.
# 2. regress all asset returns for a fixed time period 
#    against the estimated betas to determine the risk premium for each factor.

library(DBI)
library(RSQLite)
library(dbplyr)
library(broom)
# link to database
QEA_db <- dbConnect(RSQLite::SQLite(), "./QEA_db.sqlite")
ReptInfo_Acc_EPS <- tbl(QEA_db, "Income_Statement") %>% 
        filter(Typrep == 'A', 
               Accper %in% !!flatten_dbl(Accprd)
               ) %>% 
        select(c(Stkcd, Accper, B003000000)) %>% 
        collect() %>% 
        rename("EPS" = B003000000) %>% 
        mutate(Accper = as.Date(Accper, origin = '1970-01-01'))
dbDisconnect(QEA_db)

Accprd %<>% flatten_dbl() %>% as.Date(origin = '1970-01-01')
stkcd_quarter <- dir(path = file.path(model_type),
                     pattern = glue("{Pre_type}_{Markettype}_stkcd\\.csv$"),
                     recursive = TRUE, full.names = TRUE
                     ) %>% 
        map(read_csv, col_types = cols(Stkcd = col_character())) %>% 
        map(pull, "Stkcd") %>% 
        `names<-`(Accprd)

reg_FM <- vector(mode = 'list', length = length(Accprd)) %>% 
        `names<-`(Accprd) %>% 
        # save the regression data and OLS result
        map(~ vector(mode = "list", length = 2L) %>%  
                set_names(c("FM_1st", "FM_2nd"))  
            )

for (i in seq_along(Accprd)) {
    
    base_date <- Accprd[i] %T>% print()
    while (!base_date %in% trdday) base_date <- base_date + days(-1)
    start_point <- Accprd[i] + days(1) - months(3)
    # first filter the stocks and quarter date 
    # then merge the trading data with factors and risk-free interests
    trddat_quarter_ts <- trddat %>% 
            subset(Stkcd %in% stkcd_quarter[[i]], select = -c(Indus, Listdt)) %>% 
            mutate('data' = map(data, 
                                ~ filter(.x, TradingDate %within% interval(start_point, base_date)) %>% 
                                  inner_join(inner_join(ff_factor, Nrrate, by = "TradingDate"), 
                                             by = "TradingDate")
                                )
                   ) %>% 
            `[`(map_lgl(.$data, ~ nrow(.x) >= 30L), )
    # accounting indicators of stocks at this quarter
    Acc_quarter <- trddat_quarter_ts %>% 
            mutate('data' = map(data, filter, TradingDate == base_date)) %>% 
            unnest(cols = "data") %>% 
            select(c(Stkcd, Clsprc, Dsmvosd)) %>% 
            left_join(filter(ReptInfo_Acc_EPS, Accper == Accprd[i]), by = 'Stkcd') %>% 
            rename('Size' = Dsmvosd) %>% 
            mutate('PE' = Clsprc / EPS) %>% 
            select(-c(Accper, Clsprc))
    # TS, running regression about stock returns on multiple factors 
    reg_FM_1st <- trddat_quarter_ts %>% 
            transmute(Stkcd,
                      "lm_est" = map(data, 
                                     ~ lm(formula = formula(model_formula, lhs = 1), data = .x) %>% 
                                       tidy() %>% 
                                       select(term, estimate)
                                     )
            ) %>% 
            unnest(cols = "lm_est") %>% 
            spread(key = term, value = estimate) 
    
    reg_FM[[i]]$FM_1st <- reg_FM_1st
    
    # join the explained variable `EP` with predictors 
    trddat_quarter_cs <- unnest(trddat_quarter_ts, cols = 'data') %>% 
            select(Stkcd, TradingDate, Dretnd, Nrrdaydt) %>% 
            group_nest(TradingDate) %>% 
            mutate('data' = map(data, 
                                inner_join,
                                inner_join(reg_FM_1st, Acc_quarter, by = "Stkcd"),
                                by = "Stkcd")
                   )
    # CS, running regression about stock returns on estimated coefficients of factors 
    reg_FM_2nd <- trddat_quarter_cs %>% 
            transmute(TradingDate, 
                      'reg_FM_2nd' = map(data, 
                                         ~ lm(formula = formula(model_formula, lhs = 1), data = .x) %>% 
                                           tidy()
                                         )
                      ) %>% 
            unnest(cols = 'reg_FM_2nd') %>% 
            group_by(term) %>% 
            summarise(across(where(is.numeric), mean)) %>% 
            mutate(term = factor(term, levels = c('(Intercept)' ,ff_term))) %>% 
            arrange(term)
    
    reg_FM[[i]]$FM_2nd <- reg_FM_2nd
}

save(reg_FM, file = glue('./{model_type}/Regression_Fama-MacBeth_{model_type}.RData'))
