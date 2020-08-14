# This script is written for structuring the event window and estimate window
# nearby the disclosure date of earnings report under framework of event study. 

library(tidyverse)
library(lubridate)
library(magrittr)
library(modelr)

library(glue)
library(multipanelfigure)
library(RColorBrewer)
library(ggthemes)

library(DBI)
library(RSQLite)
library(dbplyr)

summarise <- purrr::partial(summarise, .groups = 'drop')

# function to extract daily trading data of a specific time range ====
mod_extract <- function(df, behind, ahead, type) {
    
    annodt <- unique(subset(df, Accper == subAccprd, select = Annodt, drop = T))
    
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
wind_ex <- -160L
wind_bh <- +160L
# # If we want to use the history trading data to contract window,
# # we just need to assign the window index with
# wind_ex <- -280
# wind_bh <- +40
#
# structure the estimate window: 240 days
wind_est_ex <- -40L
wind_est_bh <- +40L
# when we split the trading data around report announcement data into 
# estimate window and event window, we need two parameters to 
# define the time periods range
#
# structure the event window; 61 days
wind_eve_ex <- -30L
wind_eve_bh <- +30L
# Note that we discard ten (two weeks) daily trading data before announcement date
# and ten (two weeks) daily trading data after announcement date


# Part I, set the value of critical parameters ----------------------------

# the kind of factor models we will select
model_type <- "CH4"
# how much accounting periods will be running?
len_term <- 4*5
# the ending date of the first accounting period
start_term <- ymd('2013-03-31')
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


# Part II, importing essential data ----------------------------------------

# setup the working directory 
setwd('~/OneDrive/Data.backup/QEAData/')
# link to database
QEA_db <- dbConnect(RSQLite::SQLite(), "./QEA_db.sqlite")
# import daily trading data of stocks, trddat
# and trading date of China A-share markets, trdday
load(file = './PrePotfol.RData')
# import Fama-French factors (CH3, CH4, or FF5)
load(list.files(path = glue("./{model_type}"),
                pattern =  glue("{model_type}\\.RData$"),
                full.names = TRUE
                )
     )
# factor terms of regression model
if (model_type == "CH3") {
    
        ff_term <- c("mkt_rf", "SMB", "VMG")

} else if (model_type == "CH4") {
    
        ff_term <- c("mkt_rf", "SMB", "VMG", "RMW")

} else if (model_type == "FF5") {

        ff_term <- c("mkt_rf", "SMB", "HML", "RMW", "CMA")

}
ff_factor <- map(potfolreg, "data") %>% 
        # the factors are same among portfolios at every quarter
        map_dfr(`[`(1L)) %>% 
        select(TradingDate, all_of(ff_term))

# the sequence of accounting periods
Accprd <- months(seq(from = 0, by = 3, length = len_term)) %>% 
        mapply('%m+%', start_term, .) %>% 
        base::as.Date(origin = '1970-01-01')
# split by year
Accprd <- split(Accprd, year(Accprd))
# export directory
datdir <- file.path(model_type, names(Accprd))
if (!all(dir.exists(datdir))) mapply(dir.create, datdir)
for (i in seq_along(Accprd)) {
    if (!all(dir.exists(file.path(model_type, names(Accprd)[i], Accprd[[i]])))) 
        mapply(dir.create, file.path(model_type, names(Accprd)[i], Accprd[[i]]))
}

# Import the status data of quarterly financial report 
ReptInfo <- read_delim('./Acc_Quarter/IAR_Rept.txt', 
                       delim = '\t', na = '',
                       col_types = cols_only(
                               Stkcd = col_character(),
                               # 1:4, first quarter, mid of year, third quarter, 
                               # and year earnings report
                               Reptyp = col_factor(levels = c(1:4)),
                               # the deadline of accounting cycle
                               Accper = col_date(format = "%Y-%m-%d"),
                               # the date when report was discolsed
                               Annodt = col_date(format = "%Y-%m-%d"),
                               # the day of week when report was discolsed
                               # c(0:6): Sun < Mon < Tue < Wed < Thu < Fri < Sat
                               Annowk = col_factor(levels = c(0:6)),
                               # net profits and earnings per share
                               Profita = col_double(), Erana = col_double()
                               )
        )
# we could observe that the symbols of stocks belong to China A-Share markets
# are begin with number c(0, 3, 6),
# so we use regular expression and string function 'grepl' to filter others 
if (nrow(problems(ReptInfo)) >= 0L) {
        ReptInfo %<>% `[`(-unique(problems(.)$row), ) %>% 
                filter(grepl('^[0-6]', Stkcd)) %>% 
                arrange(Stkcd, Accper)
}


# structure the sub-sample according to pre-earnings report ==== 
# ForecFinReportType - Nine categories including turn to loss, continued loss, 
# turn to gain profit from loss, continued profitability, large increase,
# large decrease, slight increase, slight decrease, and uncertainty
PreRept <- read_delim('./Acc_Quarter/FIN_F_ForecFin.txt', 
                      delim = '\t', na = '', 
                      col_types = cols_only(
                              # the symbol of stocks, need to be rename
                              StockCode = col_character(),
                              PubliDate = col_date(format = "%Y-%m-%d"),
                              AccPeriod = col_date(format = "%Y-%m-%d"),
                              # 0 = performance advance expose, 1 = regular disclosure
                              Source = col_factor(),
                              # the categories of report source
                              ForecFinReportType = col_factor()
                              )
        ) %>% 
        rename('Stkcd' = StockCode) %>% 
        filter(grepl('^[0-6]', Stkcd))

# the color of category data used for plotting
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual', ]
col_vector <- unlist(mapply(brewer.pal, 
                            qual_col_pals$maxcolors, 
                            rownames(qual_col_pals)
                            )
                     )


# Part III, contract window trading data --------------------------------

for (y in seq_along(Accprd)) {  # loop in year

    for (q in seq_along(Accprd[[y]])) {  # loop in quarter
        
        # choose a given quarter
        subAccprd <- Accprd[[y]][q] %>% print()
        setwd(file.path('~/OneDrive/Data.backup/QEAData/', datdir[y], subAccprd))
        
        subReptInfo <- filter(ReptInfo, Accper == subAccprd) 
        subPreRept <- filter(PreRept, AccPeriod == subAccprd)
          
        # select the stocks that had published earnings report at currently quarter
        trdwin <- filter(trddat, Stkcd %in% pull(subReptInfo, Stkcd))
        
        # for saving memory and speed up the tidy process, we only reserve the 
        # trading data in recent two years before and after calendar accounting date
        trdwin %<>% mutate(data = map(data, filter, 
                TradingDate %within% interval(subAccprd %m+% months(-11L), subAccprd %m+% months(+13L))
                                      )
                ) %>% 
                unnest(cols = "data") %>% 
                split(.$Stkcd)
        
        # eliminating the stocks which date trading records is lesser than 2*12*15 
        # note that there is not the before and after at report announcement date
        trdwin %<>% map(~ ifelse(nrow(.x) <= 360L, return(NULL), return(.x))) %>% 
                compact()
        
        # Merge the trading data, fama-french factors and earnings report
        trdwin %<>% lapply(inner_join, ff_factor, by = "TradingDate") %>% 
                lapply(inner_join, Nrrate, by = "TradingDate") %>% 
                lapply(inner_join, subReptInfo, by = "Stkcd") %>% 
                # calculate the column that daily return minus risk free return
                lapply(mutate, 'Dret_rf' = Dretnd - Nrrdaydt) 
    
        
        ###### Matching the time-line using self-function mod_extract #######
        
        # construct the total times window, including estimate and event window 
        trdwin %<>% lapply(mod_extract, wind_ex, wind_bh, "inner") %>% compact()
        
        # confirm the length of time series data have not existed error and 
        # aren't NaN value in data frame of stocks
        if ( any(`==`(map_dbl(trdwin, nrow), wind_bh - wind_ex + 1L)) ) {

                trdwin %<>% map(
                        ~ ifelse("=="(rowSums(is.na( .x )), 0L) %>% any(), 
                                 return(select(.x, Stkcd, TradingDate, Dretnd, Dsmvosd, 
                                               Dret_rf, all_of(ff_term), Nrrdaydt,
                                               Accper, Annodt, Annowk, Markettype, Indus, Listdt
                                               )
                                        ),
                                 return(NULL) 
                                 )
                        ) %>% 
                        compact() 
        
        } else stop(glue("{Accprd[[y]][q]}, The time period of the whole window exist errors!"))
        
        # filter by market type ====
        if (Markettype %in% c(1L, 4L, 16L)) {
            
                trdwin %<>% lapply(filter, Markettype == Markettype) 
            
        } else if (Markettype == 5L) {
    
                trdwin %<>% lapply(filter, Markettype %in% c(1,4)) 
            
        }
        
        # stocks whether had published ex-report or not ====
        if (Pre_type %in% c(0L, 1L)) {
            
                trdwin %<>% `[`(pull(filter(subPreRept, Source == Pre_type), Stkcd))
                
        } else if (Pre_type == 2L) {
    
                trdwin %<>% `[`(pull(filter(subPreRept, Source %in% c(0, 1)), Stkcd))
                
        } else if (Pre_type == 3L) {
            
                trdwin %<>% `[`(!names(.) %in% pull(subPreRept, Source))
            
        } else if (Pre_type == 4L) {
     
                trdwin %<>% `[`(!(names(.) %in% pull(filter(subPreRept, Source == "0"), Stkcd))
                                )
                
        } else if (Pre_type == 5L) {
            
                trdwin %<>% `[`(!(names(.) %in% pull(filter(subPreRept, Source == "1"), Stkcd))
                                )
                
        }
        
        # filter by industry ====
        
        # leave to develop in future
        
    
                
        # The stocks in our sample
        stkcd <- names(trdwin)
        
        # export the symbols of stocks as a csv file
        glue("{subAccprd}_{Pre_type}_{Markettype}_stkcd.csv") %>%
                write.csv(list("Stkcd" = stkcd), file = ., quote = FALSE, row.names = F)
        
        ## construct the estimate and event window =====
        
        # structure the estimation window
        trdest <- lapply(trdwin, mod_extract, wind_est_ex, wind_est_bh, "outer") 
        
        wind_est_len <- ((wind_bh - wind_ex) - (wind_est_bh - wind_est_ex))
        
        if ( any(map_dbl(trdest, nrow) == wind_est_len) ) {
            
                glue("{subAccprd}_{Pre_type}_{Markettype}_{model_type}_stkest.csv") %>%
                        write_csv(bind_rows(trdest), path = .)
              
                # saving the stock symbols and date sequence index as a csv file,
                # it will be used in MATLAB, PLS(su, 2016) 
                tibble("Stkcd" = rep(1:length(stkcd), each = wind_est_len),
                       "day" = rep(1:wind_est_len, times = length(stkcd))
                       ) %>% 
                write_csv(path = paste(subAccprd, Pre_type, Markettype, 
                                       "index_MATLAB.csv",
                                       sep = "_"
                                       )
                          )
                
        } else stop(glue("{Accprd[[y]][q]}, errors existed in the estimate-window!"))
        
        # structure the event window
        trdeve <- lapply(trdwin, mod_extract, wind_eve_ex, wind_eve_bh, "inner")
        
        wind_eve_len <- wind_eve_bh - wind_eve_ex + 1L
        
        if ( any(map_dbl(trdeve, nrow) == wind_eve_len) ) {
            
                glue("{subAccprd}_{Pre_type}_{Markettype}_{model_type}_stkeve.csv") %>%
                        write_csv(bind_rows(trdeve), path = .)
            
        } else stop(glue("{Accprd[[y]][q]}, errors existed in the event-window!"))
    
    
        
        # Part IV, data visualization -----------------------------------------
        
        # cluster the stocks based on circulated market value
        g.stk <- group_by(bind_rows(trdeve), Stkcd) %>% 
                summarise("avgDsmvosd" = mean(Dsmvosd)) %>%
                mutate("g.Size" = cut(avgDsmvosd, 
                                      quantile(avgDsmvosd, c(0, 0.5, 1)), 
                                      labels = c("Small", "Big"), 
                                      include.lowest = TRUE
                                      )
                       )
        
        g.Dret <- lapply(trdeve, inner_join, g.stk, by = c("Stkcd")) %>% 
                map_dfr(add_column, "timeline" = c(wind_eve_ex:wind_eve_bh)) %>% 
                mutate("Markettype" = factor(Markettype, 
                                             levels = c("1", "4", "16"),
                                             labels = c("Shanghai A Market", 
                                                        "Shenzhen A Market", 
                                                        "Growth Enterprise Market"
                                                        )
                                             )
                       )
        
        # compare the average returns among small and big portfolios by markets ====
        
        # average daily returns grouped by Market type and time line
        figure_Dret <- group_by(g.Dret, timeline, g.Size) %>% 
                summarise("avgDret" = Dretnd %*% (avgDsmvosd / sum(avgDsmvosd))) %>% 
                    ggplot() +
                        geom_path(aes(x = timeline, y = avgDret, colour = g.Size)) +
                            labs(y = "Average return of potfolio different in Size",
        title = "The path of average daily returns around earnings report announcement date",
        subtitle = glue("from {(Accprd[[y]][q]+days(1))%m+%months(-3)} to {Accprd[[y]][q]}")
                                 ) +
                            theme_bw() +
                            theme(legend.position = "bottom",
                                  legend.title = element_blank()
                                  )
        
        figure_Dret_Market <- group_by(g.Dret, timeline, Markettype, g.Size) %>% 
                summarise("avgDret" = Dretnd %*% (avgDsmvosd / sum(avgDsmvosd))) %>% 
                    ggplot() +
                        geom_path(aes(x = timeline, y = avgDret, colour = g.Size)) +
                            facet_wrap(~ Markettype, nrow = 3) +
                                labs(title = "Facets by market type",
                                     y = "Average return of potfolio different in Size") +
                                theme_bw() +
                                theme(legend.position = "none")
    
        # fill and save the result figure
        multi_panel_figure(width = 240, height = 270, columns = 1, rows = 3) %>% 
            fill_panel(figure_Dret, column = 1, row = 1) %>%
            fill_panel(figure_Dret_Market, column = 1, row = 2:3) %>% 
                save_multi_panel_figure(
                    filename = glue("{Accprd[[y]][q]}_{Pre_type}_{Markettype}_Retrun_Market.pdf")
                    )
            
        
        # compare the average return around announcement date among industries ====
        group_by(g.Dret, timeline, "Indus" = str_sub(Indus, 1L, 1L)) %>% 
                summarise("avgDret" = Dretnd %*% (avgDsmvosd / sum(avgDsmvosd))) %>% 
                    ggplot() +
                        geom_path(aes(x = timeline, y = avgDret, color = Indus)) +
                            labs(y = "Average return",
        title = "Average returns of stocks around announcement date across industries"
                                 ) +
                            scale_color_manual(values = col_vector) +
                            theme_economist() +
                            theme(legend.position = "bottom")
        
        ggsave(glue("{Accprd[[y]][q]}_{Pre_type}_{Markettype}_Retrun_Industry.pdf"),
               width = 16, height = 9
               )
            
    }

}

rm(list = setdiff(ls(), c("Accprd", "model_type"))); gc()

# Path of stock returns when earnings report are released ====

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
    file_cd <- dir(pattern = 'stkeve\\.csv$', 
                   path = file.path('~/OneDrive/Data.backup/QEAData/',
                                    model_type, names(Accprd)[y]
                                    ), 
                   recursive = T,
                   full.names = TRUE 
                   )
        
    Dret_stk_eve <- lapply(file_cd, read_csv, na = "") %>% 
            `names<-`(Accprd[[y]])
    
    # calculate the average daily returns of all stocks by tau 
    Avg_Dret <- Dret_stk_eve %>% 
            map(~ split(.x, .x$Stkcd) %>% add_tau_stk()
                ) %>% 
            map_dfr(~ summarise(group_by(.x, tau), 
                                "avg_Dret" =  Dretnd %*% (Dsmvosd / sum(Dsmvosd)),
                                .groups = "drop"
                                ), 
                    .id = "quarter"
                    ) %>% 
            mutate("tau" = as.integer(as.character(tau)))
    
    title_char <- paste("Path of stock returns when earnings report are released",
                        names(Accprd)[y],
                        sep = ", "
                        )
    
    ggplot(data = Avg_Dret, mapping = aes(x = tau, y = avg_Dret)) +
        geom_line() + 
        geom_point() +
        geom_ref_line(h = 0, colour = "grey") +
            labs(y = "Average daily return", x = "Time line",
                 title = title_char
                 ) +
            scale_x_continuous(breaks = seq(-30, 30, by = 5)) + 
            facet_wrap(facets =  ~ quarter) +
            theme_bw() +
            theme(legend.position = "none")
        
    ggsave(filename = paste(names(Accprd)[y], "avgDret_around_tau.pdf", sep = "_"),
           width = 16, height = 9, 
           path = file.path('~/OneDrive/Data.backup/QEAData/',
                            model_type, names(Accprd)[y]
                            )
           )

}
