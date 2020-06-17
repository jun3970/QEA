# This script is written for structuring the event window and estimate window
# to calculate the abnormal returns attributed to the disclosure of earnings report
# under the framework of event study. 

library(tidyverse)
library(lubridate)
library(magrittr)
library(modelr)

library(glue)
library(multipanelfigure)
library(RColorBrewer)
library(ggthemes)


# Part I, Preparation -----------------------------------------------------

# the color of category data used for plotting
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- mapply(brewer.pal, 
                     qual_col_pals$maxcolors, rownames(qual_col_pals)
                     ) %>% unlist()

# establish a function to extract daily trading data of specific time range
## there maybe exist a bug at *****, but I am lazy to repair it.
## This function is enough to extract our desired window data 
## around a announcement date of quarterly earnings report
mod_extract <- function(df, behind, ahead, type) {
    annodt <- df[which(pull(df, Accper) == subAccprd), ]$Annodt %>% unique()
    while (!annodt %in% trdday) {annodt <- annodt + days(1L)}
    row_where <- which( pull(df, TradingDate) == annodt )
        if (length(row_where) == 1L) {
            if (`&`(`+`(row_where, behind) > 0L, 
                    row_where + ahead <= nrow(df)) 
                ) { if (type == "inner") {
                        return(df[(row_where + behind):(row_where + ahead), ])
                    } else if (type == "outer") { # *****
                        return(df[c(1:(row_where + behind - 1L), (row_where + ahead + 1L):nrow(df)), ])
                    } else print("Please input the parameter of window type!")
            } else {
                print(glue("{unique(df$Stkcd)}, time periods aren't enough!"))
                return(NULL)
            } 
        } else {
            print(glue("{unique(df$Stkcd)}, trading status exist anomalous situation."))
            return(NULL)
        }
} 


# Assign the window time index ====
exwind <- -160L
bhwind <- +160L 
# Notice: It is about twenty trading dates in a calendar month, 
# and 240 trading dates in a calendar year. 
# So, if we want to take years trading data to estimate model parameter, 
# we need a length of **320+1** time series data.
#
# If we want to use the history trading data to structure window data, alternately,
# we just need to assign the window index with
# exwind <- -280
# bhwind <- +40
#
# structure the estimate window
exwindest <- -40L
bhwindest <- +40L
# when we split the trading data around report announcement data into estimate window
# and event window, we need two parameters to define the time periods range 
#
# structure the event window
exwindeve <- -30L
bhwindeve <- +30L
# Note that we discard ten (two weeks) daily trading data before announcement date
# and ten (two weeks) daily trading data after announcement date.


# setup the working directory 
setwd('~/OneDrive/Data.backup/QEAData/')
## our desired accounting period around quarterly earnings report
len_term <- 4L
start_term <- ymd('2017-03-31')

Accprd <- months(seq(0, by = 3, length = len_term)) %>% 
        mapply('%m+%', start_term, .) %>% 
        as.Date(origin = '1970-01-01') 
## 03-31, the first quarter; 06-30, the second quarter 
## 09-30, the third quarter; 12-31, the fourth quarter 
## for example, 2018-12-31 meanings that
## we focus on the fourth quarter of year 2018

# set up the market type
M <- c(21L) 
# 1=SHA, 4=SZA, 16=start-up
# 5=1+4, 21=1+4+16

# Import the data tidy in previous script
# including daily trading data of stocks, trading date of China A-share markets,
# and Fama-French factors (CH3)
load('./CH3/PrePotfol.RData')
load('./CH3/Hu-CH3.RData')

# export directory, must be one year at there
datdir <- year(Accprd) %>% unique() %>% 
        file.path(getwd(), "CH3", .)
if (!dir.exists(datdir)) dir.create(datdir)

# extract the CH3 factor ====
CH3_factor <- map(potfolreg, "data") %>% 
# the factors are same among portfolios in every quarter, we just need one 
        map(`[`(1L)) %>% 
        bind_rows() %>% 
        select(-ptf_Ret)


# Import the status data of quarterly financial report 
ReptInfo <- read_delim('./Acc_Quarter/IAR_Rept.txt', delim = '\t', na = '',
         col_types = cols_only(Stkcd = col_character(),
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
                arrange(Stkcd, Accper) %>% 
                filter(grepl('^[0-6]', Stkcd)) %>% 
                arrange(Stkcd, Accper)
    }

# structure the sub-sample according to pre-earnings report ==== 
PreRept <- read_delim('./Acc_Quarter/FIN_F_ForecFin.txt', 
                delim = '\t', na = '', 
                col_types = cols_only(
                    # the symbol of stocks, need to be rename
                    StockCode = col_character(),
                    PubliDate = col_date(format = "%Y-%m-%d"),
                    AccPeriod = col_date(format = "%Y-%m-%d"),
                    Source = col_factor(),
                    ForecFinReportType = col_factor()
                )
        ) %>% rename('Stkcd' = StockCode) %>% 
        filter(AccPeriod %in% Accprd) %>% 
        filter(grepl('^[0-6]', Stkcd))

# select stocks whether had published ex-earnings report or not
Pretype <- 6L
## 0, select the companies that released the **performance forecast**
## 1, Select the companies that issued the **Regular Announcement**
## 2, Select the enterprises that has published information (pre-announcement or announcement), 
##    that is, the sum of the two types of enterprises, 0 and 1.
## 3, Select the enterprises that has no previous information release, 
##    that is, remove the 0 and 1 enterprises from the total sample.
## 4, Remove the companies that released the performance forecast in the total sample
## 5, Remove the companies that published regular announcements in the total sample
## If including all of stocks, enter 6 (actually, all numbers except above is OK)



# Part II, extract specific time period trading data ----------------------

for (q in 1:length(Accprd)) {  # loop in accounting period 
    
    # choose a given quarter
    subAccprd <- Accprd[q] %>% print()
    subReptInfo <- filter(ReptInfo, Accper == subAccprd) 
    subPreRept  <- filter(PreRept, AccPeriod == subAccprd)
      
    trdwin <- trddat %>% 
        # select the stocks that had published earnings report at currently quarter
        `[`( intersect(names(.), pull(subReptInfo, Stkcd)) ) %>% 
        # for saving memory and speed up the tidy process, we only reserve the 
        # trading data in recent two years before and after calendar accounting date
        lapply(filter, TradingDate %within%
               interval(subAccprd %m+% months(-11L), subAccprd %m+% months(+13L))
               ) %>%
        # eliminating the stocks which date trading records is lesser than 2*12*15 
        # note that there is not the before and after at report announcement date
        lapply(function(x) {ifelse(nrow(x) <= 360L, return(NULL), return(x))}
               ) %>% 
        compact()
    
    # Merge the trading data, fama-french factors and earnings report
    trdwin %<>% lapply(inner_join, CH3_factor, by = c("TradingDate", "Nrrdaydt")) %>% 
        lapply(inner_join, subReptInfo, by = "Stkcd") %>% 
        # calculate the column that daily return minus risk free return
        lapply(mutate, 'Dret_rf' = Dretnd - Nrrdaydt) 

    
    ###### Matching the time-line using self-function mod_extract #######
    
    # this process is the most important part of this script！
    trdwin %<>% lapply(mod_extract, exwind, bhwind, "inner") %>% compact()
    
    # confirm the number of time periods has not error in stock-dimensional
    ## this process is necessary, because exist this situation,
    ## although the max number of a stock's window length is bigger than than row_where,
    ## but we process the while sentence, it's said that due to non-trading is frequent 
    ## at current quarter, and cause that "row_where + ahead > rnow(df)". 
    trdwin %<>% lapply(function(x) { 
        # The situation state at above present as some NULL rows 
        # appear at the end rows of a stock trading data
        ifelse("!="(rowSums(is.na( x )), 0L) %>% any(), 
               return(NULL), 
               return(select(x, Stkcd, TradingDate, Dretnd, Nrrdaydt, Dsmvosd, 
                             Dret_rf, mkt_rf, SMB, VMG,
                             Accper, Annodt, Annowk, Markettype, Indus, Listdt))
               )
      }) %>% compact() 

    
    # stocks whether had published ex-report or not ====
    if (Pretype %in% c(0L, 1L)) {
        Prestk <- filter(subPreRept, Source == Pretype) %>% pull(Stkcd)
        trdwin %<>% `[`(Prestk)
    } else if (Pretype == 2L) {
        Prestk <- filter(subPreRept, Source %in% c(0,1)) %>% pull(Stkcd)
        trdwin %<>% `[`(Prestk)
    } else if (Pretype == 3L) {
        trdwin %<>% `[`(!names(.) %in% pull(subPreRept, Source))
    } else if (Pretype == 4L) {
        Prestk <- filter(subPreRept, Source == "0") %>% pull(Stkcd)
        trdwin %<>% `[`(!names(.) %in% Prestk)
    } else if (Pretype == 5L) {
        Prestk <- filter(subPreRept, Source == "1") %>% pull(Stkcd)
        trdwin %<>% `[`(!names(.) %in% Prestk)
    }
    
  
    # filter by market type =====
    stktrd <- bind_rows(trdwin); summary(stktrd)
    if (M %in% c(1L, 4L, 16L)) {
        stktrd %<>% filter(Markettype == M) 
    } else if (M == 5L) {
        stktrd %<>% filter(Markettype %in% c(1,4)) 
    }

    
    # Before export the result, we should confirmed the time periods not existed errors
    TS <- stktrd %>% count(Stkcd) %>% pull(n)
    if ( any( TS = (bhwind - exwind + 1L)) ) {
        print("Congratulations! The time period of whole window data is correct!")
        ## Then we generate a list of stocks in our sample
        stkcd <- tibble("stkcd" = stktrd %>% count(Stkcd) %>% pull(Stkcd))
            # export the simple codes of stocks in sample from current quarter report
            glue("{datdir}/{subAccprd}_{Pretype}_{M}_stkcd.csv") %>%
                write.csv(stkcd, file = ., quote = F, row.names = F)
            # # export the whole trading data of stocks in sample from current quarter report
            # # for calculating the factors to explanate abnormal return
            # glue("{datdir}/{subAccprd}_{Pretype}_{M}_stktrd.csv") %>%
            #     write.csv(stktrd, file=., quote=F, row.names = F)
    } else {print( glue("{Accprd[q]}, Terrible! The time period of the whole window exist errors!") )}
    
    
    ## structure and export the window data  =====
    
    # structure the estimation window: 240 days
    trdest <- lapply(trdwin, mod_extract, exwindest, bhwindest, "outer") 
    # export on condition
    TS.est <- bind_rows(trdest) %>% count(Stkcd) %>% pull(n)
    winlenest <- ((bhwind - exwind) - (bhwindest - exwindest))
    if (any( TS.est == winlenest )) {
        glue("{datdir}/{subAccprd}_{Pretype}_{M}_stkest.csv") %>%
            write.csv(bind_rows(trdest), file = ., quote = F, row.names = F)
      
        ## export the stock symbols and date index to be used in MATLAB for process PLS (su, 2016) 
      
        MATdex <- tibble("Stkcd" = rep(1:nrow(stkcd), each = winlenest),
                         "day" = rep(1:winlenest, times = nrow(stkcd)))
        
        glue("{datdir}/{subAccprd}_{Pretype}_{M}_stkest_MATLABindex.csv") %>%
            write.csv(MATdex, file = ., quote = F, row.names = F)
        
      } else {print( glue("{Accprd[q]}, errors existed in the estimate-window!") )}
    
    # structure the event window: 81 days
    trdeve <- lapply(trdwin, mod_extract, exwindeve, bhwindeve, "inner")
    # export on condition
    TS.eve <- bind_rows(trdeve) %>% count(Stkcd) %>% pull(n) 
    winleneve <- bhwindeve - exwindeve + 1L
    if ( any(TS.eve == winleneve) ) {
        glue("{datdir}/{subAccprd}_{Pretype}_{M}_stkeve.csv") %>%
            write.csv(bind_rows(trdeve), file = ., quote = F, row.names = F)
        } else {print( glue("{Accprd[q]}, errors existed in the event-window!") )}

    
    ########### data-visualization ##########
    # the path of average-returns around report announcement date
    
    # cluster the stocks based on circulated market value
    g.stk <- group_by(bind_rows(trdeve), Stkcd) %>% 
        summarise("avgDsmvosd" = mean(Dsmvosd)) %>%
        mutate("g.Size" = cut(avgDsmvosd, quantile(avgDsmvosd, c(0, 0.5, 1)), 
                            labels = c("Small", "Big"), include.lowest = TRUE))
    
    g.Dret <- lapply(trdeve, inner_join, g.stk, by = c("Stkcd")) %>% 
        map_dfr(add_column, "timeline" = c(exwindeve:bhwindeve)) %>% 
        mutate("Markettype" = factor(Markettype, levels = c("1", "4", "16"),
                    labels = c("Shanghai A Market", "Shenzhen A Market", 
                               "Growth Enterprise Market"))
               )
    
    # compare the average returns among small and big portfolios, by market type
    title_char <- paste0("The path of average daily returns ",
        "around earnings report announcement date ",
        glue("from {(Accprd[q]+days(1))%m+%months(-3)} to {Accprd[q]} "),
        "in Chinese A-share markets.")
    
    # average daily returns grouped by Market type and time line
    figure.ug.Dret <- group_by(g.Dret, timeline, g.Size) %>% 
        summarise("avgDret" = Dretnd %*% (avgDsmvosd / sum(avgDsmvosd)) ) %>% 
      ggplot() +
        geom_path(aes(x = timeline, y = avgDret, colour = g.Size)) +
        labs(y = "Average return of potfolio different in Size",
             title = title_char) +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.title = element_blank())
    
    figure.g.Dret <- group_by(g.Dret, timeline, Markettype, g.Size) %>% 
        summarise("avgDret" = Dretnd %*% (avgDsmvosd / sum(avgDsmvosd)) ) %>% 
      ggplot() +
        geom_path(aes(x = timeline, y = avgDret, colour = g.Size)) +
        facet_wrap(~ Markettype, nrow = 3) +
        labs(title = "Facets by market type",
             y = "Average return of potfolio different in Size") +
        theme_bw() +
        theme(legend.position = "none")
    
    # fill and save the result figure
    multi_panel_figure(width = 320, height = 360, columns = 1, rows = 3) %>% 
        fill_panel(figure.ug.Dret, column = 1, row = 1) %>%
        fill_panel(figure.g.Dret, column = 1, row = 2:3) %>% 
        save_multi_panel_figure(dpi = 300, limitsize = F, 
            filename = glue("{datdir}/{Accprd[q]}_{Pretype}_{M}_figure_MarketRetrun.pdf"))
    
    
    # compare the average return around announcement date among industries
    group_by(g.Dret, timeline, "Indus" = str_sub(Indus, 1L, 1L)) %>% 
        summarise("avgDret" = Dretnd %*% (avgDsmvosd / sum(avgDsmvosd)), .groups = 'keep') %>% 
      ggplot() +
        geom_path(aes(x = timeline, y = avgDret, color = Indus)) +
        labs(y = "Average return",
    title = "Average returns of stocks around announcement date across industries"
             ) +
        scale_color_manual(values = col_vector) +
        theme_economist() +
        theme(legend.position = "bottom")
    
    ggsave(glue("{datdir}/{Accprd[q]}_{Pretype}_{M}_figure_IndustryRetrun.pdf"),
        width = 16, height = 9, dpi = 300, units = "in", limitsize = F)
        
}


# Path of stock returns when earnings report are released -----------------
setwd(datdir)
filecd <- file.path(getwd(), dir(pattern = 'stkeve\\.csv$'))

## add the time line - tau
add_tau_stk <- function(df) {
  
    df_stk <- split(df, df$Stkcd)
    timeline_length <- map_dbl(df_stk, nrow) %>% unique()
    
    if (length(timeline_length) == 1) {
  
        df_stk %<>% lapply(arrange, TradingDate) %>% 
                map_dfr(mutate, 
                       "tau" = factor(-((timeline_length-1)/2):((timeline_length-1)/2), 
                                      ordered = TRUE)
                        )
        
    } else stop("Error! The length of time line of stocks are not all equal to a value.")
    
    return(df_stk)
}

## calculate the average daily return (subtracted risk-free interest) of all stocks 
calc_mean_Dret <- function(df) {
    
    group_by(df, tau) %>% 
        summarise("avg_Dret" =  Dretnd %*% (Dsmvosd / sum(Dsmvosd)), .groups = 'keep')
    
}


# import trading data within event window
Dret_stk_eve <- lapply(filecd, read_csv, na = "") %>% 
    `names<-`(str_sub(dir(pattern = 'stkeve\\.csv$'), start = 1L, end = 10L))

Avg_Dret <- lapply(Dret_stk_eve, add_tau_stk) %>% 
    map_dfr(calc_mean_Dret, .id = "quarter") %>% 
    mutate("tau" = as.integer(as.character(tau)))

ggplot(data = Avg_Dret, mapping = aes(x = tau, y = avg_Dret)) +
    geom_line() + 
    geom_point() +
    geom_ref_line(h = 0, colour = "grey") +
    labs(y = "Average daily return", x = "Time line") +
    scale_x_continuous(breaks = seq(-30, 30, by = 5)) + 
    facet_wrap(facets =  ~ quarter) +
    theme_bw() +
    theme(legend.position = "none")

file.path(getwd(), paste(unique(year(Accprd)), "figure", "avgDret.pdf", sep = "_")) %>% 
    ggsave(width = 16, height = 9, dpi = 300, limitsize = F)

