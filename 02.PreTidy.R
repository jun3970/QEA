# This script is writen to extract window trading data 
# undering the framework of event study. 

library(tidyverse)
library(lubridate)
library(magrittr)
library(glue)

library(multipanelfigure)
library(RColorBrewer)
library(ggthemes)


# Part I, Preparation =====
# the color of category data used for plotting
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector <- mapply(brewer.pal, 
                     qual_col_pals$maxcolors, rownames(qual_col_pals)) %>% unlist()

# establish a function to extract daily trading data of specific time range
mod_extract <- function(x,y,z,h){
    annodt <- x[which(pull(x, Accper) == subAccprd), ]$Annodt %>% unique()
    while (!annodt %in% trdday) {annodt <- annodt + days(1L)}
    row.n <- which( pull(x, TradingDate) == annodt )
        if (length(row.n) == 1L ) {
            if ( "&"( "+"(row.n, y) > 0L, row.n + z <= nrow(x)) ) {
                if ( h == "inner" ) {
                    return(x[(row.n + y):(row.n + z), ])
                } else if ( h == "outer" ) { 
                    # there mybe exist a bug, but I am lazy to repair it.
                    # This function is enough to extract our desired window data 
                    # around a announcement date of quartely earnings report
                    return(x[c(1:(row.n + y - 1L), (row.n + z + 1L):nrow(x)), ])
                } else print("Please input the parameter of window type!")
            } else {
                print(glue("{unique(x$Stkcd)}, time periods aren't enough!"))
                return(NULL)
            } 
        } else {
            print(glue("{unique(x$Stkcd)}, trading status exist anomalous situation."))
            return(NULL)
        }
    } 

# Assign the window index 
# Notice: Tt is about twenty trading dates in a calendar month, 
# and 240 trading dates in a calendar year. 
# If we want to take years trading data to estimate model parameter, 
# we need a length of **320+1** time series data.
exwind <- -160L
bhwind <- +160L 
# If we want to use the history trading data to structure window data, alternately,
# we just need to assign the window index with
# exwind <- -280
# bhwind <- -40
# when we split the trading data around report announcement data into estimate window
# and event window, we need two parameters to define the time periods range 
# structure the estimate window
exwindest <- -40L
bhwindest <- +40L
# structure the event window
exwindeve <- -30L
bhwindeve <- +30L
# Note that we discard ten (two weeks) of daily trading data before announcement date
# and ten (two weeks) of daily trading data after announcement date.
# 10 = 40 - 30, That's right! 


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
# 1=SHA, 4=SZA, 16=startup
# 5=1+4, 21=1+4+16

# Import the aggegate trading data and trading date of China A-share markets,
# and alos earnings report information
load('./CH3/PrePotfol.RData')
load('./CH3/Hu-CH3.RData')
# link to SQLite database
QEA_db <- dbConnect(SQLite(), "QEA_db.sqlite")
# export directory, must be one year at there
datdir <- year(Accprd) %>% unique() %>% 
        file.path(getwd(), "CH3", .)
if (!dir.exists(datdir)) dir.create(datdir)

# extract the CH3 factor ====
# the factors are same among portfolios every quarter, we just need one 
CH3_factor <- map(potfolreg, "data") %>% map(`[`(1L)) %>% 
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
# Attention! There are some problems.
ReptInfo %<>% `[`(-problems(.)$row, ) %>% 
        arrange(Stkcd, Accper) %>% 
        # select the stocks which are belong to China A-Share markets,
        # we could observe that the symbols of these stocks are begin with number c(0, 3, 6),
        # so we use regular expression and string function 'grepl' to get them through function filter 
        filter(grepl('^[0-6]', Stkcd)) %>% 
        arrange(Stkcd, Accper)


# structure the sub-sample according to pre-earnings report ==== 
PreRept <- read_delim('./Acc_Quarter/FIN_F_ForecFin.txt', delim = '\t', na = '',
        col_types = cols_only(StockCode = col_character(),
                StockName = col_skip(),
                PubliDate = col_date(format = "%Y-%m-%d"),
                AccPeriod = col_date(format = "%Y-%m-%d"),
                Source = col_factor(),
                ForecFinReportType = col_factor(),
                NumForecFinReport = col_skip(),
                PriAudSitu = col_skip())) %>%
        rename('Stkcd' = StockCode) %>% 
        filter(AccPeriod %in% Accprd) %>% 
        filter(grepl('^[0-6]', Stkcd))
## we select stocks whether had published ex-earnings report or not
## 0, select the companies that released the **performance forecast**
## 1, Select the companies that issued the **Regular Announcement**
## 2, Select the enterprises that has published information (pre-announcement or announcement), 
##    that is, the sum of the two types of enterprises, 0 and 1.
## 3, Select the enterprises that has no previous information release, 
##    that is, remove the 0 and 1 enterprises from the total sample.
## 4, Remove the companies that released the performance forecast in the total sample
## 5, Remove the companies that published regular announcements in the total sample
## If including all of stocks, enter 6 (actually, all numbers except above is OK)
Pretype <- 6L


# Part II, extract specific time period tading data ======
# loop in accounting period 
for (q in 1:length(Accprd)) { 
    
    # choose a given quarter
    subAccprd <- Accprd[q] %>% print()
    subReptInfo <- filter(ReptInfo, Accper == subAccprd) 
    subPreRept  <- filter(PreRept, AccPeriod == subAccprd)
      
    # only the stocks that published currently quarter reanings report is our aim
    trdsam <- trddat[intersect(names(trddat), pull(subReptInfo, Stkcd))]
    
    # At here, we set a eliminating standrad:
    # the stocks which date trading records is lesser than 24*15 within two years
    # before and after report announcement date.
    # Doing this data cleaing strategy could save memory of our computer and speed up the merge process,
    # Of course, this filter conduct wouldn't affect our result
    trdsam %<>% lapply(filter, TradingDate %within%
                    interval(subAccprd %m+% months(-11L), subAccprd %m+% months(+13L))
                    ) %>% 
            lapply(function(x) ifelse(nrow(x) <= 360L, return(NULL), return(x))) %>% 
            compact()
    
    # Merge the trading data, fama-french factors and earnings report
    trdsam %<>% lapply(inner_join, CH3_factor, by = c("TradingDate", "Nrrdaydt")) %>% 
        lapply(inner_join, subReptInfo, by = "Stkcd") %>% 
        # calculate the column that daily return minus risk free return
        lapply(mutate, 'Dret_rf' = Dretnd - Nrrdaydt) 

    
    ###### Matching the time-line using self-function mod_extract #######
    
    # this process is the most important part of this script！
    trdwin <- lapply(trdsam, mod_extract, y = exwind, z = bhwind, h = "inner") %>% compact()
    
    # confirm the number of time periods has not error in stock-dimensionality
    trdwin %<>% lapply(function(x) { # this process is necessary, because exist this situation,
        # althrough the max number of a stock's winow length is bigger than than row.n,
        # but we process the while sentence, it's said that due to non-trading is frequent 
        # at curennt quarter, and cause that "row.n + z > rnow(x)". 
        # This situation present as some NULL rows appear at the end rows of a stock tading data
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
    
  
    # filter by markettype =====
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
    } else glue("{Accprd[q]}, Terrible! The time period of whole window exist errors!") %>% print()
    
    
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
        
      } else {glue("{Accprd[q]}, Terrible! The time period of estimate-window exist errors!") %>% print()}
    
    # structure the event window: 81 days
    trdeve <- lapply(trdwin, mod_extract, exwindeve, bhwindeve, "inner")
    # export on condition
    TS.eve <- bind_rows(trdeve) %>% count(Stkcd) %>% pull(n) 
    winleneve <- bhwindeve - exwindeve + 1L
    if ( any(TS.eve == winleneve) ) {
        glue("{datdir}/{subAccprd}_{Pretype}_{M}_stkeve.csv") %>%
            write.csv(bind_rows(trdeve), file = ., quote = F, row.names = F)
        } else {glue("{Accprd[q]}, Terrible! The time period of event-window exist errors!") %>% print()}

    
    ########### data-visualizaion ##########
    # the path of average-returns around report announcement date
    
    # cluster the stocks based on circirculated market value
    g.stk <- bind_rows(trdeve) %>% group_by(Stkcd) %>% 
        summarise("avgDsmvosd" = mean(Dsmvosd)) %>%
        mutate(g.Size = cut(avgDsmvosd, quantile(avgDsmvosd, c(0, 0.5, 1)), 
                            labels = c("Small", "Big"), include.lowest = TRUE))
    
    g.Dret <- lapply(trdeve, inner_join, g.stk, by = c("Stkcd")) %>% 
        lapply(add_column, "timeline" = c(exwindeve:bhwindeve)) %>% 
        bind_rows() 
    
    g.Dret$Markettype %<>%  fct_relevel("1", "4", "16")
    levels(g.Dret$Markettype) <- c("Shanghai A Market", 
                                   "Shenzhen A Market",
                                   "Growth Enterprise Market")
    
    
    # compare the average returns among small and big portfolios, by market type
    figure.Dret <- multi_panel_figure(
        width = 320, height = 360,
        columns = 1, rows = 3)
    
    # average daily returns grouped by Markettype and timeline
    figure.ug.Dret <- group_by(g.Dret, timeline, g.Size) %>% 
        summarise("avgDret" = mean(Dretnd)) %>% 
      ggplot() +
        geom_path(aes(x = timeline, y = avgDret, colour = g.Size)) +
        labs(title = glue("The path of average daily returns around earnings report announcement date, 
                     from {(Accprd[q]+days(1))%m+%months(-3)} to {Accprd[q]} in China A-share markets."),
             y = "Average return of potfolio different in Size") +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.title = element_blank())
    
    figure.g.Dret <- group_by(g.Dret, timeline, Markettype, g.Size) %>% 
        summarise("avgDret" = mean(Dretnd)) %>% 
      ggplot() +
        geom_path(aes(x = timeline, y = avgDret, colour = g.Size)) +
        facet_wrap(~ Markettype, nrow = 3) +
        labs(title = "Facets by market type",
             y = "Average return of potfolio different in Size") +
        theme_bw() +
        theme(legend.position = "none")
    
    # fill and save the reslut figure
    figure.Dret %<>% 
        fill_panel(figure.ug.Dret, column = 1, row = 1) %<>%
        fill_panel(figure.g.Dret, column = 1, row = 2:3) %>% 
        save_multi_panel_figure(glue("{datdir}/{Accprd[q]}_{Pretype}_{M}_figure_MarketRetrun.pdf"),
                                dpi = 300, limitsize = F)
    
    
    # compare the average return around announcement date among industries
    group_by(g.Dret, timeline, "Indus" = str_sub(Indus, 1L, 1L)) %>% 
        summarise("avgDret" = mean(Dretnd)) %>% 
      ggplot() +
        geom_path(aes(x = timeline, y = avgDret, color = Indus)) +
        labs(title = "Average returns of stocks around announcement date across industries",
             y = "Average return") +
        scale_color_manual(values = col_vector) +
        theme_economist() +
        theme(legend.position = "bottom")
    
    ggsave(glue("{datdir}/{Accprd[q]}_{Pretype}_{M}_figure_IndustryRetrun.pdf"),
        width = 16, height = 9, dpi = 300, units = "in", limitsize = F)
        
    }
