library(magrittr)
library(tidyverse)
library(lubridate)
library(glue)
library(multipanelfigure)
library(grid)


# Part I, Preparation =====

# establish a function to extract daily trading data of specific time range
mod_extract <- function(x,y,z,h){
  annodt <- x[which(pull(x, Accper) == subAccprd), ]$Annodt %>% unique()
  while(!annodt %in% trdday) {annodt <- annodt + days(1L)}
  row.n <- which( pull(x, TradingDate) == annodt )
  if( length(row.n) == 1L ){
    if( "&"( "+"(row.n, y) > 0L, row.n + z <= nrow(x)) ){
      if( h == "inner" ) {
        return(x[(row.n + y):(row.n + z), ])
      } else if( h == "outer" ) { # there mybe exist a bug, but I am lazy to repair it.
        # because this function is enough to extract our desired trading data around
        # a announcement date of quartely earnings report
        return(x[c(1:(row.n + y - 1L), (row.n + z + 1L):nrow(x)), ])
      } else print("Please input the parameter of window type!")
    } else {
      glue("{unique(x$Stkcd)}, time periods aren't enough!") %>% print()
      return(NULL)
    } 
  } else {
    glue("{unique(x$Stkcd)}, trading status exist anomalous situation.") %>% print()
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
Accprd <- months(seq(0, by=3, length=4)) %>% 
  mapply('%m+%', ymd('2017-03-31'), .) %>% 
  as.Date(origin='1970-01-01') 
## 03-31, the first quarter; 06-30, the second quarter 
## 09-30, the third quarter; 12-31, the fourth quarter 
## for example, 2018-12-31 meanings that
## we focus on the fourth quarter of year 2018
# export directory, must be one year at there
datdir <- str_sub(Accprd, start = 1L, end = 4L) %>% unique() %>% 
  file.path(getwd(), "CH3", .)
if(!dir.exists(datdir)) dir.create(datdir)

# 1=SHA, 4=SZA, 16=startup
# 5=1+4
# 21=1+4+16
M <- c(21L) 

# Import the aggegate trading data and trading date of China A-share markets,
# and alos earnings report information
load(paste0(datdir,'PrePotfol.RData'))
load(paste0(datdir,'HuCH3.RData'))
# for saving computer memory, we just select the quarter that our desired 
potfolstk %<>% lapply(tibble) %>% `[`(as.character(Accprd))
# extract the CH3 factor ====
CH3_factor <- map(potfolreg, "data") %>% flatten() %>% bind_rows() %>% 
  select(-Ret, -Nrrdaydt) %>% unique.data.frame()
# structure the sub-sample according to pre-earnings report ==== 
PreRept <- read_delim('FIN_F_ForecFin.csv', delim='\t', na = '',
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
    subAccprd <- Accprd[q]
    subReptInfo <- filter(ReptInfo, Accper == subAccprd) 
    subPreRept  <- filter(PreRept, AccPeriod == subAccprd)
      
    # only the stocks that published currently quarter reanings report is our aim
    trddatsam <- trddat[intersect(names(trddat), pull(subReptInfo, Stkcd))]
    
    # At here, we set a eliminating standrad:
    # the stocks which date trading records is lesser than 24*15 within two years
    # before and after report announcement date.
    # Doing this data cleaing strategy could save memory of our computer and speed up the merge process,
    # Of course, this filter conduct wouldn't affect our result
    trddatsam %<>% lapply(filter,
                          TradingDate %within%
                          interval(subAccprd %m+% months(-11L), subAccprd %m+% months(+13L))) %>% 
                   lapply(function(x) ifelse(nrow(x) <= 360L, return(NULL), return(x))) %>% 
                   compact()
    
    # Merge the trading data, fama-french factors and earnings report
    trddatsam %<>% lapply(inner_join, CH3_factor, by = "TradingDate") %>% 
      lapply(inner_join, subReptInfo, by = "Stkcd") %>% 
      # calculate the column that daily return minus risk free return
      lapply(mutate, 'Dret_rf' = Dretnd - Nrrdaydt) 

    
    ###### Matching the time-line using self-function mod_extract #######
    
    # this process is the most important part of this script！
    trddatwin <- lapply(trddatsam, mod_extract, y = exwind, z = bhwind, h = "inner") %>% compact()
    
    # confirm the number of time periods has not error in stock-dimensionality
    trddatwin %<>% lapply(function(x) { # this process is necessary, because exist this situation,
        # althrough the max number of a stock's winow length is bigger than than row.n,
        # but we process the while sentence, it's said that due to non-trading is frequent 
        # at curennt quarter, and cause that "row.n + z > rnow(x)". 
        # This situation present as some NULL rows appear at the end rows of a stock tading data
        ifelse(is.na(x) %>% rowSums() %>% "!="(0) %>% any(), return(NULL), return(x))
      }) %>% compact()
    
    # stocks whether had published ex-report or not
    for (P in Pretype) { 
      
        if(P %in% c(0L, 1L)) {
            Prestk <- filter(subPreRept, Source == P) %>% pull(Stkcd)
            trddatwin %<>% `[`(Prestk)
        } else if (P == 2L) {
            Prestk <- filter(subPreRept, Source %in% c(0,1)) %>% pull(Stkcd)
            trddatwin %<>% `[`(Prestk)
        } else if (P == 3L) {
            trddatwin %<>% `[`(! names(.) %in% pull(subPreRept, Source))
        } else if (P == 4L) {
            Prestk <- filter(subPreRept, Source == "0") %>% pull(Stkcd)
            trddatwin %<>% `[`(! names(.) %in% Prestk)
        } else if(P == 5L) {
            Prestk <- filter(subPreRept, Source == "1") %>% pull(Stkcd)
            trddatwin %<>% `[`(! names(.) %in% Prestk)
        }
        
      
        # filter by markettype =====
        stktrd <- bind_rows(trddatwin); summary(stktrd)
        if (M %in% c(1L, 4L, 16L)) {
            stktrd %<>% filter(Markettype == M) 
        } else if (M == 5L) {
            stktrd %<>% filter(Markettype %in% c(1,4)) 
        }

        
        # Before export the result, we should confirmed the time periods not existed errors
        TS <- stktrd %>% count(Stkcd) %>% pull(n)
        if ( any( TS = (bhwind - exwind +1L)) ) {
          print("Congratulations! The time period of whole window data is correct!")
          ## Then we generate a list of stocks in our sample
          stkcd <- tibble("stkcd" = stktrd %>% count(Stkcd) %>% pull(Stkcd))
              # export the simple codes of stocks in sample from current quarter report
              glue("{datdir}/{subAccprd}_{P}_{M}_stkcd.csv") %>%
                  write.csv(stkcd, file=., quote = F, row.names = F)
              # # export the whole trading data of stocks in sample from current quarter report 
              # glue("{datdir}{subAccprd}_{P}_{M}_stktrd.csv") %>%
              #     write.csv(stktrd, file=., quote=F, row.names = F)
        } else {glue("{Accprd[q]}, Terrible! The time period of whole window exist errors!") %>% print()}
        
        
        ## structure and export the window data  =====
        
        # structure the estimation window: 240 days
        trddatest <- lapply(trddatwin, mod_extract, exwindest, bhwindest, "outer") 
        # export on condition
        TS.est <- bind_rows(trddatest) %>% count(Stkcd) %>% pull(n)
        winlenest <- ((bhwind - exwind) - (bhwindest - exwindest))
        if(any( TS.est == winlenest )) {
            glue("{datdir}/{subAccprd}_{P}_{M}_stkest.csv") %>%
                write.csv(bind_rows(trddatest), file=., quote=F, row.names = F)
          
            ## export the stock symbols and date index to be used in MATLAB for process PLS (su, 2016) 
          
            MATdex <- tibble("Stkcd" = rep(1:nrow(stkcd), each = winlenest),
                             "day" = rep(1:winlenest, times=nrow(stkcd)))
            
            glue("{datdir}/{subAccprd}_{P}_{M}_stkest_MATLABindex.csv") %>%
                    write.csv(MATdex, file=., quote=F, row.names = F)
            
          } else {glue("{Accprd[q]}, Terrible! The time period of estimate-window exist errors!") %>% print()}
        
        # structure the event window: 81 days
        trddateve <- lapply(trddatwin, mod_extract, exwindeve, bhwindeve, "inner")
        # export on condition
        TS.eve <- bind_rows(trddateve) %>% count(Stkcd) %>% pull(n) 
        winleneve <- bhwindeve - exwindeve + 1L
        if(any( TS.eve == winleneve )) {
            glue("{datdir}/{subAccprd}_{P}_{M}_stkeve.csv") %>%
              write.csv(bind_rows(trddateve), file=., quote=F, row.names = F)
          } else {glue("{Accprd[q]}, Terrible! The time period of event-window exist errors!") %>% print()}

        
        ########### data-visualizaion ##########
        # the path of average-returns around report announcement date
        # group the stocks based on circirculated market value 
        figure.Dret <- multi_panel_figure(
          width = 16, height = 18, unit = "in",
          columns = 1, rows = 2)
        
        # Pretidy the data
        g.stk <- bind_rows(trddateve) %>%  group_by(Stkcd) %>% 
          summarise(avgDsmvosd = mean(Dsmvosd)) %>%
          mutate(g.Size = cut(avgDsmvosd, quantile(avgDsmvosd, c(0, 0.3, 0.7, 1)), 
                              labels=c("Small", "Middle", "Big"))) %>% na.omit()
        
        g.Dret <- trddateve %>% `[`(pull(g.stk, Stkcd)) %>% 
          lapply(inner_join, g.stk, by = c("Stkcd")) %>% 
          lapply(add_column, "timeline" = c(exwindeve:bhwindeve)) %>% 
          bind_rows() %>% group_by(timeline, Markettype, g.Size) %>% 
          summarise("avgDret" = mean(Dretnd))
        
        # average daily returns grouped by Markettype and timeline
        figure.ug.Dret <- ggplot(g.Dret) +
          geom_path(aes(x = timeline, y= avgDret, colour = g.Size)) +
          labs(title = glue("The path of average daily returns around earnings report announcement date, 
                       from {(Accprd[q]+days(1))%m+%months(-3)} to {Accprd[q]} in China A-share markets."),
               y = "Average return of potfolio different in Size") +
          theme_bw()
        
        figure.g.Dret <- ggplot(g.Dret) +
          geom_path(aes(x = timeline, y= avgDret, colour = g.Size)) +
          facet_wrap(~ Markettype, nrow = 1) +
          labs(title = "Facets by market type (1 = Shanghai A, 4 = Shenzhen A，16 = Star-up)",
               y = "Average return of potfolio different in Size") +
          theme_bw()
        
        # fill and save the reslut figure
        figure.Dret %<>% 
          fill_panel(figure.ug.Dret, column = 1, row = 1) %<>%
          fill_panel(figure.g.Dret, column = 1, row = 2) %>% 
          save_multi_panel_figure(glue("{datdir}/{Accprd[q]}_{P}_{M}_figure_MarketRetrun.pdf"),
                                  dpi = 300, limitsize = F)
            
    }
}
