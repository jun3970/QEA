library(tidyverse)
library(plyr)
library(magrittr)
library(lubridate)
library(timeDate)


# setup the directory of data
datdir <- '~/NutSync/MyData/QEAData/'  # Hu, PC-Carbon
# datdir <- 'D:/NutSync/MyData/QEAData/' # HHY, PC-HP 


# Input the aggregate data of daily trading status of stocks in China Mainland=====
TRD <- read_delim(paste0(datdir, 'TRD_Dalyr_CSMAR.txt'), delim='\t', na = '',
                  col_types = cols(Stkcd = col_character(),
                                   Trddt = col_date(format = '%Y-%m-%d'),
                                   Opnprc= col_number(),
                                   Hiprc = col_number(),
                                   Loprc = col_number(),
                                   Clsprc = col_number(),
                                   Dnshrtrd = col_number(),
                                   Dnvaltrd = col_number(),
                                   Dsmvosd = col_number(),
                                   Dsmvtll = col_number(),
                                   Markettype = col_integer(),
                                   Trdsta = col_integer())) %>%
    rename('TradingDate' = Trddt) %>%  # rename the column of trading date 
    select(c(Stkcd,TradingDate,Opnprc,Hiprc,Loprc,Clsprc,
             Dnshrtrd,Dnvaltrd,Dsmvosd,Dsmvtll,Markettype,Trdsta)) %>% 
    filter(Markettype %in% c(1, 4, 16)) %>%  # Chinese A-Share markets
    arrange(Stkcd, TradingDate) %>% # sorting by stock and trading date
    filter(TradingDate %within% interval('2013-01-01', '2018-12-31'))

# calculate the daily returns 
TRD <- cbind(TRD, Dret=NaN)
for (i in unique(TRD$Stkcd)) {
    Dret <- subset(TRD, Stkcd == i, select = c(Clsprc), drop = TRUE)
    Dret <- c(NaN, log(Dret[-1] / Dret[-length(Dret)], base=exp(1)))
    TRD[which(TRD$Stkcd==i), "Dret"] <- Dret; rm(Dret)
}

if (length(which(is.na(TRD$Dret))) == length(unique(TRD$Stkcd))){
  TRD <- TRD[complete.cases(TRD$Dret),]
} else print(paste('Warning! Daily retruns were calculated with vectorization error.'))


# Input the aggregate data of Fama-French factors
load('C:/Users/Hu/Documents/R/QEA/HuCH3.RData')

FF_factor <- lapply(potfoldat, `[`, ,
                    c('TradingDate', 'mkt_rf', 'SMB', 'VMG', 'Nrrdaydt')) %>% 
  rbind.fill(.)

# FF_factor <- paste0(datdir, 'CUFE_FF-factor_daily.txt') %>%
#         read_delim(delim='\t', na = '',
#              col_types = cols(trddy = col_date('%Y-%m-%d'))) %>% 
#         rename('TradingDate' = trddy) %>%
#         subset(select=c(TradingDate,mkt_rf,smb,hml,umd,rmw,cma,rf))


## Merge ========
TRDFF <- merge.data.frame(TRD, FF_factor, by='TradingDate') %>% 
  arrange(Stkcd, TradingDate) %>% 
  # calculate the column that daily return minus risk free return
  mutate('Dret_rf' = Dret - Nrrdaydt)

         
## announcement date of quarterly financial report ================================
ReptInfo <- dir(datdir, pattern = 'Rept.csv$') %>%
            paste0(datdir, .) %>%
            read_delim(delim='\t', na = '',
                       col_types = cols(Stkcd = col_character(),
                                        Accper = col_date(format = "%Y-%m-%d"),
                                        Annodt = col_date(format = "%Y-%m-%d"),
                                        Annowk = col_integer())) %>%
            subset(select=c(Stkcd,Annodt,Accper,Annowk))  %>% 
            arrange(Stkcd) %>%
            as.data.frame()

# adjust the markettype category in financial report
ReptInfo <- subset(TRDFF, select = c(Stkcd, Markettype)) %>% 
  unique %>% merge(ReptInfo, ., by="Stkcd")


## Stocks whether had published ex-earnings report or not =========================
PreRept <- dir(datdir, pattern = 'ForecFin.csv$') %>%
           paste0(datdir, .) %>%
           read_delim(delim='\t', na = '',
                      col_types = cols(PubliDate = col_date(format = "%Y-%m-%d"),
                                       AccPeriod = col_date(format = "%Y-%m-%d"))) %>%
           rename('Stkcd' = StockCode) %>% 
           select(Stkcd, Source, PubliDate, AccPeriod)
        

# Setup Working day arround QEA ===================================================
TradDay <- dir(datdir, pattern = 'TRD_Cale[.]csv$') %>%
    paste0(datdir, .) %>%
    read_delim(delim='\t', na = '',
                col_types = cols(Markettype = col_integer(),
                                 Daywk = col_integer(),
                                 Clddt = col_date(format = "%Y-%m-%d"),
                                 State = col_factor(levels = c('C', 'O'))))


    ## save the image of aggregate information
    paste0(datdir, 'TRDFF-CH3', '.RData') %>% save.image()

 
# The aggregate data has formed, and then
# we generate the sub-sample in every quarterly. 
# First, we set the classification parameter ====================================


    Pretype <- c(6)
## 0, select the companies that released the **performance forecast**
## 1, Select the companies that issued the **Regular Announcement**
## 2, Select the enterprises that has published information (pre-announcement or announcement), 
##    that is, the sum of the two types of enterprises, 0 and 1.
## 3, Select the enterprises that has no previous information release, 
##    that is, remove the 0 and 1 enterprises from the total sample.
## 4, Remove the companies that released the performance forecast in the total sample
## 5, Remove the companies that published regular announcements in the total sample
## If all stocks are required, enter 6 (actually, all numbers except the above numbers is OK)


    weekterm <- c('wekbind')
# Select the stocks which announcemented on week-day(stkwek) =====================
# weekend-day(stkwnd), or all of them(wekbind, rbind(stkwek, stkwnd))
    

    Accprd <- months(seq(0, by=3, length=4)) %>% 
      mapply('%m+%', ymd('2017-03-31'), .) %>% 
      as.Date(origin='1970-01-01') 
## Accounting period of quarterly earnings report ==================================
## 03-31, the first quarter; 06-30, the second quarter 
## 09-30, the third quarter; 12-31, the fourth quarter 
## for example, 2018-12-31 meanings that
## we concentrated on the fourth quarter of year 2018

    
# change the directory for saving the results of below scripts
datdir <- '~/NutSync/MyData/QEAData/CH3/'

for (A in 1:length(Accprd)) { # loop in accounting period

    subAccprd <- Accprd[A]
    
    # subset of the aggregate data
    TRDFFSAM <- filter(TRDFF, TradingDate %within% 
                       interval(subAccprd %m+% months(-8), subAccprd %m+% months(+10)))
    
    subReptInfo <- filter(ReptInfo, Accper == subAccprd) %>% 
      filter(grepl('^[0-6]', Stkcd))
    
    subPreRept  <- filter(PreRept, AccPeriod == subAccprd) %>% 
      filter(grepl('^[0-6]', Stkcd))
    
    
    ## Matching ====================================================================
    ## this process is the most important port in this program
    exwind <- -160L
    bhwind <- +160L # Notice: we will have length of **260+1** time series data
    stkwek <- data.frame()
    stkwnd <- data.frame()
    stkbrk <- data.frame()
    for (i in 1:nrow(subReptInfo)) {
        QEAgrp <- data.frame()
        stkts <- filter(TRDFFSAM, Stkcd == subReptInfo[i,'Stkcd'])
        if(nrow(stkts) == 0) next  # the firms in report not exist in trading data
        WorkingDay <- subset(TradDay, (Markettype == unique(stkts$Markettype)) & (State == 'O'), 
                             select = c('Clddt'), drop = TRUE)
        if (subReptInfo[i,'Annodt'] %in% WorkingDay) {
            n.row <- which(WorkingDay==subReptInfo[i,'Annodt'])
            QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
            QEAgrp <- filter(stkts, TradingDate %in% QEA.date)
              ifelse(nrow(QEAgrp) == bhwind-exwind +1,
                    {QEAgrp <- cbind(QEAgrp, weekday = c(1)) # one for announcement within weekday
                     stkwek <- rbind(stkwek, QEAgrp)},
                     stkbrk <- rbind(stkbrk, subReptInfo[i,]))
        } else if (wday(subReptInfo[i,'Annodt']) == 7) {
            QEADate <- subReptInfo[i,'Annodt'] + days(2)
            ifelse(QEADate %in% WorkingDay,
                   n.row <- which(WorkingDay==QEADate),
                   stkbrk <- rbind(stkbrk, subReptInfo[i,]))
            QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
            QEAgrp <- filter(stkts, TradingDate %in% QEA.date)
              ifelse(nrow(QEAgrp) == bhwind-exwind +1,
                    {QEAgrp <- cbind(QEAgrp, weekday = c(0)) # zero for announcement on Saturday
                     stkwnd <- rbind(stkwnd, QEAgrp)},
                     stkbrk <- rbind(stkbrk, subReptInfo[i,]))
        } else if (wday(subReptInfo[i,'Annodt']) == 1) {
            QEADate <- subReptInfo[i,'Annodt'] + days(1)
            ifelse(QEADate %in% WorkingDay,
                   n.row <- which(WorkingDay==QEADate),
                   stkbrk <- rbind(stkbrk, subReptInfo[i,]))
            QEA.date <- WorkingDay[c((n.row + exwind):(n.row + bhwind))]
            QEAgrp <- filter(stkts, TradingDate  %in% QEA.date)
              ifelse(nrow(QEAgrp) == bhwind-exwind +1,
                    {QEAgrp <- cbind(QEAgrp, weekday = c(0)) # zero for announcement on Sunday
                     stkwnd <- rbind(stkwnd, QEAgrp)},
                     stkbrk <- rbind(stkbrk, subReptInfo[i,]))
        } else stkbrk <- rbind(stkbrk, subReptInfo[i,])
    }
    rm(QEAgrp, stkts, QEADate, QEA.date, n.row)
    
        ## save the image
        paste0(datdir, subAccprd, '.RData') %>% save.image()
    
    
    for (W in weekterm) { # loop in kinds of announcement day within a week

        if (W == 'weekday') {
            stktrd <- stkwek
        } else if (W == 'weekend') {
            stktrd <- stkwnd
        } else {stktrd <- rbind(stkwek, stkwnd) %>% 
                          arrange(Stkcd)}
      

        for (P in Pretype) { # loop in kinds of stocks whether has published ex-report or not
            
            
            if(P %in% c(0,1)) {
                stktrdp <- data.frame()
                Prestk <- filter(subPreRept, Source == P)$StockCode
                for (i in Prestk) {
                    TRDaln <- filter(stktrd, Stkcd==i)
                    stktrdp <- rbind(stktrdp, TRDaln)
                }
            } else if (P==2) {
                stktrdp <- data.frame()
                Prestk <- filter(subPreRept, Source %in% c(0,1))$StockCode
                for (i in Prestk) {
                    TRDaln <- filter(stktrd, Stkcd==i)
                    stktrdp <- rbind(stktrdp, TRDaln)
                }
            } else if (P == 3) {
                stktrdp <- stktrd
                for (i in subPreRept$StockCode) {
                    stktrdp <- filter(stktrdp, Stkcd!=i)}
            } else if (P == 4) {
                stktrdp <- stktrd
                Prestk <- filter(subPreRept, Source == 0 )$StockCode
                for (i in Prestk) {
                    stktrdp <- filter(stktrdp, Stkcd!=i)
                }
            } else if(P == 5) {
                stktrdp <- stktrd
                Prestk <- filter(subPreRept, Source == 1 )$StockCode
                for (i in Prestk) {
                    stktrdp <- filter(stktrdp, Stkcd!=i)
                }
            } else {stktrdp <- stktrd}
            
          
            ## filter with trading status =====
            Trdstatype <- c(1)
            stktrdp <- filter(stktrdp, Trdsta %in% Trdstatype) %>% 
              subset(select= -Trdsta)
            # 1=正常交易, 2=ST, 3＝*ST, 4＝S(2006年10月9日及之后股改未完成)             
            # 5＝SST, 6＝S*ST, 7=G(2006年10月9日之前已完成股改)
            # 8=GST, 9=G*ST, 10=U(2006年10月9日之前股改未完成)
            # 11=UST, 12=U*ST, 13=N, 14=NST, 15=N*ST, 16=PT       

            
            ## filter with market type
            mkttype <- c(21) 
            # 1=上海A, 4=深圳A, 16=创业板 ===================
            # 5=上海(1)＋深圳(4), 其它数字则为三板总和(建议为 21=1+4+16)
            
            
            for (M in mkttype) {
                
                if (M %in% c(1,4,16)) {
                    stkdat <- filter(stktrdp, Markettype == M) 
                } else if (M == 5) {
                    stkdat <- filter(stktrdp, Markettype %in% c(1,4)) 
                } else {
                    stkdat <- filter(stktrdp, Markettype %in% c(1,4,16)) 
                  }
                
              
                ## After assured the time periods not existed errors,
                ## we generate a symbol list of stocks in simple.
                TSN <- c()
                for (i in unique(stkdat$Stkcd)) {
                    TS <- nrow(filter(stkdat, Stkcd==i))
                    if (TS!=bhwind-exwind+1){
                        stkdat <- filter(stkdat, Stkcd!=i)
                  } else {TSN <- c(TSN,i)}
                }
                
                    paste(subAccprd, P, M, weekterm,'est','stkcd', sep='_') %>%
                      paste0(datdir, ., '.csv') %>%
                        write.csv(TSN, file=., quote=F, row.names = F)
                    
                    paste(subAccprd, P, M, weekterm, 'TradStat', sep='_') %>%
                      paste0(datdir, ., '.csv') %>%
                        write.csv(stkdat, file=., quote=F, row.names = F)
                

                ## Extract window data  ==============================================
                
                for (i in 1:2) {
                  
                    if(i == 1) {  # Output the data of estimation window: 160 days
                      
                        exdate <- 1
                        bhdate <- 120
                        exdate.bh <- 202 # 81 trading days for event window
                        bhdate.bh <- 321

                        stkest <- data.frame()
                        for (i in unique(stkdat$Stkcd)) {
                            stksim <- filter(stkdat, Stkcd==i) %>% 
                                        .[c(c(exdate:bhdate),c(exdate.bh:bhdate.bh)),]
                            stkest <- rbind(stkest, stksim)
                        }
                        rm(stksim)
                    
                  
                            paste(subAccprd, P, M, weekterm, 'est','TradStat', sep='_') %>%
                              paste0(datdir, ., '.csv') %>%
                                write.csv(stkest, file=., quote=F, row.names = F)

                        
                        ## Outout the stock symbols and day index to be used in MATLAB 
                        MATdex <- cbind("Stkcd"=rep(1:length(TSN), 
                                                    each=bhdate-exdate+1+bhdate.bh-exdate.bh+1),
                                        "day"=rep(seq(from=1, 
                                                      to=bhdate-exdate+1+bhdate.bh-exdate.bh+1, by=1), 
                                                  times=length(TSN)))
                        
                            paste(subAccprd, P, M,weekterm, 'est', 'MATdex', sep='_') %>%
                              paste0(datdir, ., '.csv') %>%
                                write.csv(MATdex, file=., quote=F, row.names = F)
                    
                    
                    } else {  ## Output the data of event window: 81 days
                      
                        exdate <- 121 
                        bhdate <- 201
                        
                        stkeve <- data.frame()
                        for (i in unique(stkdat$Stkcd)) {
                            stksim <- filter(stkdat, Stkcd==i) %>% .[c(exdate:bhdate),]
                            stkeve <- rbind(stkeve, stksim)
                        }
                        rm(stksim)
                        
                            paste(subAccprd, P, M, weekterm, 'eve', 'TradStat', sep='_') %>%
                              paste0(datdir, ., '.csv') %>%
                                write.csv(stkeve, file=., quote=F, row.names = F)
                         
                           
                        # output the daily returns to depict the path of returns around QEA
                        stkeve.D <- split(stkeve, stkeve$Stkcd) %>% 
                            lapply(function(x) {
                              ifelse(nrow(x) < (bhdate-exdate+1), return(NULL), return(x))
                            }) %>% # delete the list that nrow not equal to TSN
                              Filter(Negate(function(x) is.null(unlist(x))), .)
                        
                        stkeve.Df <- lapply(stkeve.D, `[`, , ('Dret'))
                        stkDR <- do.call(cbind.data.frame,stkeve.Df) %>% `colnames<-`(names(stkeve.Df))
                        
                            paste(subAccprd, P, M, weekterm, 'DretMean', sep='_') %>%
                              paste0(datdir, ., '.csv') %>%
                                write.csv(rowMeans(stkDR), file=., quote=F, row.names = F)
                        
                    }
                }
            }
        }
    }
}
