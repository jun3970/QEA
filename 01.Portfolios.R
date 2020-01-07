## CC Hongyu Hu 
## 2018.04--2019.10

## Summary of this Script ====
# This R script are writen for obtain the 5*5 stock portfolios 
# and 2*3 mimicing risk factors formed on size and 
# earnings-to-price (EP) equity in Chinese A-share markets,
# whos code symbol begin with 00, 30, 60.

# The quantity of a stock *size* is computed as the end of 
# previous quarter’s closing price times its total shares.
# A stock’s *EP* is the ratio of the quarterly earnings which is
# the most recently reported net profit excluding nonrecurrent 
# gains/losses in last quarter to the product of its closing price 
# and total shares at ending of last quarter. 

# We weight each stock by its market capitalization 
# produce by its outstanding A shares, including nontradable shares, 
# product its daily closing price at present.

# The market factor, MKT, is the return on the value-weighted portfolio 
# formed with all stocks in Chinese A-Share Markets
# in excess of the current period one-year deposit interest rate.

# For the size factor, SMB (small minus big), we obtain it by using 
# the median size of stocks belong to Chinese A-Share markets to
# split stocks into two groups, small and big. 

# To obtain the value factor, VMG (value minus growth), 
# We break stocks into three groups by earnings-to-price ratio
# based on the breakpoints for the ranked bottom 30% (value, V), 
# middle 40% (Medium, M), and top 30% (growth, G) in each quarter 'q'.

# That is our size and size factors, denoted as SMB and VMG, 
# combined the returns on these six intersect portfolios as follows,
#
# SMB = 1/3 (Small Value + Small Neutral + Small Growth) – 
#       1/3 (Big Value + Big Neutral + Big Growth).
#
# VMG = 1/2 (Small Value + Big Value) – 
#       1/2 (Small Growth + Big Growth).
#
# Note that daily value-weighted returns on the six portfolios 
# are calculated from 'q' quarter to 'q+1' quarter 
# using available accounting data produced at quarter 'q-1'.


library(tidyverse)
library(plyr)
library(magrittr)
library(lubridate)


# set the directory of original data 
datdir <- '~/NutSync/MyData/QEAData/'  # Hu, PC-Carbon

# Input the aggregate daily trading data =====
trddat <- read_delim(paste0(datdir, 'TRD_Dalyr_CSMAR.txt'), delim = '\t', na = '',
                     col_types = cols(Stkcd = col_character(),
                                      Trddt = col_date(format = '%Y-%m-%d'),
                                      Opnprc = col_number(),
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
    arrange(Stkcd, TradingDate) %>%   # sorting by stock and trading date
    split(., .$Markettype) # generate the lists according to market class


# dependent variable, Ri - Rf =====

# calculate the daily returns
for(i in 1:length(trddat)) {
    trddat[[i]] <- cbind(trddat[[i]], Dret = NaN)
    for (k in unique(trddat[[i]]$Stkcd)) {
        Dret <- subset(trddat[[i]], Stkcd == k, select = c(Clsprc), drop = TRUE)
        # take the logarithm of the closing price of stocks, and then do the difference
        Dret <- c(NaN, log(Dret[-1] / Dret[-length(Dret)], base=exp(1))) 
        trddat[[i]][which(trddat[[i]]$Stkcd == k), "Dret"] <- Dret; rm(Dret)
    }
    if (length(which(is.na(trddat[[i]]$Dret))) == length(unique(trddat[[i]]$Stkcd))){
        trddat[[i]] <- trddat[[i]][complete.cases(trddat[[i]]),]
    } else print(paste('Warning! Daily retruns of list', i, 
                       'were calculated with vectorization error.'))
} 


# Input daily one-year deposit interest rate, Rf 
Rf <- read_delim(paste0(datdir, 'TRD_Nrrate_CSMAR.csv'), delim='\t', na = '',
                 col_types = cols(Nrr1 = col_factor(levels = c('NRI01', 'TBC')),
                                  Clsdt = col_date(format = '%Y-%m-%d'),
                                  Nrrdaydt = col_double())) %>%
    rename('TradingDate' = Clsdt) %>%
    filter(Nrr1 == 'NRI01') %>% # select the one-year deposit interest rate
    select(c(TradingDate, Nrrdaydt)) %>% 
    arrange(TradingDate)

# Merge the trading data with risk-free rate using 'lapply'
# and we get the result list grouping by trading date (then stock's symbol)
trddat <- lapply(trddat, merge, Rf, by = 'TradingDate') %>% 
    lapply(arrange, TradingDate, Stkcd)


# Setup Working day arround QEA ====
# Sun < Mon < Tue < Wed < Thu < Fri < Sat, c(0:6)
Trdday <- read_delim(paste0(datdir, 'TRD_Cale.csv'), delim='\t', na = '',
                     col_types = cols(Markettype = col_factor(c(1,2,4,8,16)),
                                      Daywk = col_factor(c(0:6)),
                                      Clddt = col_date(format = "%Y-%m-%d"),
                                      State = col_factor(levels = c('C', 'O')))) %>% 
    rename('TradingDate' = Clddt) %>%
    filter(State == c('O')) %>% # 'Open', going trading on that day
    arrange(TradingDate) %>% 
    split(., .$Markettype) %>% # generate the lists grouped by market class
    lapply(subset, select = c(TradingDate), drop = TRUE) %>% 
    `[`(c('1','4','16')) # select the stocks of Chinese A-Share markets


# Input the list date of stocks
AF_Co <- read_delim(paste0(datdir, 'AF_Co.csv'), delim='\t', na = '',
                    col_types = cols(Stkcd = col_character(), 
                                     Listdt = col_date('%Y-%m-%d'))) %>% 
    select(c(Stkcd, Listdt))


# Input the accounting data whitin quarterly financial report 
# There is a defect that the stock's adjusted quarterly profits and earnings 
# from CSMAR exsited too many inherent NaN, so we have to use the unadjusted
# index to form the asset portfolios provided in Fama (1993).
ReptInfo <- read_delim(paste0(datdir, 'IAR_Rept.csv'), delim='\t', na = '',
                       col_types = cols(Stkcd = col_character(),
                                        Sctcd = col_factor(levels = c(1, 2)),
                                        Reptyp = col_factor(levels = c(1, 2, 3, 4)),
                                        Accper = col_date(format = "%Y-%m-%d"),
                                        Annodt = col_date(format = "%Y-%m-%d"),
                                        Profita = col_double(),
                                        Erana = col_double())) %>%
    select(c(Stkcd,Sctcd,Reptyp,Accper,Annodt,Profita,Erana)) %>% 
    # select the stocks of Chinese A-Share markets
    filter(grepl('^[0-6]', Stkcd)) %>% 
    arrange(Stkcd, Accper)

save.image(paste0(datdir, 'PrePotfolReg', '.RData'))




# set the quarter terms
# notice that we need to set one quarter ahead
Accprd <- months(seq(0, by=3, length=24)) %>% 
    mapply('%m+%', ymd('2012-12-31'), .) %>% 
    as.Date(origin='1970-01-01') 

# setup the saving list of results
potfolstk <- vector('list', length = length(Accprd)) %>% `names<-`(Accprd)
potfoldat <- vector('list', length = length(Accprd)) %>% `names<-`(Accprd)
potfolreg <- vector('list', length = length(Accprd)) %>% `names<-`(Accprd)

# loop strat point
for(q in 1:length(Accprd)) {
    
    # seeking the fundamental accounting status stocks ====
    # in Chinese A-Share markets at quarter t-1 
    
    # find the last trading date at the ending of quarter t-1 
    # Mon < Tue < Wed < Thu < Fri < Sat < Sun, c(1:7)
    ahdqua <- (Accprd[q] + days(1)) %m+% months(-3) + days(-1)
    wday(ahdqua, label = TRUE, week_start = getOption("lubridate.week.start", 1))
    splday <- lapply(Trdday, function(x) {while (!(ahdqua %in% x)) {
        ahdqua <- ahdqua + days(-1)}
        return(ahdqua)})
    
    # eliminate the outer stocks =====
    
    # eliminating the the stocks which listed less than six months
    trddatl <- vector(mode="list", length = length(trddat)) %>% `names<-`(names(trddat))
    for(i in 1:length(trddat)) {
        trddatl[[i]] <- merge(trddat[[i]], AF_Co, by='Stkcd') %>% 
            filter(interval(Listdt, splday[[i]]) >= months(6)) %>% 
            arrange(Stkcd, TradingDate)
    }
    
    # eliminating the the stocks which having less than 180 trading records
    # in the past year or less than 15 trading records in the past month.
    # generate the time series lists by stocks
    trddatl <- lapply(trddatl, function(x) split(x, x$Stkcd)) 
    for (i in 1:length(trddatl)) {
        d <- vector(mode = 'integer')
        for(k in 1:length(trddatl[[i]])) {
            if('|'(trddatl[[i]][[k]] %>% 
                   filter(TradingDate %within% interval(splday[[i]] + years(-1), splday[[i]])) %>% 
                   nrow() <= 180L, 
                   trddatl[[i]][[k]] %>% 
                   filter(TradingDate %within% interval(splday[[i]] %m+% months(-1), splday[[i]])) %>% 
                   nrow() <= 15L)) d <- c(d,k)
        }
        trddatl[[i]][d] <- NULL
        trddatl[[i]] <- do.call(rbind.data.frame, trddatl[[i]])
    }
    
    
    # Classification ====
    
    ### Ahead ###
    
    # Generate the calendar lists ====
    trdlc <- lapply(trddatl, function(x) split(x, x$TradingDate))
    
    # the accounting status at the ending of quarter t-1
    RepInf <- filter(ReptInfo, Accper == ahdqua) %>% 
        mutate(CmnEqty = Profita / Erana) %>% 
        select(Stkcd, Erana, CmnEqty)
    
    # extract the closing price of stocks at the ending of quarter t-1
    # and calculate the accounting indicator, 'size' and value (EP)
    potfol <- vector(mode="list", length = length(trdlc))
    for (i in 1:length(splday)) {
        names(potfol)[i] <- names(splday)[i]
        potfol[[i]] <- trdlc[[i]] %>% `[[`(as.character(splday[[i]])) %>%  
            select(Stkcd, TradingDate, Clsprc) %>% 
            # notice that only the stocks that had published 
            # quarter financial report within our sample
            merge(RepInf, by='Stkcd') %>%  
            mutate(Size = Clsprc * CmnEqty, # SMB
                   Value = Erana / Clsprc) %>%  # VMG
            `[`(complete.cases(.$Size & .$Value),)
    } 
    
    
    ####### The explanatory returns ########
    
    # Notice that we could set two cluster paths, HML or VMG, at there.

    # we use the quantile of the indicators Size and Value to group the stocks
    potfol1 <- do.call(rbind.data.frame, potfol) %>% `row.names<-`(NULL) %>% 
        mutate('SMB' = cut(.$Size, quantile(.$Size, c(0, 0.5, 1)), labels=F)) %>% 
        mutate('VMG' = cut(.$Value, quantile(.$Value, c(0, 0.3, 0.7, 1)), labels=F)) %>% 
        `[`(complete.cases(.$SMB & .$VMG),)
    
    
    # structure the 2*3 portfolios ====
    
    # size effect 
    sizeff <- c('small','big')
    for(i in 1:length(sizeff)) {
        assign(sizeff[i], subset(potfol1, SMB == i, select = c(Stkcd), drop = TRUE))}
    
    # value effect 
    valueff <- c('value','middle','growth')
    for(i in 1:length(valueff)) {
        assign(valueff[i], subset(potfol1, VMG == i, select = c(Stkcd), drop = TRUE))}
    
    # make intersect of size and value portfolios
    # and we will use them to calculate the Fama-French risk factors.
    potfolff <- vector('list', length = length(sizeff)*length(valueff))
    potfolname <- c('SV','SM','SG','BV','BM','BG'); k <- 0
    for(i in 1:length(sizeff)){
        for (u in 1:length(valueff)) {
            k <- k+1
            potfolff[k] <- list(intersect(get(sizeff[i]), get(valueff[u])))
            names(potfolff)[k] <- potfolname[k]
        }
    }
    
    # save the portfolios
    potfolstk[[q]] <- potfolff
    
    # calculate the daily risk factors at the quarter t+1 ====
    
    ### Behind ###
    
    # tidy the panel data at the quarter t-1
    trdlff <- do.call(rbind.data.frame, trddatl) %>% # delete market type attributes
        select(Stkcd, TradingDate, Dsmvtll, Dret, Nrrdaydt) %>% 
        # extract the time series interval for the t-1 quarter
        filter(TradingDate %within% 
                   interval(Accprd[q]+days(1), (Accprd[q]+days(1))%m+%months(3)+days(-1))) %>% 
        # Generate the calendar ordered lists
        split(., .$TradingDate) %>% 
        # abandon the new listed stocks
        lapply(filter, Stkcd %in% unlist(potfolff, use.names=FALSE))
    
    
    # mkt_rf, the returns of factor MKT minus the risk-free rate ====
    mkt_rf <- lapply(trdlff, function(x) {
        '/'(x$Dsmvtll, sum(x$Dsmvtll)) %>% # obtain the daily weighted-value
            '%*%'(x$Dret) %>% as.vector() %>% # calculate the index of the factor MKT
            '-'(., unique(x$Nrrdaydt))}) # minus the risk-free rate of market 
    
    
    # form the 2*3 stock portfolios after filtering the stocks
    trdlp <- lapply(trdlff, function(x) {
        Pt <- vector('list', length = length(potfolff))
        names(Pt) <- names(potfolff)
        for (i in 1:length(Pt)) {Pt[[i]] <- filter(x, Stkcd %in% potfolff[[i]])} 
        return(Pt)
    }) 
    
    # calculate the factors of SMB and VMG  ====
    
    # measure the risk returns for portfolios by calendar
    trdlpr <- lapply(trdlp, function(x) {
        lapply(x, function(y) {
            # weighted-value
            weg <- subset(y, select=c(Dsmvtll), drop = TRUE) %>% '/'(., sum(.))
            # weighted-value returns for portfolios
            return(weg %*% subset(y, select=c(Dret), drop = TRUE) %>% 
                       as.vector())})
    })
    
    
    SMB <- lapply(trdlpr, function(x){
        1/3 * ('-'(do.call(sum, x[c('SV','SM','SG')]), 
                   do.call(sum, x[c('BV','BM','BG')]))) 
    })
    
    VMG <- lapply(trdlpr, function(x){
        1/2 * ('-'(do.call(sum, x[c('SV','BV')]), 
                   do.call(sum, x[c('SG','BG')]))) 
    })
    
    
    
    ####### The dependent returns of 5*5 stock portfolios #########
    
    # group by accouting indicator, Size and Value
    potfol2 <- do.call(rbind.data.frame, potfol) %>% `row.names<-`(NULL) %>% 
        mutate('SMB' = cut(.$Size, 
                           quantile(.$Size, seq(0,1,by=0.2)), labels=F)) %>% 
        mutate('VMG' = cut(.$Value, 
                           quantile(.$Value, seq(0,1,by=0.2)), labels=F)) %>% 
        `[`(complete.cases(.$SMB & .$VMG),)
    
    # form the intersection 5*5 stock portfolios 
    sizeP5 <- c('size1','size2','size3','size4','size5')
    for(i in 1:length(sizeP5)) {
        assign(sizeP5[i], subset(potfol2, SMB == i, select = c(Stkcd), drop = TRUE))}
    
    valueP5 <- c('value1','value2','value3','value4','value5')
    for(i in 1:length(valueP5)) {
        assign(valueP5[i], subset(potfol2, VMG == i, select = c(Stkcd), drop = TRUE))}
    
    potfol25 <- vector('list', length = length(sizeP5)*length(valueP5)); k <- 0
    for(i in 1:length(sizeP5)){
        for (u in 1:length(valueP5)) {
            k <- k+1
            potfol25[k] <- list(intersect(get(sizeP5[i]), get(valueP5[u])))
            names(potfol25)[k] <- paste0('sz', i, 'vl', u)
        }
    }
    
    # form the 5*5 stock portfolios after filtering the stocks
    trdlp25 <- lapply(trdlff, function(x) {
        Pt <- vector('list', length = length(potfol25))
        names(Pt) <- names(potfol25)
        for (i in 1:length(Pt)) {Pt[[i]] <- filter(x, Stkcd %in% potfol25[[i]])} 
        return(Pt)
    }) 
    
    trdlpr25 <- lapply(trdlp25, function(x) {
        lapply(x, function(y) {
            # weighted-value
            subset(y, select=c(Dsmvtll), drop = TRUE) %>% '/'(., sum(.)) %>% 
                # weighted-value returns for portfolios
                '%*%' (subset(y, select=c(Dret), drop = TRUE)) %>% as.vector()})}) %>% 
        do.call(rbind.data.frame, .) %>% # get the time series returns of 25 stock portfolios
        mutate(TradingDate = as.Date(rownames(.))) %T>% print() %>%  
        `[`(, c(ncol(.), 1:25))
    
    # retain the daily regression data
    potfoldat[[q]] <- cbind(trdlpr25, 
                            'mkt_rf'=unlist(mkt_rf), 'SMB'=unlist(SMB), 'VMG'=unlist(VMG)) %>% 
        merge(Rf, by='TradingDate')
    
    
    ############## Regression ##################
    
    # run time series regression by portfolios
    potfollm <- vector('list', length = length(potfol25))
    names(potfollm) <- names(potfol25)
    for (i in 1:length(potfol25)) {
        potfollm[[i]] <- eval(substitute(lm( I(variable - Nrrdaydt) ~ 
                                                 mkt_rf + SMB + VMG, data = potfoldat[[q]]),
                                         list(variable = as.name(names(potfol25)[i]))))
    }
    
    # save the regression results
    potfolreg[[q]] <- potfollm
    
}

save(potfolstk, potfoldat, potfolreg, file = 'HuCH3.RData')


######### coefficients of regression results #########

varname <- c('(Intercept)', 'mkt_rf', 'SMB', 'VMG')
staname <- c('Estimate', 't value')
potfolreg.est <- lapply(potfolreg, function(x) {
    lapply(x, function(y) coef(summary(y)) %>% as.data.frame()%>% 
               `[`(varname, staname))
})

