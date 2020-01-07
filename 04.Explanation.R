library(tidyverse)
library(lubridate)

# Specifying trem information
Accprd <- as.Date('2017-09-30')
Pretype <- '6'
Markettype <- '5'
weekterm <- 'wekbind'
modeltype <- 'FF3'
datadir <- '~/NutSync/MyData/QEAData/'


# Input trading data with FF factors from event window
stkeve <- paste(Accprd, Pretype, Markettype, weekterm, sep='_') %>% 
    paste0('.*?eve_TradStat[.]csv$') %>% 
    dir(datadir, pattern = .) %>% 
    paste0(datadir, .) %>% 
    read_delim(delim=',', na = '')


T <- 81L
N <- nrow(stkeve)/T
Quittime <- 20L # we abandon somes event window for the beauty of data visualization
timeline <- c(-20:+40) # just the order of tau, 
# the number of day that before or after the earnings announcement day

minsrow <- c()
for (i in 1:N) {
    subrow <- seq(1, Quittime) + (i-1)*T
    minsrow <- c(minsrow, subrow)
}
stkeve <- stkeve[-minsrow,]
Stkcd_sub <- unique(stkeve$Stkcd) # symbols of selected stocks
rm(subrow, minsrow)

# Input the aggregate trading data form CSMAR

TRD_Dalyr <- read_delim("~/NutSync/MyData/QEAData/TRD_Dalyr.txt", "\t", 
                        escape_double = FALSE, trim_ws = TRUE,
                        col_types = cols(
                        Adjprcnd = col_skip(), Adjprcwd = col_skip(), 
                        Capchgdt = col_skip(), Clsprc = col_number(), 
                        Dnshrtrd = col_number(), 
                        Dnvaltrd = col_number(), Dretnd = col_skip(), 
                        Dretwd = col_skip(), Dsmvosd = col_number(), 
                        Dsmvtll = col_number(), Hiprc = col_number(), 
                        Loprc = col_number(), Markettype = col_integer(), 
                        Opnprc = col_number(), Stkcd = col_character(), 
                        Trddt = col_date(format = "%Y-%m-%d"), 
                        Trdsta = col_integer())) %>% 
            rename('TradingDate' = Trddt)

TRD_Dalyr_sub <- TRD_Dalyr[-c(1,2), ] %>% 
             subset(Stkcd %in% Stkcd_sub) %>% 
             subset(TradingDate %within% interval(Accprd %m+% months(-6), Accprd %m+% months(+6)))


# calculate the amplitude of stocks on trading day

TRD_sam <- data.frame()    
for (i in Stkcd_sub) {
    TRD_sim <- TRD_Dalyr_sub %>% as_tibble() %>%
        subset(Stkcd == i) %>% 
        mutate(amplitude = c(NaN, (Hiprc - Loprc)[-1] / Clsprc[-length(Clsprc)]))
    TRD_sam <- rbind(TRD_sam, TRD_sim)
}
TRD_Dalyr_sub <- TRD_sam; rm(TRD_sam, TRD_sim)


# Input the transaction derivative index

STK_MKT_Dalyr <- read_delim("~/NutSync/MyData/QEAData/STK_MKT_Dalyr.txt", 
                            "\t", escape_double = FALSE, trim_ws = TRUE,
                            col_types = cols(Amount = col_skip(), Symbol = col_character(),
                                             ChangeRatio = col_skip(), 
                                             CirculatedMarketValue = col_skip(), 
                                             Liquidility = col_number(), PB = col_number(), 
                                             PCF = col_skip(), PE = col_number(), 
                                             PS = col_number(), Ret = col_number(), 
                                             SecurityID = col_skip(), ShortName = col_skip(), 
                                             TradingDate = col_date(format = "%Y-%m-%d"), 
                                             Turnover = col_number())) %>% 
                rename('Stkcd' = Symbol)

STK_MKT_Dalyr_sub <- STK_MKT_Dalyr[-c(1,2), ] %>% 
    subset(Stkcd %in% Stkcd_sub) %>% 
    subset(TradingDate %within% interval(Accprd %m+% months(-6), Accprd %m+% months(+6)))


# merge above three parts of data
TRD_reg <- merge(stkeve, TRD_Dalyr_sub, by = c("Stkcd", "TradingDate")) %>% 
            merge(STK_MKT_Dalyr_sub, by = c("Stkcd", "TradingDate"))



# bind colums of abnormal returns and group information with above trading status (TRD_reg)
ARcd <- paste(Accprd, Pretype, Markettype, weekterm, modeltype, 'group', sep='_') %>% 
        paste0('.*?AR[.]csv$') %>% 
        dir(datadir, pattern = .) %>% 
        paste0(datadir, .)


for (i in 1:length(ARcd)) {
   assign(paste0('stkabr', '_g', i), read_delim(ARcd[i], delim=',', na = ''))
   stkabrgp <- get(paste0('stkabr', '_g', i))
   assign(paste0('TRD', '_reg','_g', i), subset(TRD_reg, Stkcd %in% colnames(stkabrgp)))
   TRD_reg_gp <- get(paste0('TRD', '_reg','_g', i))
   
   TRD_lm <- data.frame()
   for (z in colnames(stkabrgp)[-1]) {
       TRD_abr <- cbind(subset(TRD_reg_gp, Stkcd == z),
                        stkabrgp[, colnames(stkabrgp) == z], 
                        'group' = c(i))
       if (z == unique(TRD_abr$Stkcd)) {
          colnames(TRD_abr)[ncol(TRD_abr)-1] <- 'abnormalreturn' 
          TRD_lm <- rbind(TRD_lm, TRD_abr)
       } else cat('exsit matching error for stock', z)
   }
   assign(paste0('TRD', '_reg', '_g', i), TRD_lm)
}

TRD_reg <- mget(ls(pattern = '^TRD_reg_g[0-9]$'))
rm(stkabrgp, TRD_reg_gp, TRD_abr,TRD_lm)


# Standard deviation of group stock's daily returns everyday (event window)
TRD_Dret_sd <- c()
for (i in 1:length(ARcd)) {
    TRD_reg_ana <- get(ls(pattern = '^TRD_reg_g[0-9]$')[i])
    stkabr_ana <- get(ls(pattern = '^stkabr_g[0-9]$')[i])
    TRD_Dret_sdc <- c()
    for (z in 1:nrow(stkabr_ana)) {
        sd_nrow <- seq(z, nrow(stkabr_ana)*(ncol(stkabr_ana)-1), nrow(stkabr_ana))
        TRD_Dretsd <- sd(TRD_reg_ana$Dretwd[sd_nrow])
        TRD_Dret_sdc <- c(TRD_Dret_sdc, TRD_Dretsd)
    }
    TRD_Dret_sd <- cbind(TRD_Dret_sd, TRD_Dret_sdc)
}
colnames(TRD_Dret_sd) <- ls(pattern = '^stkabr_g[0-9]$')
rm(stkabr_ana, TRD_reg_ana, sd_nrow, TRD_Dretsd, TRD_Dret_sdc, sd_nrow)


plot(TRD_Dret_sd[,1])
plot(TRD_Dret_sd[,2], col='red')



# Statistical properties
summary(TRD_reg_g1$Dretwd)
summary(TRD_reg_g2$Dretwd)



# Regression
summary(lm(abnormalreturn ~ Liquidility, data = TRD_reg[[1]]))
summary(lm(abnormalreturn ~ Liquidility, data = TRD_reg[[2]]))
summary(lm(abnormalreturn ~ Liquidility, data = rbind()))

summary(lm(abnormalreturn ~ Turnover+amplitude+Liquidility, 
           data = TRD_reg[[1]]))
summary(lm(abnormalreturn ~ Turnover+amplitude+Liquidility, 
           data = TRD_reg[[2]]))
summary(lm(abnormalreturn ~ Turnover+amplitude+Liquidility, 
           data = rbind()))


