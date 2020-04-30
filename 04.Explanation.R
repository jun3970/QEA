library(magrittr)
library(tidyverse)
library(lubridate)

library(broom)
library(glue)


# function to export figure according to type term 
mod_figure <- function(y, h, z) { 
    # y = file name, z = figure size, h = figure ratio
    if(h == 1L){ # 9/16 ration
        paste(Accprd, Pretype, Markettype, Modeltype, grpnum, "Figure", y, sep='_') %>%
            paste0(datdir, "/", ., '.pdf') %>% 
            ggsave(width = 9*z, height = 9*z, dpi = 300, units="in", limitsize = F)
    } else if(h == 2L){
        paste(Accprd, Pretype, Markettype, Modeltype, grpnum, "Figure", y, sep='_') %>%
            paste0(datdir, "/", ., '.pdf') %>% 
            ggsave(width = 8*z, height = 6*z, dpi = 300, units="in", limitsize = F)
    } else if(h == 3L){
        paste(Accprd, Pretype, Markettype, Modeltype, grpnum, "Figure", y, sep='_') %>%
            paste0(datdir, "/", ., '.pdf') %>% 
            ggsave(width = 16*z, height = 9*z, dpi = 300, units="in", limitsize = F)
    } else {print("Please customize the image output process!")}
}



setwd('~/OneDrive/Data.backup/QEAData/CH3/2017/')
# Specifying trem information
Accprd <- as.Date('2017-09-30')
Pretype <- '6'
Markettype <- '21'
Modeltype <- 'CH3'
grpnum <- 2L

datdir <- file.path(getwd(), Accprd)

# Input trading data with FF factors from event window
stkeve <- paste(Accprd, Pretype, Markettype, "stkeve", sep='_') %>% 
    paste0(getwd(), "/", ., ".csv") %>%
    read_delim(delim = ',', na = '', # for comma
           col_types = cols(.default = col_double(),
                    Stkcd = col_character(),
                    TradingDate = col_date(format = "%Y-%m-%d"),
                    Accper = col_date(format = "%Y-%m-%d"),
                    Listdt = col_date(format = "%Y-%m-%d"),
                    Annodt = col_date(format = "%Y-%m-%d"),
                    Markettype = col_factor(levels = c(1,4,16)),
                    Indus = col_factor(),
                    Annowk = col_factor())) %>% split(.$Stkcd)


stkcd <- read_csv("2017-09-30/2017-09-30_6_21_stkcd_sam.csv") %>% pull(1L)
stkeve %<>% `[`(stkcd) 
    
# the number of stocks in our sample this quarter
N <- length(stkeve)
# the length of estimate window
T <- nrow(stkeve[[N]])
stkcd <- names(stkeve)
# the timeline of event window
# Note that we could abandon a part of event window for the beauty of CAR path
timeline <- (-(T-1L)/2):((T-1L)/2)


# Input the aggregate trading data form CSMAR
TRD_Dalyr <- read_delim("~/OneDrive/Data.backup/QEAData/TRD_Dalyr_CSMAR.txt", "\t", 
        escape_double = FALSE, trim_ws = TRUE,
        col_types = cols(.default = col_double(), 
                # stocks symbol, for tidy convenience 
                # we set it's data class as factor (category)
                Stkcd = col_character(),
                # trading date
                Trddt = col_date(format = '%Y-%m-%d'),
                Dretwd = col_skip(), Adjprcwd = col_skip(), Adjprcnd = col_skip(),
                # 1=SH-A, 2=SH-B, 4=SZ-A, 8=SZ-B, 16=startup, 32=Tech
                Markettype = col_factor(levels = c(1,2,4,8,16,32)), 
                # trading status and the late date of its capitalization changed
                Trdsta = col_factor(levels = c(1:16)), Capchgdt = col_skip())) %>% 
    # only the category of 1 indicate that stocks were traded normally 
    filter(Trdsta == "1") %>% select(-Trdsta) %>%  
    # focus on Chinese A-Share markets
    filter(Markettype %in% as.character(c(1, 4, 16))) %>% droplevels() %>%  
    # rename the column of trading date
    rename("TradingDate" = Trddt) %>% 
    # Select a specific time period
    subset(TradingDate %within% interval(Accprd %m+% months(-6), Accprd %m+% months(+6))) %>% 
    # sorting by stock and trading date
    arrange(Markettype, Stkcd, TradingDate) %>%   
    #  generate the time series lists sort by stocks.
    split(.$Stkcd) %>% 
    # subset the stocks in our quarterly sample
    `[`(stkcd) %>% 
    # calculate the amplitude of stocks on trading day
    lapply(mutate, amplitude = c(NaN, (Hiprc - Loprc)[-1] / Clsprc[-length(Clsprc)]))


# Import the transaction derivative index
STK_MKT_Dalyr <- read_delim("~/OneDrive/Data.backup/QEAData/STK_MKT_Dalyr_CSMAR.txt", 
    "\t", escape_double = FALSE, trim_ws = TRUE,
    col_types = cols_only(
         Symbol = col_character(), 
         TradingDate = col_date(format = "%Y-%m-%d"), 
         ChangeRatio = col_double(), Turnover = col_double(),
         Liquidility = col_double(), 
         CirculatedMarketValue = col_double(),
         PE = col_double(), PB = col_double(),
         PS = col_double())) %>% 
    rename('Stkcd' = Symbol) %>% 
    subset(TradingDate %within% interval(Accprd%m+%months(-6), Accprd%m+%months(+6))) %>% 
    split(.$Stkcd) %>% `[`(stkcd) 

# import the data of abnormal return
setwd(datdir)
QEA_gAR <- paste(Accprd, Pretype, Markettype, Modeltype, grpnum, sep='_') %>% 
    paste0('.*?AR[.]csv$') %>% 
    dir(pattern = .) %>% paste0(getwd(), "/", .) %>% 
    read_delim(delim=',', na = '',
               col_types = cols(Stkcd = col_character(),
                       TradingDate = col_date(format = "%Y-%m-%d"),
                       g.PLS = col_factor(levels = c(1:grpnum)),
                       #g.PLS = col_factor(levels = c(1,2,3)),
                       AbRet = col_double())) %>% 
    split(.$Stkcd) %>% 
    lapply(add_column, "timeline" = factor(timeline, ordered = TRUE))

# merge above three parts of data
STK_reg <- map2(stkeve, TRD_Dalyr, left_join) %>% 
    map2(STK_MKT_Dalyr, left_join, by = c("Stkcd", "TradingDate")) %>% 
    lapply(add_column, "timeline" = factor(timeline, ordered = TRUE)) %>% 
    `[`(stkcd) %>% # join using map2 function, Notice that the order of 
    # stocks in two lists, otherwise the join process will appear False 
    map2(QEA_gAR[stkcd], left_join) %>% bind_rows()


# Standard deviation of group stock's daily returns everyday (event window)
QEA_Dret_sd <- group_by(STK_reg, g.PLS, timeline) %>% 
    summarise("Dretmrf_sd" = sd(Dret_rf)) #%>% spread(g.PLS, Dretmrf_sd)
QEA_Dret_sd$timeline %<>% as.character() %>% as.integer()
ggplot(QEA_Dret_sd, aes(x=timeline, y=Dretmrf_sd, colour=g.PLS)) +
    geom_path() + geom_point(aes(shape = g.PLS), size = 2) +
    scale_color_manual(values = c("#377EB8", "#E41A1C")) +
    scale_x_continuous(breaks=seq(-30, 30, by = 5), labels = seq(-30, 30, by=5)) + 
    labs(x = "Time line", y = 'standard deviation of stock\'s real daily return') + 
    theme_bw() +
    theme(plot.title = element_text(size=14),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(),
          axis.title.y = element_text(face = "italic"),
          legend.title = element_blank(),
          legend.position = c(0.1, 0.9),
          legend.text = element_text(size=12))

mod_figure("Dret_sd", 3L, 1.2)

QEA_gAR_sd <- group_by(STK_reg, g.PLS, timeline) %>% 
    summarise("AR_sd" = sd(AbRet)) #%>% spread(g.PLS, AR_sd)
QEA_gAR_sd$timeline %<>% as.character() %>% as.integer()
ggplot(data=QEA_gAR_sd, aes(x=timeline, y=AR_sd, colour = g.PLS)) +
    geom_path() + geom_point(aes(shape = g.PLS), size = 2) +
    scale_color_manual(values = c("#377EB8", "#E41A1C")) +
    scale_x_continuous(breaks=seq(-30, 30, by = 5), labels = seq(-30, 30, by=5)) + 
    labs(x = "Time line", y = 'standard deviation of stock\'s daily abnormal return in event window') + 
    theme_bw() +
    theme(plot.title = element_text(size=14),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.x = element_text(),
          axis.title.y = element_text(face = "italic"),
          legend.title = element_blank(),
          legend.position = c(0.1, 0.9),
          legend.text = element_text(size=12))

# Statistical properties

# Regression
library(stargazer)

### Time series ======

# trading ====
## unclassified
summary(lm(AbRet ~ amplitude + Turnover + Dnshrtrd + Dsmvosd + Liquidility, data = STK_reg))
## PLS
STK_reg %>% split(.$g.PLS) %>%
    map(~ lm(AbRet ~ amplitude + Turnover + Dnshrtrd + Dsmvosd + Liquidility, data = .x)) %>% 
    stargazer(align = TRUE, type = "text", keep.stat = c("n", "rsq"))

lm(AbRet ~ (amplitude + Turnover + Dnshrtrd + Dsmvosd + Liquidility)*g.PLS, data = STK_reg) %>% 
    stargazer(align = TRUE, type = "text", keep.stat = c("n", "rsq"))

## group-stkcd
# data frame
trd_lm <- function(df) lm(AbRet ~ amplitude + Turnover + Dnshrtrd + Dsmvosd + Liquidility, data = df)
ts_trdreg <- group_nest(STK_reg, g.PLS, Stkcd) %>% 
    transmute(g.PLS, Stkcd, "trd_tslm" = map(data, ~ trd_lm(.x) %>% tidy())) %>% 
    unnest(cols = c(trd_tslm))
# table
STK_reg %>% split(.$g.PLS) %>% 
    lapply(function(x) { split(x, x$Stkcd) %>% 
        lapply(trd_lm)}) %>% 
            lapply(sample, 5) %>% 
                stargazer(align = TRUE, type = "text", keep.stat = c("n", "rsq"))



# factor =====
datdir <- '~/OneDrive/Data.backup/QEAData/'
# Import the accounting data within quarterly financial report 
ReptInfo <- read_delim(paste0(datdir, 'IAR_Rept.csv'), delim='\t', na = '',
                   col_types = cols_only(Stkcd = col_character(),
                             # the deadline of accounting cycle
                             Accper = col_date(format = "%Y-%m-%d"),
                             # the date when report was discolsed
                             Annodt = col_date(format = "%Y-%m-%d"),
                             # net profits and earnings per share
                             Profita = col_double(), Erana = col_double())) %>% 
    filter(Stkcd %in% stkcd & Accper == Accprd) %>% arrange(Stkcd) %>%
    mutate('g.UMD' = factor(cut(.$Erana, quantile(.$Erana, c(0, 0.3, 0.7, 1)), labels=F), 
                            levels = c(1,2,3), ordered = T)) %>% 
    # there is a pity! when we use cut function to structure the category variable,
    # several stocks will be droped, and we get NA
    na.omit()

# merge the trading data, transaction derivative index, 
# CH3 factors, PLS result, AR, with report information
STK_reg %<>% inner_join(ReptInfo) %>% arrange(Stkcd, timeline)
# structure the factor UMD
group_by(STK_reg, g.UMD, timeline) %>% summarise("avgUMD" = mean(Dretnd)) %>% 
    ggplot(aes(x=as.integer(timeline), y=avgUMD, colour=g.UMD)) +
    geom_path() +
    scale_color_brewer(palette = "Dark2")
    
UMD <- group_by(STK_reg, g.UMD, timeline) %>% summarise("avgUMD" = mean(Dretnd)) %>% 
    spread(g.UMD, avgUMD)
# doing the substract between with portfolios
UMD <- tibble("timeline" = factor(timeline, ordered = TRUE), "UMD" = UMD$`3` - UMD$`1`)
# merge with regression data
STK_reg %<>% inner_join(UMD) %>% na.omit()

## PLS
STK_reg %>% split(.$g.PLS) %>%
    map(~ lm(AbRet ~ UMD, data = .x)) %>% 
    stargazer(align = TRUE, type = "text", keep.stat = c("n", "rsq"))

lm(AbRet ~ UMD*g.PLS, data = STK_reg) %>% 
    stargazer(align = TRUE, type = "text", keep.stat = c("n", "rsq"))

## group-stkcd
### data frame
ff_lm <- function(df) lm(AbRet ~ UMD, data = df)
ts_ffreg <- group_nest(STK_reg, g.PLS, Stkcd) %>% 
    transmute(g.PLS, Stkcd, "ff_tslm" = map(data, ~ ff_lm(.x) %>% tidy())) %>% 
    unnest(cols = c(ff_tslm))
### table
STK_reg %>% split(.$g.PLS) %>% 
    lapply(function(x) { split(x, x$Stkcd) %>% 
        lapply(ff_lm)}) %>% 
            lapply(sample, 5) %>% 
                stargazer(align = TRUE, type = "text", keep.stat = c("n", "rsq"))


# cross-section
STK_csreg <- group_nest(STK_reg, g.PLS, timeline) 
cs_trdreg <- transmute(STK_csreg, g.PLS, timeline, "trd_cslm" = map(data, ~ trd_lm(.x) %>% tidy())) %>% 
    unnest(cols = c(trd_cslm)) %>% print()

cs_trdreg %>% gather(statistics, estimator, estimate, statistic) %>% 
ggplot(aes(x=as.integer(timeline), y=estimator, colour = g.PLS)) +
    geom_path() +
    facet_grid(statistics~term)
   