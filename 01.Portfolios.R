# Summary of this Script ====
# CC Hongyu Hu
# 2018.04--2019.10
# 
# This R script are writen for obtain the 5*5 stock portfolios
# and 2*3 mimicing risk factors formed on size and
# earnings-to-price (EP) equity in Chinese A-share markets,
# whos code symbol begin with 00, 30, 60.
# 
# The quantity of a stock *size* is computed as the end of
# previous quarter’s closing price times its total shares.
# A stock’s *EP* is the ratio of the quarterly earnings which is
# the most recently reported net profit excluding nonrecurrent
# gains/losses in last quarter to the product of its closing price
# and total shares at ending of last quarter.
# 
# We weight each stock by its market capitalization
# produce by its outstanding A shares, including nontradable shares,
# product its daily closing price at present.
# 
# The market factor, MKT, is the return on the value-weighted portfolio
# formed with all stocks in Chinese A-Share Markets
# in excess of the current period one-year deposit interest rate.
# 
# For the size factor, SMB (small minus big), we obtain it by using
# the median size of stocks belong to Chinese A-Share markets to
# split stocks into two groups, small and big.
# 
# To obtain the value factor, VMG (value minus growth),
# We break stocks into three groups by earnings-to-price ratio
# based on the breakpoints for the ranked bottom 30% (value, V),
# middle 40% (Medium, M), and top 30% (growth, G) in each quarter 'q'.
# 
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

library(magrittr)
library(tidyverse)
library(lubridate)

library(glue)
library(broom)
library(multipanelfigure)
library(RColorBrewer)
####### Part I, import data ########

# working directory
setwd('~/OneDrive/Data.backup/QEAData/')
# Output directory
datdir <- file.path(getwd(), "CH3")
# Import the aggregate data of daily trading in China A-Share market
trddat <- read_delim('TRD_Dalyr_CSMAR.txt', delim = '\t', na = '',
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
    filter(TradingDate %within% interval('2012-01-01', '2019-12-31')) %>% 
    # sorting by stock and trading date
    arrange(Markettype, Stkcd, TradingDate) %>%   
    #  generate the time series lists sort by stocks.
    split(.$Stkcd)


#calculate the daily returns of stocks (dependent variable)
trddat.Dret <- map(trddat, ~ {# closing price everyday
                        Clsprc <- pull(.x, Clsprc) 
                        # daily return
                        Dret <- log( Clsprc[-1] / Clsprc[-length(Clsprc)], base=exp(1))})
# confirm the results we calculated is same with CSMAR about daily returns of stocks
# R_{it} = ln(P_{it}) - ln(P_{it-1})
if(map2_lgl(trddat.Dret, trddat, ~ {
    # my own results of stock's daily return
    Dret.me <- round(.x, digits = 3)
    Dret.CSMAR <- pull(.y, Dretnd)[-1] %>% round(digits = 3)
    near(Dret.me, Dret.CSMAR) %>% any()}
    ) %>% any()) {
    print("Congratulations! The daily returns from CSMAR are same with us.")
    rm(trddat.Dret)
    } else print("Attention! The daily returns from CSMAR aren't same with us.")


# Import the daily one-year-deposit-interest-rate, free-risk return in market 
Nrrate <- read_delim('TRD_Nrrate_CSMAR.csv', delim='\t', na = '',
                 col_types = cols_only(Nrr1 = col_factor(levels = c('NRI01', 'TBC')),
                                       Clsdt = col_date(format = '%Y-%m-%d'),
                                       Nrrdaydt = col_double())) %>%
    # select the one-year deposit interest rate as the free-risk return
    filter(Nrr1 == 'NRI01') %>% select(-Nrr1) %>% 
    # you can either select the Treasury rate with 
    # filter(Nrr1 == "TBC")
    rename('TradingDate' = Clsdt)


# Merge the trading data with risk-free rate using 'lapply'
# and sort by trading date, then by stock symbol
trddat %<>% lapply(inner_join, Nrrate, by = 'TradingDate') %>% 
    lapply(arrange, TradingDate)


# Setup trading day arround QEA ====
trdday <- read_delim('TRD_Cale.csv', delim='\t', na = '',
     col_types = cols(Markettype = col_factor(c(1,2,4,8,16,32)),
          Clddt = col_date(format = "%Y-%m-%d"),
          # c(0:6): Sun < Mon < Tue < Wed < Thu < Fri < Sat
          Daywk = col_factor(c(0:6)),
          # "Open" or "Closed", stock share were traded or not
          State = col_factor(levels = c('C', 'O')))) %>% 
    # trading date in China A-Share markets
    filter(Markettype %in% as.character(c(1,4,16))) %>%  droplevels() %>% 
    rename('TradingDate' = Clddt) %>%
    # columns by market class
    spread(Markettype, State) %>% na.omit()
# confirm the trade calendar is same among different kinds of share in China A-share market
if(sum(all_equal(trdday$`1`, trdday$`4`),
    all_equal(trdday$`1`, trdday$`16`),
    all_equal(trdday$`4`, trdday$`16`)) == 3) {
    # we take the trading dates of Shanghai A-share as China A-share market's
    trdday %<>% filter(.$`1` == "O") %>% pull(TradingDate)
} else print("Attention! The trading dates among China A-share markets are different.")


# Import the status data of quarterly financial report 
ReptInfo <- read_delim('IAR_Rept.csv', delim='\t', na = '',
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
             Profita = col_double(), Erana = col_double())
       ) %>% arrange(Stkcd, Accper) %>% 
    # select the stocks which are belong to China A-Share markets,
    # we could observe that the symbols of these stocks are begin with number c(0, 3, 6),
    # so we use regular expression and string function 'grepl' to get them through function filter 
    filter(grepl('^[0-6]', Stkcd)) %>% na.omit() %>% 
    arrange(Stkcd, Accper)


# Input the listing date and idustry of stocks 
AF_Co <- read_delim('AF_Co.csv', delim='\t', na = '',
        col_types = cols_only(Stkcd = col_character(),
             # industry divide standard
             IndClaCd = col_factor(),
             # industry symbol
             Indus = col_character(), 
             # listed date
             Listdt = col_date('%Y-%m-%d'))) %>% 
    # just using the 2012 Edition Industry Classification
    # published by China Securities Regulatory Commission 
    filter(IndClaCd == 2) %>% select(-IndClaCd) 

AF_Co %>% count(year = str_sub(.$Listdt, 1, 4), 
            Industry = str_sub(.$Indus, 1, 1)) %>% 
    ggplot() +
    geom_col(aes(x = year, y = n, fill = Industry)) +
    labs(x="Year", y = "The count of firms listed in this year") +
    theme_bw()

ggsave(glue("{datdir}/Stock-Indus-His.pdf"),
       width = 16, height = 9, dpi = 300, units="in", limitsize = F) 

trddat %<>% keep(names(.) %in% pull(AF_Co, Stkcd)) %>% 
    lapply(inner_join, AF_Co, by = 'Stkcd') %>% 
    lapply(arrange, TradingDate)

# for convenience, wo store a image that contain all of above data
glue('{datdir}/PrePotfol.RData') %>% save.image()


######## Part II, reproduce Fama-French factor model by quarterly ########

# First, we set the quarter term.
# At there, we setup a data pool composing with six year panel (24 quarter)
# Notice that we should set one quarter ahead for structuring current invest portfolio
Accprd <- months(seq(0, by=3, length=24)) %>% 
    mapply('%m+%', ymd('2013-03-31'), .) %>% 
    as.Date(origin='1970-01-01') 

# create lists for storing our desired results
potfolstk <- potfolreg <- vector(mode = 'list', length = length(Accprd)) %>% 
    set_names(Accprd)

for(q in 1:length(Accprd)) { # loop in quarter
    
    print(Accprd[q])
    
    # seeking the fundamental accounting status of stocks ====
    # at quarter t-2 to structure current quarter portfolio, 
    # It is rational that investors make decision according to known recently report (quarter t-2)
    splday <- ahdqua <- (Accprd[q] + days(1)) %m+% months(-6) + days(-1)
    # c(1:7), Mon < Tue < Wed < Thu < Fri < Sat < Sun
    wday(splday, label = TRUE, week_start = getOption("lubridate.week.start", 1))
    # Note that we have confirmed that the trade dates in differents China A-share makets are same
    while (!splday %in% trdday) splday <- splday + days(-1)
        
    # eliminate the outer stocks ====
    # eliminating the the stocks which listed less than six months
    trddatl <- keep(trddat, ~ unique(pull(.x, Listdt)) %>% 
                       interval(splday) %>% `>=`(months(6)))

    # eliminating the the stocks which having less than 180 trading records in the past year
    # or less than 15 trading records in the past month.
    trddatl %<>% keep(~ '|'(filter(.x, TradingDate %within% interval(splday + years(-1), splday)) %>% 
                            nrow() >= 180L, 
                            filter(.x, TradingDate %within% interval(splday %m+% months(-1), splday)) %>% 
                            nrow() >= 15L)
                      )
    
    # save the reserved stocks as quarterly sample for calculating the abnormal return
    potfolstk[[q]] <- names(trddatl)
    
    
    # Fama-French three factor model (CH3) ====
    
    # Generate the calendar lists ====
    trddatc <- bind_rows(trddatl) %>% split(.$TradingDate)
    
    
    
    
    
    
    
    
    # the accounting status at the ending of quarter t-1
    RepInfo.q <- filter(ReptInfo, Accper == ahdqua) %>% 
        # company equty
        mutate(CmnEqty = Profita / Erana) %>% 
        # the number of share = net profit / earnings per share (EPS), CSMAR
        select(Stkcd, Erana, Accper, CmnEqty)
    
    
    ####### The explanatory variables, (mkt_rf, SMB,VMG) ########
    
    # Notice that we could set two cluster paths, HML or VMG, at there 
    # if we have sufficient accounting data
    
    # extract the closing price of stocks at the ending of quarter t-2 ====
    # and get the group index bsaed on size and value
    potfol <- pluck(trddatc, as.character(splday)) %>%
        # select(Stkcd, TradingDate, Clsprc) %>% 
        # notice that only the stocks, which published quarter financial report,
        # are brought into our sample to structure portfolio, 
        # it's also because of the limit of database
        inner_join(RepInfo.q, by='Stkcd') %>%
        mutate(# size, the number of shares product its closing price
               Size = Clsprc * CmnEqty, 
               # value, earnings-to-price
               Value = Erana / Clsprc,
               # add the accounting period 
               Accprd = Accprd[q]) 

    # we use the quantile of the indicators Size and Value to group stocks
    potfol1 <- mutate(potfol, 'g.SMB' = cut(Size, quantile(Size, c(0, 0.5, 1)), 
                                            labels=c("Small", "Big"),
                                            ordered_result = TRUE, include.lowest = TRUE)) %>% 
            mutate('g.VMG' = cut(Value, quantile(Value, c(0, 0.3, 0.7, 1)), 
                                 labels=c("Growth", "Middle", "Value"),
                                 ordered_result = TRUE, include.lowest = TRUE))
   
    # re-level the group structure
    potfol1$g.SMB %<>%  fct_relevel(c("Small", "Big"))
    potfol1$g.VMG %<>%  fct_relevel(c("Value", "Middle", "Growth"))
    
    # calculate the daily risk factors at quarter t ====
    trddatff <- bind_rows(trddatl) %>%  # unsplit stocks attributes
        #select(Stkcd, TradingDate, Dsmvosd, Dretnd, Nrrdaydt) %>% 
        # extract the time series interval for the t quarter
        filter(TradingDate %within% interval((Accprd[q]+days(1))%m+%months(-3), Accprd[q])) %>% 
        # merge with portfolio information
        # Notice that this step will abandon the stocks which are not included in our sample
        inner_join(select(potfol1, Stkcd, g.SMB, g.VMG), by = "Stkcd") %>% 
        # lapply(filter, Stkcd %in% pull(potfol1, Stkcd)) %>% 
        # Generate the calendar ordered lists
        split(.$TradingDate) 
    
    # mkt_rf ====
    # the returns of market risk subtracted risk-free rate of that day
    mkt_rf <- sapply(trddatff, function(x) {
        # calculate the factor MKT
        with(x, (Dsmvosd / sum(Dsmvosd)) %*% Dretnd) %>% 
        # minus the risk-free rate of market 
        '-'(unique(x$Nrrdaydt))}) 
    
    
    # SMB and VMG  ====
    # firstly, we calculate the group weighted returns of different portfolios
    trddatg.Ret <- trddatff %>% lapply(group_by, g.SMB, g.VMG) %>% 
        # mean daily returns of stocks within a group
        lapply(summarise, Ret = mean(Dretnd)) %>% 
        # # weighted by Dsmvosd
        # lapply(summarise, Ret = Dretnd %*% (Dsmvosd /sum(Dsmvosd))) %>% 
        bind_rows() 
    # manually add a trading date column, maybe there exist a sequence bug...
    trddatg.Ret %<>% add_column(TradingDate = rep(names(trddatff), each=2*3) %>% ymd())
    
    # data-visualization
    figureFF <- multi_panel_figure(
        width = 16, height = 18,
        unit = "in",
        columns = 1, rows = 2)
    
    FFSMB <- ggplot(trddatg.Ret) +
        geom_line(aes(x=TradingDate, y=Ret, colour = g.SMB)) +
        labs(title = "Portfolios structured in size (Capitalization)",
             y="Daily return of portfolios Weighted by circulation market value") +
        theme_bw() +
        theme(plot.title = element_text(face = "bold"),
              axis.title.x = element_blank(),
              legend.position = c(0.05, 0.9),
              legend.title = element_blank(),
              legend.text = element_text(size = 14))
    
    FFVMG <- ggplot(trddatg.Ret) +
        geom_line(aes(x=TradingDate, y=Ret, colour = g.VMG)) +
        labs(title = "Portfolios structured in value (Earnings-to-price)",
             y="Daily return of portfolios Weighted by circulation market value") +
        theme_bw() +
        theme(plot.title = element_text(face = "bold"),
              axis.title.x = element_blank(),
              legend.position = c(0.05, 0.9),
              legend.title = element_blank(),
              legend.text = element_text(size = 14))
    
    figureFF %<>% 
        fill_panel(FFSMB, column = 1, row = 1) %<>%
        fill_panel(FFVMG, column = 1, row = 2) %>% 
        save_multi_panel_figure(glue("{datdir}/{Accprd[q]}_figure_portfolio_Retrun.pdf"),
                                dpi = 300, limitsize = F)

    # group-date to calculate the mean returns of portfolios, it's a magic!
    trddatg.Ret %<>% group_by(TradingDate)
    # then we take the mean between with portfolios
    SMB <- `-`(filter(trddatg.Ret, g.SMB == "Small") %>% summarise(Ret = mean(Ret)) %>% pull(Ret),
               filter(trddatg.Ret, g.SMB == "Big") %>% summarise(Ret = mean(Ret)) %>% pull(Ret)) %>% 
        set_names(names(trddatff))
    
    VMG <- `-`(filter(trddatg.Ret, g.VMG == "Value") %>% summarise(Ret = mean(Ret)) %>% pull(Ret),
               filter(trddatg.Ret, g.VMG == "Growth") %>% summarise(Ret = mean(Ret)) %>% pull(Ret)) %>% 
        set_names(names(trddatff))
    
    
    ####### The dependent variable, weighted daily returns of 5*5 portfolios #####
    
    # structure the intersection 5*5 stock portfolios 
    potfol2 <- mutate(potfol, 'g.SMB' = cut(Size, quantile(Size, seq(0, 1, by=0.2)), 
                                            labels=c("Small", "2", "3", "4", "Big"),
                                            ordered_result = TRUE, include.lowest = TRUE)) %>% 
        mutate('g.VMG' = cut(Value, quantile(Value, seq(0, 1, by=0.2)), 
                           labels=c("Growth", 2, 3, 4, "Value"),
                           ordered_result = TRUE, include.lowest = TRUE)) %>% 
        `[`(complete.cases(.$g.SMB, .$g.VMG),)
    
    # re-level the group structure
    potfol1$g.SMB %<>%  fct_relevel(c("Small", "2", "3", "4", "Big"))
    potfol1$g.VMG %<>%  fct_relevel(c("Value", "2", "3", "4", "Growth"))

    # merge the group status with the trading data
    trddatff <- bind_rows(trddatl) %>%  # unsplit stocks attributes
        #select(Stkcd, TradingDate, Dsmvosd, Dretnd, Nrrdaydt) %>% 
        # extract the time series interval for the t quarter
        filter(TradingDate %within% interval((Accprd[q]+days(1))%m+%months(-3), Accprd[q])) %>% 
        # merge with portfolio information
        # Notice that this step will abandon the stocks which are not included in our sample
        inner_join(select(potfol2, Stkcd, g.SMB, g.VMG), by = "Stkcd") %>% 
        # lapply(filter, Stkcd %in% pull(potfol1, Stkcd)) %>% 
        # Generate the calendar ordered lists
        split(.$TradingDate) 
    
    # caluculate the daily returns of portfolios weighted by circulation market value
    trddatg.Ret <- lapply(trddatff, group_by, g.SMB, g.VMG) %>% 
        lapply(summarise, Ret = mean(Dretnd)) %>% 
        # # weighted by Dsmvosd
        # lapply(summarise, Ret = Dretnd %*% (Dsmvosd /sum(Dsmvosd))) %>% 
        bind_rows()
    # manually add a trading date column, maybe there exist a sequence bug...
    trddatg.Ret %<>% add_column(TradingDate = rep(names(trddatff), each=5*5) %>% ymd())
    
    # retain the daily regression data
    trddatqg <- tibble("TradingDate" = names(trddatff) %>% ymd(),
           'mkt_rf' = mkt_rf,
           'SMB' = SMB,
           'VMG' = VMG) %>% 
        inner_join(trddatg.Ret, by = "TradingDate") %>% 
        inner_join(Nrrate, by = "TradingDate")
    
    
    ############## Regression (quarterly) ############
    
    # Fama-French multiple-factor model
    mod_lm <- function(df) lm( I(Ret - Nrrdaydt) ~ mkt_rf + SMB + VMG, data = df)
    # run time series regression by portfolios
    potfollm <- group_nest(trddatqg, g.SMB, g.VMG) %>%
        mutate(model = map(data, mod_lm))
    
    # coefficients of regression results ====   
    potfollm %<>% mutate(Coef = map(model, tidy))

    # save the regression results
    potfolreg[[q]] <- potfollm
    
}

save(potfolstk, potfolreg, file = glue("{datdir}/Hu-CH3.RData"))
