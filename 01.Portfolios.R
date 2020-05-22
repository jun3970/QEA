# Summary of this Script ====
# CC Hongyu Hu
# 2018.04--2019.10
# 
# This R script is writen for obtaining the 5*5 stock portfolios
# and 2*3 mimicing risk factors formed on size and value 
# in Chinese A-share markets, these stocks symbol begin with 00, 30, 60.
# 
# The quantity of a stock *size* is computed as the end of
# previous quarter’s closing price times its total shares.
# A stock’s value (earnings-to-pric, EP) is the ratio of 
# the quarterly earnings which is the most recently reported net profit 
# excluding nonrecurrent gains/losses in last quarter 
# to the product of its closing price and total shares at ending of last quarter.
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

library(tidyverse)
library(lubridate)
library(magrittr)
library(glue)
library(broom)

library(DBI)
library(RSQLite)
library(dbplyr)

####### Part I, import data ########
# working directory
setwd('~/OneDrive/Data.backup/QEAData/')
# link to SQLite database
QEA_db <- dbConnect(SQLite(), "QEA_db.sqlite")
# Import the daily trading in China A-Share markets
trddat <- dbGetQuery(QEA_db, 
        "SELECT Stkcd, TradingDate, Clsprc, Dretnd, Dsmvosd, Markettype
        FROM daily")
# transform the record of trading date to date form
trddat$TradingDate %<>% as.Date(origin = "1970-01-01")
                                                        
# Import the daily one-year-deposit-interest-rate, free-risk return in market 
Nrrate <- read_delim('TRD_Nrrate.csv', delim = '\t', na = '',
        col_types = cols_only(Nrrdaydt = col_double(),
            Nrr1 = col_factor(levels = c('NRI01', 'TBC')),
            Clsdt = col_date(format = '%Y-%m-%d'))) %>%
        # select the one-year deposit interest rate as the free-risk return
        filter(Nrr1 == 'NRI01') %>% select(-Nrr1) %>% 
        # # we could either select the Treasury rate as risk-free interest
        # filter(Nrr1 == "TBC") %>% select(- Nrr1) %>%
        rename('TradingDate' = Clsdt)

# Merge the trading data with risk-free rate
trddat %<>% split(.$Stkcd) %>%  # split by stock
        lapply(left_join, Nrrate, by = 'TradingDate')

# Input the listing date and idustry of stocks 
# for wiping out the stocks listed newly 
AF_Co <- read_delim('AF_Co.csv', delim = '\t', na = '',
        col_types = cols_only(Stkcd = col_character(),
                IndClaCd = col_factor(),  # industry divide standard
                Indus = col_character(),  # industry symbol
                Listdt = col_date('%Y-%m-%d'))) %>% 
        # just using the 2012 Edition Industry Classification
        # published by China Securities Regulatory Commission 
        filter(IndClaCd == 2) %>% select(-IndClaCd) 
# take a look 
count(AF_Co, year = str_sub(Listdt, 1, 4), Industry = str_sub(Indus, 1, 1)) %>% 
        ggplot() +
        geom_col(aes(x = year, y = n, fill = Industry)) +
        labs(x = "Year", y = "The count of firms listed in this year") +
        theme_bw()
# export this image
ggsave(glue("./CH3/Stock-Indus-His.pdf"),
       width = 16, height = 9, dpi = 300, units = "in", limitsize = F) 

# join the information of stocks listed date to trading data
trddat %<>% `[`(intersect(names(.), pull(AF_Co, Stkcd))) %>% 
        lapply(inner_join, AF_Co, by = 'Stkcd') %>% 
        lapply(arrange, TradingDate)


# the structure of shares of stocks (annual capitialzation)
Nshr <- read_delim("./Acc_Annual/CG_Capchg.txt", delim = '\t', na = '',
        col_types = cols_only(Stkcd = col_character(),
                 Reptdt = col_date(format = "%Y-%m-%d"),  # the annual interval
                 Nshrnn = col_double(),  # non-circulation
                 Nshra = col_double())  # A-shares under circulation
        ) %>% # take a sum of non-circulation shares and A-shares
        transmute(Stkcd, Reptdt, shrttl = Nshrnn + Nshra) %>% 
        # for merge convinence, just extraction the year of report date (annual data)
        mutate(Reptdt = str_sub(Reptdt, 1L, 4L)) %>% 
        arrange(Reptdt)


# Setup trading day arround QEA ====
trdday <- read_delim('TRD_Cale.csv', delim = '\t', na = '',
        col_types = cols(Markettype = col_factor(c(1,2,4,8,16,32)),
                Clddt = col_date(format = "%Y-%m-%d"),
                # c(0:6): Sun < Mon < Tue < Wed < Thu < Fri < Sat
                Daywk = col_factor(c(0:6)),
                # "Open" or "Closed", stock share were traded or not
                State = col_factor(levels = c('C', 'O'))
                )
        ) %>% rename('TradingDate' = Clddt) %>%
        # trading date in China A-Share markets
        filter(Markettype %in% as.character(c(1,4,16))) %>% droplevels() %>% 
        # columns by market class
        spread(Markettype, State) %>% na.omit()
# confirm the trade calendar is same among different kinds of share in China A-share market
if (sum(all_equal(trdday$`1`, trdday$`4`),
       all_equal(trdday$`1`, trdday$`16`),
       all_equal(trdday$`4`, trdday$`16`)) == 3L
   ) {  # we take the trading dates of Shanghai A-share as China A-share market's
        trdday %<>% filter(.$`1` == "O") %>% pull(TradingDate)
} else print("Attention! The trading dates among China A-share markets are different.")

# for tidy convenience, we store above data as a image  
save.image('./CH3/PrePotfol.RData')


######## Part II, reproduce Fama-French multi-factor model ########
# set the type of multi-factor model
# yearly, FF5 (Fama-French, 2015)
# quarterly, CH3 (Liu, 2018)
model_type <- "CH3"

# set the type of Value: EPS, BVPS, or CFPS
# That is we could select a index from these three accounting indicator 
# as the cluster standard of factor `Value` in multi-factor model
# EPS: earings-to-price
# BVPS: book-to-market (equity)
# CFPS: the radio of cash flow to closing price
Value_type <- "EPS"

# set the quarter term
# Notice that we should set `two quarters ahead`
# to structure the invest portfolio at current quarter 
len_term <- 12L
start_term <- ymd('2016-03-31')


# accounting periods we want to process
if (model_type == "CH3") {  # quarterly
    
        Accprd <- months(seq(0, by = 3, length = len_term)) %>% 
                mapply('%m+%', start_term, .) %>% 
                as.Date(origin = '1970-01-01') 
        
} else if (model_type == "FF5") {
    
        Accprd <- years(seq(0, by = 1, length = len_term)) %>% 
                mapply('+', start_term, .) %>% 
                as.Date(origin = '1970-01-01')
        
}

# Fama-French multiple-factor model
lm_CH3 <- function(df) lm( I(ptf_Ret - Nrrdaydt) ~ mkt_rf + SMB + VMG, data = df)
lm_FF5 <- function(df) lm( I(ptf_Ret - Nrrdaydt) ~ mkt_rf + SMB + VMG + RMW + CMW, data = df)

# create lists to store our results
potfolreg <- vector(mode = 'list', length = length(Accprd)) %>% 
    set_names(Accprd)


for (q in 1:length(Accprd)) { # loop in quarter
        
# It is rational that investors make decision according to accounting index
# in earnings report of quarter `q-2`, they known recently,  at the begining of quater `q`. 
# so we using the accounting indicators at `q-2` to structure portfolios at quarter `q`
    
    print(Accprd[q])
    
    if (model_type == "CH3") {
            # seeking the fundamental accounting status of stocks at quarter t-2
            # to structure current quarter portfolio
            splday <- ahdqua <- (Accprd[q] + days(1)) %m+% months(-6) + days(-1)
    } else if (model_type == "FF5") {
            # using the accounting indicator at year y-1
            # to structure current year portfolio
            splday <- ahdqua <- Accprd[q] + years(-1)
    }
    
    # c(1:7), Mon < Tue < Wed < Thu < Fri < Sat < Sun
    wday(splday, label = TRUE, week_start = getOption("lubridate.week.start", 1))
    # we need to confirm that the trade dates in differents China A-share makets are same
    while (!splday %in% trdday) splday <- splday + days(-1)
    
    # eliminate stocks to enable reasonable precision and power ====
    trddats <- trddat %>% 
    # eliminating the stocks which listed less than six months
    keep(~ `>=`(interval(unique(pull(.x, Listdt)), Accprd[q]), months(6))) %>% 
    keep(~ `&`(filter(.x, TradingDate %within% interval(splday + years(-1), splday)) %>% 
                    nrow() >= 180L,  # less than 180 trading records in the past year
               filter(.x, TradingDate %within% interval(splday %m+% months(-1), splday)) %>% 
                    nrow() >= 15L))  # less than 15 trading records in the past month
  
    # import the information of earnings report
    if (model_type == "CH3") {
            # earnings report at quarter t-2 
            ReptInfo_Acc <- tbl(QEA_db, "quarter") %>% 
                    filter(Accper == !!as.integer(ahdqua)) %>% 
                    filter(Stkcd %in% !!names(trddats)) %>% 
                    filter(Typrep == "A") %>%  # union report
                    select(Stkcd, Accper, F020105) %>% 
                    collect() %>% 
                    arrange(Stkcd, Accper)
            
            ReptInfo_Acc %<>% rename("EPS" = F020105) 
    
    } else if (model_type == "FF5") {
            # earnings report at year y-1
            ReptInfo_Acc <- tbl(QEA_db, "quarter") %>% 
                    filter(Accper == !!as.integer(ahdqua)) %>% 
                    filter(Stkcd %in% !!names(trddats)) %>% 
                    filter(Typrep == "A") %>%  
                    # select(Stkcd, Accper, F020105, F020106, F020107, F020104, F080602A) %>% 
                    select(Stkcd, Accper, F020105, F020104, F080602A) %>% 
                    collect() %>% 
                    arrange(Stkcd, Accper)
    
            ReptInfo_Acc %<>% rename("EPS" = F020105,
                    "Profitability" = F020104, "Investment" = F080602A)
    }
    
    ReptInfo_Acc$Accper %<>% as.Date(origin = "1970-01-01")
    
    trddats %<>% `[`(intersect(names(.), pull(ReptInfo_Acc, Stkcd)))
    
    # extract the closing price of stocks at the ending of quarter t-2 ====
    # and get the group index bsaed on factors size and value
    # Notice that only the stocks, which published quarter financial report,
    # are brought into our sample to structure portfolio
    potfol <- lapply(trddats, filter, TradingDate == ymd(splday)) %>% 
            bind_rows() %>% inner_join(ReptInfo_Acc, by = "Stkcd") %>% 
            mutate("Reptdt" = str_sub(TradingDate, 1L, 4L)) %>% 
            inner_join(Nshr, by = c("Stkcd", "Reptdt")) %>% 
            select(-Reptdt) %>% 
            mutate(# size, the number of shares product its closing price
                   Size = Clsprc * shrttl, 
                   # value, at there we have three cluster paths, EPS, BVPS, VFPS
                   Value = get(!!Value_type) / Clsprc ) %>% 
            filter("=="(rowSums( is.na(.) ), 0L)) 
            
    # There is a latent problem we should pay attention to,
    # the time terms of stocks in our sample (potfol)
    # aren't same with market trading dates at all (non-trading).
    
    # extract the current interval trading data of stocks
    if (model_type == "CH3") {  
            # extract the time series data at quarter `q`
            trdff <- trddats[pull(potfol, Stkcd)] %>%  
                    lapply(select, Stkcd, TradingDate, Dretnd, Dsmvosd, Nrrdaydt) %>% 
                    lapply(filter, TradingDate %within% 
                            interval((Accprd[q] + days(1)) %m+% months(-3), Accprd[q])) %>% 
                    bind_rows()
            
    } else if (model_type == "FF5") {  
            # extract the time series data at year `y`
            trdff <- trddats[pull(potfol, Stkcd)] %>%  
                    lapply(select, Stkcd, TradingDate, Dretnd, Dsmvosd, Nrrdaydt) %>% 
                    lapply(filter, TradingDate %within% 
                            interval(Accprd[q] + years(-1), Accprd[q])) %>% 
                    bind_rows()
    }
    
    
    ####### The explanatory variables, (mkt_rf, SMB, VMG...) ########
    
    # we use the quantile of the indicators Size and Value to group stocks
    if (model_type == "CH3") {
            
            potfol_x <- mutate(potfol, 
                    'g.SMB' = cut(Size, quantile(Size, c(0, 0.5, 1)), 
                                labels = c("Small", "Big"),
                                ordered_result = TRUE, include.lowest = TRUE), 
                    'g.VMG' = cut(Value, quantile(Value, c(0, 0.3, 0.7, 1)), 
                                labels = c("Growth", "V.Neutral", "Value"),
                                ordered_result = TRUE, include.lowest = TRUE)
                    )
            
            # re-level the group structure
            potfol_x$g.SMB %<>%  fct_relevel(c("Small", "Big"))
            potfol_x$g.VMG %<>%  fct_relevel(c("Value", "V.Neutral", "Growth"))
            
    } else if (model_type == "FF5") {
    
            potfol_x <- mutate(potfol, 
                    'g.SMB' = cut(Size, quantile(Size, c(0, 0.5, 1)), 
                        labels = c("Small", "Big"),
                        ordered_result = TRUE, include.lowest = TRUE), 
                    'g.VMG' = cut(Value, quantile(Value, c(0, 0.3, 0.7, 1)), 
                        labels = c("Growth", "V.Neutral", "Value"),
                        ordered_result = TRUE, include.lowest = TRUE),
                    'g.RMW' = cut(Profitability, quantile(Profitability, c(0, 0.3, 0.7, 1)),
                        labels = c("Robust", "P.Neutral", "Weak"),
                        ordered_result = TRUE, include.lowest = TRUE),
                    'g.CMA' = cut(Investment, quantile(Investment, c(0, 0.3, 0.7, 1)),
                        labels = c("Conservative", "I.Neutral", "Aggressive"),
                        ordered_result = TRUE, include.lowest = TRUE)
                    )
    }
    
    
    # join the trading data of stocks with portfolio structure (cluster)
    # Notice that this step will abandon the stocks which are not included in our sample
    if ( model_type == "CH3" ) {
            
            trdff_x <- inner_join(trdff, 
                    select(potfol_x, Stkcd, g.SMB, g.VMG), by = "Stkcd") %>% 
                    group_nest(TradingDate)  # Generate the calendar ordered column-list
    
            trdff_x$data %<>% map(group_by, g.SMB, g.VMG)
            
    } else if ( model_type == "FF5" ) {
            
            trdff_x <- inner_join(trdff, 
                    select(potfol_x, Stkcd, g.SMB, g.VMG, g.RMW, g.CMA), by = "Stkcd") %>% 
                    group_nest(TradingDate)  
            
            trdff_x$data %<>% map(group_by, g.SMB, g.VMG, g.RMW, g.CMA) 
    
    }
    
    # calculate the daily risk factors ====
    
    # mkt_rf, the returns of market risk subtracted risk-free rate of that day 
    trdff_x %<>% mutate("mkt_rf" = map_dbl(data, 
            ~ with(.x, (Dsmvosd / sum(Dsmvosd)) %*% Dretnd - unique(Nrrdaydt)))) 
    
    
    # calculate the weighted returns of different portfolios
    trdff_x %<>% mutate("Portfolio.Ret" = map(data, 
                    ~ summarise(.x, Ret = Dretnd %*% (Dsmvosd / sum(Dsmvosd)))))
    
    if (model_type == "CH3") {
            
            trdff_x %<>% mutate(
                    "SMB" = map_dbl(Portfolio.Ret, ~ group_by(.x, g.SMB) %>% 
                                    summarise("avgRet" = mean(Ret)) %>% 
                                    spread(g.SMB, avgRet) %>% 
                                    with(., Small - Big)),
                    
                    "VMG" = map_dbl(Portfolio.Ret, ~ group_by(.x, g.VMG) %>% 
                                    summarise("avgRet" = mean(Ret)) %>% 
                                    spread(g.VMG, avgRet) %>% 
                                    with(., Value - Growth))
                    )
            
    } # else if(model_type == "FF5") {}
    
    
    # data-visualization, plot a line figure ====
    if (model_type == "CH3") {
    
    select(trdff_x, TradingDate, Portfolio.Ret) %>% 
            unnest(cols = Portfolio.Ret) %>% 
            filter(g.VMG != "V.Neutral") %>% 
    ggplot() +
            geom_line(aes(TradingDate, Ret, colour = g.SMB, linetype = g.VMG)) +
            labs(title = "Daily returns of portfolios Weighted by circulation market value",
                 y = "Daily returns") +
            scale_color_brewer(palette = "Set1") +
            theme_bw() +
            theme(plot.title = element_text(face = "bold"),
                  axis.title.x = element_blank(),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 14))
        
    } # else if(model_type == "FF5") {}
    
    ggsave(glue("./CH3/figure_portfolio_return/{Accprd[q]}_figure_portfolio_return.pdf"),
       width = 16, height = 9, dpi = 300, units = "in", limitsize = F)
    
    
    ##### The dependent variable, weighted daily returns of 5*5 portfolios #####
    
    # structure the intersection 5*5 stock portfolios 
    if (model_type == "CH3") {
        
            potfol_y <- mutate(potfol, 
                       'g.SMB' = cut(Size, quantile(Size, seq(0, 1, by = 0.2)), 
                                        labels = c("Small", "2", "3", "4", "Big"),
                                        ordered_result = TRUE, include.lowest = TRUE),
                       
                       'g.VMG' = cut(Value, quantile(Value, seq(0, 1, by = 0.2)), 
                                        labels = c("Growth", 2, 3, 4, "Value"),
                                        ordered_result = TRUE, include.lowest = TRUE)
                       ) 
            
            # re-level the group structure
            potfol_y$g.SMB %<>%  fct_relevel(c("Small", "2", "3", "4", "Big"))
            potfol_y$g.VMG %<>%  fct_relevel(c("Value", "2", "3", "4", "Growth"))
            
    } # else if(model_type == "FF5") {}
    
    
    
    # join the trading data of stocks with portfolio structure (cluster)
    # Notice that this step will abandon the stocks which are not included in our sample
    if (model_type == "CH3") {
            
            portfolio_ret <- inner_join(trdff, 
                    select(potfol_y, Stkcd, g.SMB, g.VMG), by = "Stkcd") %>% 
                    # Generate the calendar ordered lists
                    group_by(TradingDate) %>% nest() %>%  
                    # caluculate the daily returns of portfolios 
                    # weighted by circulation market value
                    mutate("data" = map(data, group_by, g.SMB, g.VMG)) %>% 
                    transmute("p.Ret" = map(data, 
                            ~ summarise(.x, "ptf_Ret" = Dretnd %*% (Dsmvosd /sum(Dsmvosd)))
                            )) %>% 
                    unnest(cols = "p.Ret")
            
    } # else if(modle_type == "FF5") {}
    
    # retain the daily data for running regression
    if (model_type == "CH3") {
            
            trd_reg <- inner_join(portfolio_ret, 
                            select(trdff_x, TradingDate, mkt_rf, SMB, VMG), 
                            by = "TradingDate") %>% 
                    inner_join(Nrrate, by = "TradingDate")
            
    } else if (model_type == "FF5") {
            
            trd_reg <- inner_join(portfolio_ret, 
                            select(trdff_x, TradingDate, mkt_rf, SMB, VMG, RMW, CMA), 
                            by = "TradingDate") %>% 
                    inner_join(Nrrate, by = "TradingDate")
            
    }
    
    ############## Regression (quarterly) ############
    
    
    if (model_type == "CH3") {
            
            potfolreg[[q]] <- group_by(trd_reg, g.SMB, g.VMG) %>% nest() %>%  
                    # run time series regression by portfolios
                    mutate("lm_CH3_result" = map(data, lm_CH3)) %>% 
                    # coefficients of regression results ====   
                    mutate("Coef" = map(lm_CH3_result, tidy))
    
    } else if (model_type == "FF5") {
            
            potfolreg[[q]] <- group_by(trd_reg, g.SMB, g.VMG, g.RMW, g.CMA) %>% nest() %>%  
                    # run time series regression by portfolios
                    mutate("lm_FF5_result" = map(data, lm_FF5)) %>% 
                    # coefficients of regression results ====   
                    mutate("Coef" = map(lm_FF5_result, tidy))
            
    }
                
}

save(potfolreg, file = glue("./CH3/Hu-CH3.RData"))

# portfolio estimate to generate table
pft_est <- map(potfolreg, ~ select(.x, c("g.SMB", "g.VMG", "Coef"))) %>% 
        bind_rows(.id = "Accprd") %>% 
        unnest(cols = "Coef")

# factors
# map(potfolreg, "data") %>% flatten_dfr() %>% select(- ptf_Ret) %>% unique.data.frame()
factor_exp <- map(potfolreg, "data") %>% 
        # the factors data are same among portfolios every quarter, we just need one 
        map(`[`(1L)) %>%
        bind_rows() %>% 
        select(-ptf_Ret)

