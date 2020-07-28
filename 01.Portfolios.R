# This R script is written to replicate the quarterly empirical results of 
# Fama and French (1993, 2015) in Chinese A-share markets.
#
# The explanation factors ------------------------------------------------
#
# The return is positive correlation with risk.
# To explanation the cross-sectional variation of the daily returns of stocks,
# we construct multiple factors mimicking the risk of some accounting dimensions in firm.
#
# MKT_RF (CAPM)
# The market risk factor, MKT_RF, is the return on the systemic invest risk
# in Chinese A-Share Markets (a value-weighted portfolio formed with all stocks)
# in excess of the current period one-year deposit interest rate by daily.
# We weight each stock returns by its market capitalization.
#
# Structure some diversified portfolios ====
#
# SIZE | SMB 
# The capitalization of a listed firm are called as `size` from stock angle.
# It is computed as the closing price at the end trading date of ahead quarter(s)
# times its outstanding A shares, including non-tradeable shares.
# Then we use the median value of size of stocks in Chinese A-Share markets to
# split stocks into two groups, Small and Big. The corresponding factor is SMB, 
# which are calculated as the daily risk return of small stocks minus big's.
#
# VALUE | VMG(HML)
# Value measure the relative value level of a stock, like as sharp ratio.
# At there, we could define a variety of index of `value`.
# for example, when we using B/M (Fama, 1993) to construct portfolios 
# we get factor VMG, B/M is defined as book value to market value of a stock. 
# Otherwise, using EP (Liu, 2018) to construct portfolios we get factor VMG, 
# and EP is defined as the earnings per share to price of a stock.
#
# In detail, the book value at below code is the net asset for fiscal year t-1,
# the market value equal to the size in factor SMB. 
# On the other hand, EP is calculated as the ratio of the quarterly earnings,
# as the net profit excluding non-recurrent gains/losses,
# to the product of its closing price and total shares.
# 
# To obtain the factor, VMG (value minus growth), We cluster stocks into  
# three groups based on the breakpoints of earnings-to-price, 
# bottom 30% (Growth, G), middle 40% (Medium, M), and top 30% (Value, V). 
# The process of build factor HML is same with it, expect the we split
# the stocks into there groups, High, neutral, and Low. 
#
# Notice that the difference of time interval, 
# VMG, Value (high value of EP) to Growth for fiscal quarter t-1 
# HML, High B/M to Low B/M for fiscal year t-1
#
# Profitability | RMW
# We use the accounting index ROE, known as earnings-to-book equity, 
# to structure the factor RMW representing the risk of profitability. 
# The process is same with above factor's. The earnings is the net earnings  
# in the quarterly earnings report quarter ago(s), equity is the value of 
# total assets minus liabilities. And we split the stocks into three groups,
# Robust(top 30%), neutral(Medium 40%), Weak(bottom 30%).
#
# Investment | CMA
# Using the annual total asset growth rate to split the stocks into three groups,
# Aggressive(top 30%), neutral(Medium 40%), Conservative(bottom 30%).
# 
# 
# Calculate the factors to mimicking risk ====
#
# The value of factors is the difference risk returns among diversified portfolios,
# constructed by taking intersect of portfolios in different dimension.
# 
# three factor model: R_rf ~ mkt_rf + SMB + VMG
#
# SMB = 1/3 (Small Value + Small Neutral + Small Growth) –
#       1/3 (Big Value + Big Neutral + Big Growth)
#
# VMG = 1/2 (Small Value + Big Value) –
#       1/2 (Small Growth + Big Growth)
#
# four factor model: R_rf ~ mkt_rf + SMB + VMG + RMW
#
# SMB_Value = 1/3 (Small Value + Small Neutral + Small Growth) –
#       1/3 (Big Value + Big Neutral + Big Growth)
#
# SMB_Profitability = 1/3 (Small Robust + Small Neutral + Small Weak) –
#       1/3 (Big Robust + Big Neutral + Big Weak)
#
# SMB = (SMB_Value + SMB_Profitability) / 2
#
# RMW = 1/2 (Small Robust + Big Robust) –
#       1/2 (Small Weak + Big Weak)
#
# Fama-French five factor model: R_rf ~ mkt_rf + SMB + VMG + RMW + CMA
#
# SMB_Investment = 1/3 (Small Aggressive + Small Neutral + Small Conservative) –
#       1/3 (Big Aggressive + Big Neutral + Big Conservative)
#
# SMB = (SMB_Value + SMB_Profitability + SMB_Investment) / 3
#
# HML = 1/2 (Small High + Big High) –
#       1/2 (Small Low + Big Low)
#
# CMA = 1/2 (Small Conservative + Big Conservative) –
#       1/2 (Small Aggressive + Big Aggressive)
#
# At last, it is worthy to emphasize that when we structure the value-weighted 
# portfolios at quarter 'q', if we using the available accounting data 
# in accounting report at quarter `q-1`, the meanings of the multiple factor 
# model is to explain the variance of cross-sectional returns of stocks.
# but if we using the data in quarter `q-2`, the aim is to make forecasting.

library(tidyverse)
library(lubridate)
library(magrittr)

library(glue)
library(broom)
library(gt)
library(RColorBrewer)

library(DBI)
library(RSQLite)
library(dbplyr)

# Fama-French multiple-factor model
lm_ff <- function(ff_class = "CH3", df, ...){
    
    if(ff_class == "CH3") {
        ff_formula <- as.formula(I(ptf_Ret - Nrrdaydt) ~ mkt_rf + SMB + VMG)
    } else if(ff_class == "CH4") {
        ff_formula <- as.formula(I(ptf_Ret - Nrrdaydt) ~ mkt_rf + SMB + VMG + RMW)
    } else if(ff_class == "FF5") {
        ff_formula <- as.formula(I(ptf_Ret - Nrrdaydt) ~ mkt_rf + SMB + HML + RMW + CMA)
    }
    
    return(lm(formula = ff_formula, data = df, ...))
}


# Part I, assign the basic parameter -------------------------------------

# set the type of multi-factor model ====
model_type <- "CH3"
# yearly -- FF5 (Fama-French, 2015), quarterly -- CH3 & CH4 (Liu, 2018)

# set the type of Value: EPS, or CFPS ====
value_base <- "CFPS"
# we could select a index from three accounting indicators
# as the cluster standard of factor `Value` in multi-factor model,
# EPS: earnings-to-price; CFPS: the radio of cash flow to closing price

# Forecasting or explanation? ====
ahead_quarter <- 1L
# We should using how many quarter ahead accounting reports information 
# to structure current quarter portfolio?
# one ahead for explanation, two ahead for forecasting

# set the quarter term ====
len_term <- 1L
start_term <- ymd('2013-03-31')
## 03-31, the first quarter; 06-30, the second quarter 
## 09-30, the third quarter; 12-31, the fourth quarter


# Part II, replicate Fama-French multi-factor model -----------------------

# working directory
setwd('~/OneDrive/Data.backup/QEAData/')
# import data
load(file = "./PrePotfol.RData")
# create directory to save script results
if (!dir.exists(file.path(model_type, "figure_portfolio_return"))) {
        dir.create(file.path(model_type, "figure_portfolio_return"))
} 
# link to database
QEA_db <- dbConnect(RSQLite::SQLite(), "./QEA_db.sqlite")
# the regression model and according periods will be running 
if (model_type == "CH3") {
        ff_term <- c("SMB", "VMG")
        Accprd <- months(seq(from = 0, by = 3, length = len_term)) 
} else if (model_type == "CH4") {
        ff_term <- c("SMB", "VMG", "RMW")
        Accprd <- months(seq(from = 0, by = 3, length = len_term)) 
} else if (model_type == "FF5") {
        ff_term <- c("SMB", "HML", "RMW", "CMA")
        Accprd <- years(seq(from = 0, by = 1, length = len_term)) 
}
Accprd %<>% mapply('%m+%', start_term, .) %>% as.Date(origin = '1970-01-01') 
# create lists to store our results
potfolreg <- vector(mode = 'list', length = length(Accprd)) %>% 
        set_names(Accprd)

for (q in 1:length(Accprd)) { # loop in quarter

# It is rational that the investors make decisions according to 
# the accounting indicators in earnings report they known recently. 
# At the beginning of quarter `q`, they know the information of quarter `q-2`.
# But for explanation maximize, we use the accounting indicators at `q-1` 
# to structure portfolios and explanation factors at quarter `q`.
    
    print(Accprd[q])
    
    if (model_type %in% c("CH3", "CH4")) {
            # seeking the fundamental accounting status of stocks 
            # at ahead quarter to structure current quarter portfolio
            splday <- ahdqua <- 
                (Accprd[q] + days(1)) %m-% months(ahead_quarter * 3L) + days(-1)
    } else if (model_type == "FF5") {
            # using the accounting indicator at year y-1
            # to structure current year portfolio
            splday <- ahdqua <- Accprd[q] + years(-1)
    }
    
    while (!splday %in% trdday) splday <- splday + days(-1)
    
    # eliminate stocks to enable reasonable precision and power ====
    trddat %<>% 
        keep(~ `>=`(interval(unique(pull(.x, Listdt)), Accprd[q]), 
                    months(6))  # eliminating the stocks which listed less than six months
             ) %>% 
        keep(~ `&`(filter(.x, TradingDate %within% interval(splday + years(-1), splday)) %>% 
                        nrow() >= 180L,  # less than 180 trading records in the past year
                   filter(.x, TradingDate %within% interval(splday %m+% months(-1), splday)) %>% 
                        nrow() >= 15L)  # less than 15 trading records in the past month
             )

    # import the information of earnings report ====
    ## There is a shortcoming in this part, we will just select  
    ## the earnings reports of parent listed company to analysis (Typrep == "A")
    
    if (model_type == "CH3") {
            
            if (value_base == "EPS") {
                
                    ReptInfo_Acc <- tbl(QEA_db, "quarter") %>% 
                            filter(Accper == !!as.integer(ahdqua)) %>% 
                            filter(Stkcd %in% !!names(trddat)) %>% 
                            filter(Typrep == "A") %>%  
                            select(Stkcd, Accper, F020105) %>% 
                            collect() %>% 
                            rename("EPS" = F020105)
                    
            } else if (value_base == "CFPS") {
                
                    ReptInfo_Acc <- tbl(QEA_db, "CashFlow_Statement") %>% 
                            filter(Accper == !!as.integer(ahdqua)) %>% 
                            filter(Stkcd %in% !!names(trddat)) %>% 
                            filter(Typrep == "A") %>%  
                            select(Stkcd, Accper, C005000000) %>% 
                            collect() %>% 
                            rename("CFPS" = C005000000)
                
            }
            
    } else if(model_type == "CH4") {
            
            if (value_base == "EPS") {
                
                    ReptInfo_Acc_Income <- tbl(QEA_db, "Income_Statement") %>% 
                            filter(Accper == !!as.integer(ahdqua)) %>% 
                            filter(Stkcd %in% !!names(trddat)) %>% 
                            filter(Typrep == "A") %>%  
                            select(Stkcd, Accper, B002000000, B003000000) %>% 
                            collect() %>% 
                            rename("Earning" = B002000000,
                                   "EPS" = B003000000)
                    
            } else if (value_base == "CFPS") {
                
                    ReptInfo_Acc_Earning <- tbl(QEA_db, "Income_Statement") %>% 
                            filter(Accper == !!as.integer(ahdqua)) %>% 
                            filter(Stkcd %in% !!names(trddat)) %>% 
                            filter(Typrep == "A") %>%  
                            select(Stkcd, Accper, B002000000) %>% 
                            collect() %>% 
                            rename("Earning" = B002000000)
            
                    ReptInfo_Acc_CashFlow <- tbl(QEA_db, "CashFlow_Statement") %>% 
                            filter(Accper == !!as.integer(ahdqua)) %>% 
                            filter(Stkcd %in% !!names(trddat)) %>% 
                            filter(Typrep == "A") %>%  
                            select(Stkcd, Accper, C005000000) %>% 
                            collect() %>% 
                            rename("CFPS" = C005000000)
                    
                    ReptInfo_Acc_Income <- inner_join(ReptInfo_Acc_Earning,
                                                      ReptInfo_Acc_CashFlow,
                                                      by = c("Stkcd", "Accper")
                                                      )
                
            }
                
            ReptInfo_Acc_Balance <- tbl(QEA_db, "Balance_Sheet") %>% 
                    filter(Accper == !!as.integer(ahdqua)) %>% 
                    filter(Stkcd %in% !!names(trddat)) %>% 
                    filter(Typrep == "A") %>%  
                    select(Stkcd, Accper, A004000000) %>% 
                    collect() %>% 
                    rename("Equity" = A004000000)
            
            ReptInfo_Acc <- inner_join(ReptInfo_Acc_Income,
                                       ReptInfo_Acc_Balance,
                                       by = c("Stkcd", "Accper")
                                       )
            
    } else if (model_type == "FF5") {
            
            ReptInfo_Acc_Income <- tbl(QEA_db, "Income_Statement") %>% 
                    filter(Accper == !!as.integer(ahdqua)) %>% 
                    filter(Stkcd %in% !!names(trddat)) %>% 
                    filter(Typrep == "A") %>%  
                    select(Stkcd, Accper, B002000000) %>% 
                    collect() %>% 
                    rename("Earning" = B002000000)
             
            ReptInfo_Acc_Balance <- tbl(QEA_db, "Balance_Sheet") %>% 
                    filter(Accper == !!as.integer(ahdqua)) %>% 
                    filter(Stkcd %in% !!names(trddat)) %>% 
                    filter(Typrep == "A") %>%  
                    select(Stkcd, Accper, A001000000, A002000000, A004000000) %>% 
                    collect() %>% 
                    rename("Asset" = A001000000, 
                           "Liability" = A002000000,
                           "Equity" = A004000000
                           ) %>% 
                    mutate("Asset_Net" = Asset - Liability)
                
            ReptInfo_Acc_Asset <- tbl(QEA_db, "Balance_Sheet") %>% 
                    # the difference value between two accounting period
                    filter(Accper %in% !!as.integer(c(ahdqua, ahdqua+years(-1)))) %>% 
                    filter(Stkcd %in% !!names(trddat)) %>% 
                    filter(Typrep == "A") %>%  
                    select(Stkcd, Accper, A001000000) %>% 
                    collect() %>% 
                    mutate(Accper = as.Date(Accper, origin = "1970-01-01")) %>% 
                    spread(Accper, A001000000) %>%
                    mutate("Asset_Growth_Rate" = 
                               get(as.character(ahdqua)) / 
                               get(as.character(ahdqua+years(-1))) - 1
                           )
            
            ReptInfo_Acc <- select(ReptInfo_Acc_Balance, c("Stkcd", "Equity", "Asset_Net")) %>% 
                    inner_join(ReptInfo_Acc_Income, ., by = c("Stkcd")) %>%
                    inner_join(select(ReptInfo_Acc_Asset, c("Stkcd", "Asset_Growth_Rate")),
                               by = c("Stkcd")
                               ) 
        
    }
                        
    ReptInfo_Acc %<>% 
            mutate(Accper = as.Date(Accper, origin = "1970-01-01")) %>% 
            arrange(Stkcd, Accper)
    
    # calculate the value of factors base accounting indicators ====
    # Notice that only the stocks, which published quarter financial report,
    # are brought into our sample to structure portfolio
    potfol <- trddat %>% 
            # extract the closing price of stocks at the ending of ahead quarter
            lapply(filter, TradingDate == ymd(splday)) %>% 
            bind_rows() %>% 
            inner_join(ReptInfo_Acc, by = "Stkcd") %>% 
            mutate("Reptdt" = year(TradingDate)) %>% 
            inner_join(Nshr, by = c("Stkcd", "Reptdt")) %>% 
            select(-c("Reptdt", "Dretnd", "Dsmvosd", "Nrrdaydt", "Listdt", "TradingDate")) %>% 
            # size, the number of shares product its closing price
            mutate("Size" = Clsprc * shrttl)
                   
    if(model_type == "CH3"){
        
            # value, at there we have three cluster paths, EPS, BVPS, VFPS
            potfol %<>% mutate(Value = get(!!value_base) / Clsprc) %>% 
                    filter("=="(rowSums(is.na(.)), 0L))
        
    } else if(model_type == "CH4") {
        
            potfol %<>% mutate(Value = get(!!value_base) / Clsprc,
                               # profitability, earnings-to-book equity
                               Profitability = Earning / Equity
                               ) %>% 
                    filter("=="(rowSums(is.na(.)), 0L))
        
    } else if(model_type == "FF5") {
        
            potfol %<>% mutate(Value = Asset_Net / Size, # B/M
                               Profitability = Earning / Equity # ROE
                               ) %>% 
                    # Investment, annual total asset growth rate
                    rename("Investment" = Asset_Growth_Rate) %>% 
                    filter("=="(rowSums(is.na(.)), 0L))
        
    }

    # There is a latent problem we should pay attention to,
    # the time terms of stocks in our sample (potfol)
    # aren't same with market trading dates at all (non-trading).
    
    # extract the current interval trading data of stocks ====
    trdff <- `[`(trddat, pull(potfol, Stkcd)) %>% 
            lapply(select, Stkcd, TradingDate, Dretnd, Dsmvosd, Nrrdaydt)
    
    if (model_type %in% c("CH3", "CH4")) {  
            # extract the time series data at quarter `q`
            trdff <- lapply(trdff, filter, TradingDate %within% 
                            interval((Accprd[q] + days(1)) %m+% months(-3), Accprd[q])
                            ) %>% 
                    bind_rows()
    
    } else if (model_type == "FF5") {  
            # extract the time series data at year `y`
            trdff <- lapply(trdff, filter, TradingDate %within% 
                            interval(Accprd[q] + years(-1), Accprd[q])
                            ) %>% 
                    bind_rows()
    }
    
    
    ####### The explanatory variables, (mkt_rf, SMB, VMG...) ########
    
    # mkt_rf, the returns of market risk subtracted risk-free rate of that day 
    trdff %<>% group_nest(TradingDate) %>% # Generate the calendar ordered column-list
            mutate("mkt_rf" = map_dbl(data, 
                    ~ with(.x, Dretnd %*% (Dsmvosd / sum(Dsmvosd)) - unique(Nrrdaydt)))
                   ) 
                        
    # we use the quantile of the factor Size and Value to cluster stocks
    potfol_x <- potfol %>% 
            mutate('g.SMB' = cut(Size, 
                                 breaks = quantile(Size, c(0, 0.5, 1)), 
                                 labels = c("Small", "Big"),
                                 ordered_result = TRUE, include.lowest = TRUE
                                 )
                   )
                    
    if (model_type == "CH3") {
            
            potfol_x %<>% mutate(
                    'g.VMG' = cut(Value, 
                                  breaks = quantile(Value, c(0, 0.3, 0.7, 1)), 
                                  labels = c("Growth", "V.Neutral", "Value"),
                                  ordered_result = TRUE, include.lowest = TRUE
                                  )
                    ) %>% 
            mutate(g.VMG = fct_relevel(g.VMG, c("Value", "V.Neutral", "Growth")))
            
    } else if(model_type == "CH4") {
        
            potfol_x %<>% mutate(
                    'g.VMG' = cut(Value, 
                                  breaks = quantile(Value, c(0, 0.3, 0.7, 1)), 
                                  labels = c("Growth", "V.Neutral", "Value"),
                                  ordered_result = TRUE, include.lowest = TRUE
                                  ),
                    'g.RMW' = cut(Profitability, 
                                  breaks = quantile(Profitability, c(0, 0.3, 0.7, 1)), 
                                  labels = c("Weak", "P.Neutral", "Robust"),
                                  ordered_result = TRUE, include.lowest = TRUE
                                  )
                    ) %>% 
                mutate(g.VMG = fct_relevel(g.VMG, c("Value", "V.Neutral", "Growth")),
                       g.RMW = fct_relevel(g.RMW, c("Robust", "P.Neutral", "Weak")))
            
    } else if(model_type == "FF5") {
    
            potfol_x %<>% mutate(
                    'g.HML' = cut(Value, quantile(Value, c(0, 0.3, 0.7, 1)), 
                                  labels = c("Low", "V.Neutral","High"),
                                  ordered_result = TRUE, include.lowest = TRUE
                                  ),
                    'g.RMW' = cut(Profitability, quantile(Profitability, c(0, 0.3, 0.7, 1)),
                                  labels = c("Weak", "P.Neutral", "Robust"),
                                  ordered_result = TRUE, include.lowest = TRUE
                                  ),
                    'g.CMA' = cut(Investment, quantile(Investment, c(0, 0.3, 0.7, 1)),
                                  labels = c("Conservative", "I.Neutral", "Aggressive"),
                                  ordered_result = TRUE, include.lowest = TRUE
                                  )
                    ) %>% 
                    mutate(g.HML = fct_relevel(g.HML, c("High", "V.Neutral", "Low")),
                           g.RMW = fct_relevel(g.RMW, c("Robust", "P.Neutral", "Weak"))
                           )
            
    }
    
    # join the trading data of stocks with portfolio structure (cluster)
    # Notice that this step will abandon the stocks which are not included in our sample                
    trdff_x <- trdff %>% 
            mutate(data = map(data, inner_join, 
                              select(potfol_x, Stkcd, ends_with(ff_term)), 
                              by = "Stkcd"
                              )
                   )
                        
    if(model_type == "CH3") {

            # grouping by factor
            trdff_x %<>% mutate(data = map(data, group_by, g.SMB, g.VMG))  
                
            # calculate the weighted returns of different portfolios
            trdff_x %<>% mutate("Portfolio_Return" = map(data, 
                    ~ summarise(.x, 
                                "Ret" = Dretnd %*% (Dsmvosd / sum(Dsmvosd)), 
                                .groups = "drop" 
                                ))
                    ) 
                
            # calculate the values of factors
            trdff_x %<>% mutate(
                    "SMB" = map_dbl(Portfolio_Return, ~ group_by(.x, g.SMB) %>% 
                                    summarise("avgRet" = mean(Ret), .groups = 'drop') %>% 
                                    spread(g.SMB, avgRet) %>% 
                                    with(., Small - Big)),
                    "VMG" = map_dbl(Portfolio_Return, ~ group_by(.x, g.VMG) %>% 
                                    summarise("avgRet" = mean(Ret), .groups = 'drop') %>% 
                                    spread(g.VMG, avgRet) %>% 
                                    with(., Value - Growth))
                    )
            
    } else if(model_type == "CH4") {
        
            trdff_x %<>% mutate(
                    "SV_Ret" = map(data, 
                                   ~ summarise(group_by(.x, g.SMB, g.VMG), 
                                               "Ret" = Dretnd %*% (Dsmvosd / sum(Dsmvosd)), 
                                               .groups = "drop" )
                                   ),
                    "SP_Ret" = map(data, 
                                   ~ summarise(group_by(.x, g.SMB, g.RMW), 
                                               "Ret" = Dretnd %*% (Dsmvosd / sum(Dsmvosd)), 
                                               .groups = "drop" )
                                   )
                    )
                
            # calculate the values of factors
            trdff_x %<>% mutate(
                    "VMG" = map_dbl(SV_Ret, 
                                    ~ summarise(group_by(.x, g.VMG), 
                                                "avgRet" = mean(Ret), .groups = 'drop'
                                                ) %>% 
                                            spread(g.VMG, avgRet) %>% 
                                            with(., Value - Growth)
                                    ),
                    "RMW" = map_dbl(SP_Ret, 
                                    ~ summarise(group_by(.x, g.RMW), 
                                                "avgRet" = mean(Ret), .groups = 'drop'
                                                ) %>% 
                                            spread(g.RMW, avgRet) %>% 
                                            with(., Robust - Weak)
                                    ),
                    # SMB
                    "SMB_VMG" = map_dbl(SV_Ret, ~ group_by(.x, g.SMB) %>% 
                                summarise("avgRet" = mean(Ret), .groups = 'drop') %>% 
                                spread(g.SMB, avgRet) %>% 
                                with(., Small - Big)),
                    "SMB_RMW" = map_dbl(SP_Ret, ~ group_by(.x, g.SMB) %>% 
                                summarise("avgRet" = mean(Ret), .groups = 'drop') %>% 
                                spread(g.SMB, avgRet) %>% 
                                with(., Small - Big)),
                    "SMB" = (SMB_VMG + SMB_RMW) / 2
                    )
                
            trdff_x %<>% select(-c("SV_Ret", "SP_Ret", "SMB_VMG", "SMB_RMW"))
        
    } else if(model_type == "FF5") {
        
            # weighted returns of portfolios
            trdff_x %<>% mutate(
                    "SV_Ret" = map(data, 
                                   ~ summarise(group_by(.x, g.SMB, g.HML), 
                                               "Ret" = Dretnd %*% (Dsmvosd / sum(Dsmvosd)), 
                                               .groups = "drop" )
                                   ),
                    "SP_Ret" = map(data, 
                                   ~ summarise(group_by(.x, g.SMB, g.RMW), 
                                               "Ret" = Dretnd %*% (Dsmvosd / sum(Dsmvosd)), 
                                               .groups = "drop" )
                                   ),
                    "SI_Ret" = map(data, 
                                   ~ summarise(group_by(.x, g.SMB, g.CMA), 
                                               "Ret" = Dretnd %*% (Dsmvosd / sum(Dsmvosd)), 
                                               .groups = "drop" )
                                   )
                    )
            
            # calculate the values of factors
            trdff_x %<>% mutate(
                    "HML" = map_dbl(SV_Ret, 
                                    ~ summarise(group_by(.x, g.HML), 
                                                "avgRet" = mean(Ret), .groups = 'drop'
                                                ) %>% 
                                            spread(g.HML, avgRet) %>% 
                                            with(., High - Low)
                                    ),
                    "RMW" = map_dbl(SP_Ret, 
                                    ~ summarise(group_by(.x, g.RMW), 
                                                "avgRet" = mean(Ret), .groups = 'drop'
                                                ) %>% 
                                            spread(g.RMW, avgRet) %>% 
                                            with(., Robust - Weak)
                                    ),
                    "CMA" = map_dbl(SI_Ret, 
                                    ~ summarise(group_by(.x, g.CMA), 
                                                "avgRet" = mean(Ret), .groups = 'drop'
                                                ) %>% 
                                            spread(g.CMA, avgRet) %>% 
                                            with(., Conservative - Aggressive)
                                    ),
                    # SMB
                    "SMB_HML" = map_dbl(SV_Ret, ~ group_by(.x, g.SMB) %>% 
                                summarise("avgRet" = mean(Ret), .groups = 'drop') %>% 
                                spread(g.SMB, avgRet) %>% 
                                with(., Small - Big)),
                    "SMB_RMW" = map_dbl(SP_Ret, ~ group_by(.x, g.SMB) %>% 
                                summarise("avgRet" = mean(Ret), .groups = 'drop') %>% 
                                spread(g.SMB, avgRet) %>% 
                                with(., Small - Big)),
                    "SMB_CMA" = map_dbl(SI_Ret, ~ group_by(.x, g.SMB) %>% 
                                summarise("avgRet" = mean(Ret), .groups = 'drop') %>% 
                                spread(g.SMB, avgRet) %>% 
                                with(., Small - Big)),
                    "SMB" = (SMB_HML + SMB_RMW + SMB_CMA) / 3
                    )
                
            trdff_x %<>% select(-c("SV_Ret", "SP_Ret", "SI_Ret",
                                   "SMB_HML", "SMB_RMW", "SMB_CMA")
                                )
        
    }
    
    
    ##### The dependent variable, weighted daily returns of 5*5 portfolios #####
    
    if (model_type == "CH3") {
    # structure the intersection 5*5 stock portfolios 
            potfol_y <- mutate(potfol, 
                        'g.SMB' = cut(Size, quantile(Size, seq(0, 1, by = 0.2)), 
                                      labels = c("Small", "2", "3", "4", "Big"),
                                      ordered_result = TRUE, include.lowest = TRUE),
                        
                        'g.VMG' = cut(Value, quantile(Value, seq(0, 1, by = 0.2)), 
                                      labels = c("Growth", 2, 3, 4, "Value"),
                                      ordered_result = TRUE, include.lowest = TRUE)
                        ) 
            
            # re-level the group structure
            potfol_y$g.VMG %<>% fct_relevel(c("Value", "4", "3", "2", "Growth"))
            
    } else if(model_type == "CH4") {
        
            potfol_y <- mutate(potfol, 
                        'g.SMB' = cut(Size, 
                                      breaks = quantile(Size, c(0, 0.5, 1)), 
                                      labels = c("Small", "Big"),
                                      ordered_result = TRUE, include.lowest = TRUE
                                      ),
                       
                        'g.VMG' = cut(Value, 
                                      breaks = quantile(Value, seq(0, 1, by = 0.25)), 
                                      labels = c("Growth", 2, 3, "Value"),
                                      ordered_result = TRUE, include.lowest = TRUE
                                      ),

                        'g.RMW' = cut(Profitability, 
                                      breaks = quantile(Profitability, seq(0, 1, by = 0.25)), 
                                      labels = c("Weak", 2, 3, "Robust"),
                                      ordered_result = TRUE, include.lowest = TRUE
                                      )
                        ) 
            
            # re-level the group structure
            potfol_y$g.VMG %<>% fct_relevel(c("Value", "3", "2", "Growth"))
            potfol_y$g.RMW %<>% fct_relevel(c("Robust", "3", "2", "Weak"))
        
    } else if(model_type == "FF5") {
        
            potfol_y <- mutate(potfol, 
                        'g.SMB' = cut(Size, 
                                      breaks = quantile(Size, c(0, 0.5, 1)), 
                                      labels = c("Small", "Big"),
                                      ordered_result = TRUE, include.lowest = TRUE
                                      ),
                        
                        'g.HML' = cut(Value, 
                                      breaks = quantile(Value, seq(0, 1, by = 0.25)), 
                                      labels = c("Low", 2, 3, "High"),
                                      ordered_result = TRUE, include.lowest = TRUE
                                      ),
                         
                        'g.RMW' = cut(Profitability, 
                                      breaks = quantile(Profitability, seq(0, 1, by = 0.25)), 
                                      labels = c("Weak", 2, 3, "Robust"),
                                      ordered_result = TRUE, include.lowest = TRUE
                                      ),
                        
                        'g.CMA' = cut(Investment, 
                                      breaks = quantile(Investment, seq(0, 1, by = 0.25)), 
                                      labels = c("Conservative", 2, 3, "Aggressive"),
                                      ordered_result = TRUE, include.lowest = TRUE
                                      )
                        ) 
                
            # re-level the group structure
            potfol_y$g.HML %<>% fct_relevel(c("High", "3", "2", "Low"))
            potfol_y$g.RMW %<>% fct_relevel(c("Robust", "3", "2", "Weak"))
        
    }
    
    
    # join the trading data of stocks with portfolio structure (cluster)
    # Notice that this step will abandon the stocks which are not included in our sample
    portfolio_ret <- trdff %>%
            mutate(data = map(data, inner_join, 
                              select(potfol_y, Stkcd, ends_with(ff_term)), 
                              by = "Stkcd"
                              )
                   )
                
    if (model_type == "CH3") {
            
        # calculate the weighted daily returns of portfolios 
        portfolio_ret %<>% transmute(TradingDate, 
                        "PR" = map(data, 
                                   ~ summarise(group_by(.x, g.SMB, g.VMG), 
                        "ptf_Ret" = Dretnd %*% (Dsmvosd /sum(Dsmvosd)), .groups = 'drop'))
                          ) %>% 
                unnest(cols = "PR")
            
            # data-visualization, plot a line figure ====
            filter(portfolio_ret, 
                   `&`(g.SMB %in% c("Small", "Big"),
                       g.VMG %in% c("Value", "Growth"))
                   ) %>% 
            ggplot(mapping = aes(x = TradingDate, y = ptf_Ret)) +
            geom_line(aes(colour = g.SMB, linetype = g.VMG)) + 
            labs(title = "Daily return of portfolios structured in size and value",
                 y = "Weighted daily return of portfolios") +
            scale_color_brewer(palette = "Set1") +
            theme_bw() +
            theme(plot.title = element_text(face = "bold"),
                  axis.title.x = element_blank(),
                  legend.position = "bottom",
                  legend.title = element_blank(),
                  legend.text = element_text(size = 15)
                  )
            
            ggsave(glue("./{model_type}/figure_portfolio_return/{Accprd[q]}_portfolio_return.pdf"),
                    width = 16, height = 9, dpi = 300, units = "in", limitsize = F)
        
    } else if(model_type == "CH4") {
        
            portfolio_ret %<>% transmute(TradingDate, 
                              "PR" = map(data, 
                                         ~ summarise(group_by(.x, g.SMB, g.VMG, g.RMW), 
                              "ptf_Ret" = Dretnd %*% (Dsmvosd /sum(Dsmvosd)), .groups = 'drop'))
                              ) %>% 
                    unnest(cols = "PR")
                
            filter(portfolio_ret, g.VMG %in% c("Value", "Growth")) %>% 
            filter(g.RMW %in% c("Robust", "Weak")) %>% 
                ggplot(mapping = aes(x = TradingDate, y = ptf_Ret)) +
                geom_line(aes(colour = g.RMW, linetype = g.VMG)) + 
                facet_wrap(~ g.SMB, nrow = 2) + 
                labs(title = "Daily return of portfolios structured in size, value, and profitability.",
                     y = "Weighted daily return of portfolios") +
                scale_color_brewer(palette = "Set1") +
                theme_bw() +
                theme(plot.title = element_text(face = "bold"),
                      axis.title.x = element_blank(),
                      legend.position = "bottom",
                      legend.title = element_blank(),
                      legend.text = element_text(size = 15)
                      )
            
            ggsave(glue("./{model_type}/figure_portfolio_return/{Accprd[q]}_portfolio_return.pdf"),
                    width = 16, height = 9, dpi = 300, units = "in", limitsize = F)
        
    } else if(model_type == "FF5") {
        
            portfolio_ret %<>% transmute(TradingDate, 
                              "PR" = map(data, 
                                         ~ summarise(group_by(.x, g.SMB, g.HML, g.RMW, g.CMA), 
                              "ptf_Ret" = Dretnd %*% (Dsmvosd /sum(Dsmvosd)), .groups = 'drop'))
                              ) %>% 
                    unnest(cols = "PR")
        
    }
    
    # retain the daily data for running regression
    trd_reg <- select(trdff_x, TradingDate, mkt_rf, all_of(ff_term)) %>% 
            inner_join(portfolio_ret, ., by = "TradingDate") %>% 
            inner_join(Nrrate, by = "TradingDate")
    
    
    ############## Regression ############
    
    if (model_type == "CH3") {
            
            potfolreg[[q]] <- trd_reg %>% 
                    group_by(g.SMB, g.VMG) %>% nest() %>%  
                    # run time series regression by portfolios
                    mutate("lm_CH3" = map(data, lm_ff, ff_class = "CH3")) %>% 
                    # coefficients of regression results ====   
                    mutate("Coef" = map(lm_CH3, tidy))

    } else if (model_type == "CH4") {
            
            potfolreg[[q]] <- trd_reg %>% 
                    group_by(g.SMB, g.VMG, g.RMW) %>% nest() %>%  
                    # run time series regression by portfolios
                    mutate("lm_CH4" = map(data, lm_ff, ff_class = "CH4")) %>% 
                    # coefficients of regression results ====   
                    mutate("Coef" = map(lm_CH4, tidy))
    
    } else if (model_type == "FF5") {
            
            potfolreg[[q]] <- trd_reg %>% 
                    group_by(g.SMB, g.HML, g.RMW, g.CMA) %>% nest() %>%  
                    # run time series regression by portfolios
                    mutate("lm_FF5" = map(data, lm_ff, ff_class = "FF5")) %>% 
                    # coefficients of regression results ====   
                    mutate("Coef" = map(lm_FF5, tidy))
            
    }
    
}

save(potfolreg, file = glue("./{model_type}/Hu-{model_type}.RData"))
dbDisconnect(QEA_db); rm(QEA_db)


# Part IV, visualizing the factor by month ----------------------------------
Accprd.year <- unique(year(Accprd))

for (i in seq_along(Accprd.year)) {

    FF_factor <- lapply(potfolreg, unnest, cols = c("data")) %>% 
        lapply(as_tibble) %>%  # remove the group attributes
        map_dfr(select, c('TradingDate', all_of(ff_term))) %>% 
        distinct() %>%  # duplicated among different portfolios
        gather(Factor, Return, -TradingDate) %>% 
        filter(year(TradingDate) == Accprd.year[i]) %>% # annually
        mutate("month" = month(TradingDate, label = TRUE),
               "day" = day(TradingDate),
               Factor = factor(Factor, levels = all_of(ff_term))
               )
    
    ggplot(data = FF_factor) +
        # The month that earnings report are released
        geom_rect(data = data.frame(month = factor(c("Jan", "Apr", "Jul", "Oct"))),
            aes(xmin = 1, xmax = 31, ymin = -Inf, ymax = Inf),
            alpha = 0.1, fill = 'blue') +
        geom_path(aes(x = day, y = Return, linetype = Factor, color = Factor)) + 
        facet_wrap( ~ month, ncol = 3) +
        labs(y = "The difference return of Mimicking-risk factor") +
        scale_color_brewer(palette = "Set1") +
        theme_bw() +
        theme(legend.position = "bottom",
              axis.title.x = element_blank()
              )
    
    file.path(getwd(), model_type, glue('{Accprd.year[i]}_path-{model_type}_factor.pdf')) %>% 
            ggsave(width = 16, height = 9, dpi = 300, units = "in", limitsize = F)

}


# table - portfolio estimates
map_dfr(potfolreg, unnest, cols = c("Coef"), .id = "quarter") %>% 
    select(-c(data, glue("lm_{model_type}"))) %>% 
        DT::datatable(rownames = FALSE) %>% 
        DT::formatRound(columns=c('estimate', 'std.error', 'statistic', 'p.value'), 
                        digits=3
                        )

