# This R script is written to replicate the quarterly empirical results of 
# Fama and French (1993, 2015) in Chinese A-share markets.
#
# The explanation factors ------------------------------------------------
#
# The return is positive correlation with risk.
# To explanation the cross-sectional variation of the daily returns of stocks,
# we construct multiple factors to mimic the risk of firm operating.
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
# three factor model: R_rf ~ mkt_rf + SMB + VMG ====
#
# SMB = 1/3 (Small Value + Small Neutral + Small Growth) –
#       1/3 (Big Value + Big Neutral + Big Growth)
#
# VMG = 1/2 (Small Value + Big Value) –
#       1/2 (Small Growth + Big Growth)
#
# four factor model: R_rf ~ mkt_rf + SMB + VMG + RMW ====
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
# Fama-French five factor model: R_rf ~ mkt_rf + SMB + VMG + RMW + CMA ====
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
# It is rational that the investors make decisions according to 
# the accounting indicators in earnings report they known recently. 
# At the beginning of quarter `q`, they know the information of quarter `q-2`.

library(tidyverse)
library(lubridate)
library(magrittr)

library(glue)
library(broom)

library(DBI)
library(RSQLite)
library(dbplyr)

library(RColorBrewer)

# function to query accounting data in reports from SQLite database
# the argument, report_type, represent the type of earnings report
# A, union report of institution, B, the parent company
tbl_query <- function(database = QEA_db, table_name, 
                      report_type = "A", 
                      date = ahead_period, 
                      stkcd = names(trdwin)
                      ) {

    tbl(database, table_name) %>% 
            filter(Accper %in% !!as.integer(date)) %>% 
            filter(Stkcd %in% !!stkcd) %>% 
            filter(Typrep == report_type) 
    
}

# change a new value to the default argument for a function
cut <- purrr::partial(cut, ordered_result = TRUE, include.lowest = TRUE)
summarise <- purrr::partial(summarise, .groups = 'drop')


# Part I, assign the basic parameter -------------------------------------

# set the type of multi-factor model
# CAPM, CH3 (Liu, 2018), CH4, CH5 (Fama-French, 2015)
model_type <- "FF3"

# set the type of interval of time period, yearly or quarterly?
period_type <- "quarterly"  

# set the time period term
# how much accounting periods will be running?
len_term <- 4*9
# the ending date of the first accounting period
start_term <- ymd('2010-03-31')
# 03-31, the first quarter; 06-30, the second quarter
# 09-30, the third quarter; 12-31, the fourth quarter

# Forecasting or explanation? 
# one ahead for explanation, two ahead for forecasting
ahead_quarter <- 1L

# Part II, replicate Fama-French multi-factor model -----------------------

# working directory
setwd('~/OneDrive/Data.backup/QEAData/')
# link to database
QEA_db <- dbConnect(RSQLite::SQLite(), "./QEA_db.sqlite")
# import data
load(file = "./PrePotfol.RData")
        
# factor terms and formula of regression model
if (model_type == "CAPM") {
        
        model_formula <- as.formula(I(Dretnd - Nrrdaydt) ~ mkt_rf)
        
} else if (model_type == "FF3") {
    
        ff_term <- c("SMB", "HML")
        model_formula <- as.formula(I(ptf_Ret - Nrrdaydt) ~ mkt_rf + SMB + HML)
        
} else if (model_type %in% c("CH3", "CH4")) { 
    
        if (model_type == "CH3") {
        
            ff_term <- c("SMB", "VMG")
            model_formula <- as.formula(I(ptf_Ret - Nrrdaydt) ~ mkt_rf + SMB + VMG)
    
        } else if (model_type == "CH4") {
        
            ff_term <- c("SMB", "VMG", "RMW")
            model_formula <- as.formula(I(ptf_Ret - Nrrdaydt) ~ mkt_rf + SMB + VMG + RMW)
            
        }
        
        # set the type of Value: EPS, or CFPS
        # EPS: using earnings-to-price as Value index; 
        # CFPS: the radio of cash flow per share to closing price
        value_base <- "EPS"  
        
} else if (model_type == "FF4") {
    
        ff_term <- c("SMB", "HML", "WML")
        model_formula <- as.formula(I(ptf_Ret - Nrrdaydt) ~ mkt_rf + SMB + HML + WML)
        
} else if (model_type == "FF5") {
        
        ff_term <- c("SMB", "HML", "RMW", "CMA")
        model_formula <- as.formula(I(ptf_Ret - Nrrdaydt) ~ mkt_rf + SMB + HML + RMW + CMA)
        
}

# according periods will be running 
if (period_type == "quarterly"){
    
        Accprd <- months(seq(from = 0, by = 3, length = len_term)) %>% 
                mapply(`%m+%`, start_term, .) %>% 
                as.Date(origin = '1970-01-01') 
        
} else if (period_type == "yearly") {
    
        Accprd <- years(seq(from = 0, by = 1, length = len_term)) %>% 
                mapply(`+`, start_term, .) %>% 
                as.Date(origin = '1970-01-01') 
        
}

# create lists to store results of multiple time periods
potfolreg <- vector(mode = 'list', length = length(Accprd)) %>% set_names(Accprd)
# create directory to save below results as some files
if (model_type %in% c("CAPM", "FF4", "CH4", "FF5")) {
    
    if (!dir.exists(model_type)) dir.create(model_type)
    
} else if (model_type %in% c("FF3", "CH3")) {
    
    if (!dir.exists(file.path(model_type, "figure_portfolio_return"))) {
            dir.create(model_type)
            dir.create(file.path(model_type, "figure_portfolio_return"))
    }
}


for (q in seq_along(Accprd)) {  # loop in time period
    
    print(Accprd[q])
    
    # set the benchmark date, and we will use the closing price of stocks 
    # at that date to contract current period portfolios
    if (period_type == "quarterly") {
        
            base_date <- ahead_period <- 
                    (Accprd[q] + days(1)) %m-% months(ahead_quarter * 3L) + days(-1)
            # the starting date of current accounting period
            starting_point <- (Accprd[q] + days(1)) %m-% months(3L)
            
    } else if (period_type == "yearly") {
        
            base_date <- ahead_period <- Accprd[q] + years(-1)
            
            starting_point <- ahead_period + days(1)
            
    }
    
    while (!base_date %in% trdday) base_date <- base_date + days(-1)
    
    # to speed up the script running, we strike out the data older than 2008-06-30
    trdwin <- trddat %>%  
            mutate(data = map(data, 
                              filter, 
                              TradingDate %within% interval(base_date, Accprd[q])
                              )
                   ) %>% 
            unnest(cols = "data") %>% 
            split(.$Stkcd)

    # eliminate stocks to enable reasonable precision and power ====
    
    # eliminating the stocks which listed less than six months
    trdwin %<>% keep(~ `>=`(interval(unique(pull(.x, Listdt)), ahead_period), months(6)))
    
    if (period_type == "quarterly") {  # less than 15 trading records in the past month
        
            trdwin %<>% keep(~ filter(.x, TradingDate %within% 
                                          interval(Accprd[q] %m+% months(-1), Accprd[q])
                                      ) %>% 
                                 nrow() >= 15L
                             )  
        
    } else if (period_type == "yearly") {  # less than 180 trading records in the past year
        
           trdwin %<>% keep(~ filter(.x, TradingDate %within% 
                                         interval(Accprd[q] + years(-1), Accprd[q])
                                     ) %>% 
                                nrow() >= 180L
                            )
                 
    }

    # import the information of earnings report ====
    if (model_type %in% c("CH3", "CH4", "FF3", "FF4", "FF5")) {
        
        if (model_type %in% c("CH3", "CH4")) {
            
            if (model_type == "CH3") {
                    
                    if (value_base == "EPS") {
                            
                            ReptInfo_Acc <- tbl_query(table_name = "Income_Statement") %>% 
                                    select(Stkcd, Accper, B003000000) %>% 
                                    collect() %>% 
                                    rename("EPS" = B003000000)
                            
                            
                    } else if (value_base == "CFPS") {
                        
                            ReptInfo_Acc <- tbl_query(table_name = "CashFlow_Statement") %>% 
                                    select(Stkcd, Accper, C005000000) %>% 
                                    collect() %>% 
                                    rename("CashFlow" = C005000000)
                        
                    }
                    
            } else if (model_type == "CH4") {
                
                    if (value_base == "EPS") {
                        
                            ReptInfo_Acc_Income <- tbl_query(table_name = "Income_Statement") %>% 
                                    select(Stkcd, Accper, B002000000, B003000000) %>% 
                                    collect() %>% 
                                    rename("Earning" = B002000000,
                                           "EPS" = B003000000
                                           )
                            
                    } else if (value_base == "CFPS") {
                        
                            ReptInfo_Acc_Earning <- tbl_query(table_name = "Income_Statement") %>% 
                                    select(Stkcd, Accper, B002000000) %>% 
                                    collect() %>% 
                                    rename("Earning" = B002000000)
                    
                            ReptInfo_Acc_CashFlow <- tbl_query(table_name = "CashFlow_Statement") %>% 
                                    select(Stkcd, Accper, C005000000) %>% 
                                    collect() %>% 
                                    rename("CashFlow" = C005000000)
                            
                            ReptInfo_Acc_Income <- inner_join(ReptInfo_Acc_Earning,
                                                              ReptInfo_Acc_CashFlow,
                                                              by = c("Stkcd", "Accper")
                                                              )
                        
                    }
                        
                    ReptInfo_Acc_Balance <- tbl_query(table_name = "Balance_Sheet") %>% 
                            select(Stkcd, Accper, A004000000) %>% 
                            collect() %>% 
                            rename("Equity" = A004000000)
                    
                    ReptInfo_Acc <- inner_join(ReptInfo_Acc_Income,
                                               ReptInfo_Acc_Balance,
                                               by = c("Stkcd", "Accper")
                                               )
            }
            
        }
        
        if (model_type %in% c("FF3", "FF4", "FF5")) {
            
            if (model_type == "FF3") {
                
                    ReptInfo_Acc <- tbl_query(table_name = "Balance_Sheet") %>% 
                            select(Stkcd, Accper, A001000000, A002000000) %>% 
                            collect() %>% 
                            rename("Asset" = A001000000, 
                                   "Liability" = A002000000
                                   ) %>% 
                            mutate("Asset_Net" = Asset - Liability) %>% 
                            select(c(Stkcd, Accper, "Asset_Net"))
                        
                
            } else if (model_type == "FF4") {
                
                        
                
            } else if (model_type == "FF5") {
                    
                    ReptInfo_Acc_Income <- tbl_query(table_name = "Income_Statement") %>% 
                            select(Stkcd, Accper, B002000000) %>% 
                            collect() %>% 
                            rename("Earning" = B002000000)
                     
                    ReptInfo_Acc_Balance <- tbl_query(table_name = "Balance_Sheet") %>% 
                            select(Stkcd, Accper, A001000000, A002000000, A004000000) %>% 
                            collect() %>% 
                            rename("Asset" = A001000000, 
                                   "Liability" = A002000000,
                                   "Equity" = A004000000
                                   ) %>% 
                            mutate("Asset_Net" = Asset - Liability) %>% 
                            select(c("Stkcd", "Equity", "Asset_Net"))
                        
                    # the first difference value between two accounting period
                    if (period_type == "quarterly") {
                        
                            link_period <- c(ahead_period, (ahead_period + days(1)) %m+% months(-3) + days(-1))
                            
                    } else if (period_type == "yearly") {
                        
                            link_period <- c(ahead_period, ahead_period + years(-1))
                            
                    }
                    
                    ReptInfo_Acc_Asset <- tbl_query(table_name = "Balance_Sheet",
                                                    date = link_period
                                                    ) %>% 
                            select(Stkcd, Accper, A001000000) %>% 
                            collect() %>% 
                            mutate('Accper' = as.Date(Accper, origin = "1970-01-01")) %>% 
                            spread(Accper, A001000000) %>%
                            mutate("Asset_Growth_Rate" = 
                                           get(as.character(link_period[1])) / 
                                           get(as.character(link_period[2])) - 1
                                   ) %>% 
                            select(c("Stkcd", "Asset_Growth_Rate"))
                    
                    ReptInfo_Acc <- inner_join(ReptInfo_Acc_Income, 
                                               ReptInfo_Acc_Balance, 
                                               by = c("Stkcd")
                                               ) %>%
                            inner_join(ReptInfo_Acc_Asset, by = c("Stkcd")) 
                
            }
        }        
        
        
        ReptInfo_Acc %<>% 
                mutate('Accper' = as.Date(Accper, origin = "1970-01-01")) %>% 
                arrange(Stkcd, Accper)

        # calculate the value of factors base accounting indicators ====
        # Notice that only the stocks, which published quarter financial report,
        # are brought into our sample to structure portfolio
        potfol <- trdwin %>% 
                # extract the closing price of stocks at the ending of ahead quarter
                lapply(filter, TradingDate == ymd(base_date)) %>% 
                bind_rows() %>% 
                inner_join(ReptInfo_Acc, by = "Stkcd") %>% 
                mutate("Reptdt" = year(TradingDate)) %>% 
                inner_join(Nshr, by = c("Stkcd", "Reptdt")) %>% 
                select(-c("Reptdt", "Dretnd", "Dsmvosd", "Listdt", "TradingDate")) %>% 
                # size, the number of shares product its closing price
                mutate("Size" = Clsprc * shrttl)
        
        if (model_type %in% c("CH3", "CH4")) {
            
                if(value_base == "CFPS") {
                    
                        potfol %<>% mutate("CFPS" = CashFlow / shrttl) %>% select(-CashFlow)
                    
                }
                
                # value, at there we have three cluster paths, EPS, BVPS, VFPS
                potfol %<>% mutate('Value' = get(!!value_base) / Clsprc)
                
                if(model_type == "CH4") {
                    
                        # profitability, earnings-to-book equity
                        potfol %<>% mutate('Profitability' = Earning / Equity)
                    
                }
        }
        
        if (model_type %in% c("FF3", "FF4", "FF5")) {
            
                # Value, Book value to Market value
                potfol %<>% mutate('Value' = Asset_Net / Size)
                        
                if (model_type == "FF4") {
                    
                        
                    
                } else if (model_type == "FF5") {
                    
                        potfol %<>%
                                # Profitability, ROE
                                mutate('Profitability' = Earning / Equity) %>% 
                                # Investment, total asset growth rate
                                rename("Investment" = Asset_Growth_Rate)
                    
                }
            
        }
        
        # exclude the rows (stock) contained NaN value
        potfol %<>% filter("=="(rowSums(is.na(.)), 0L))
        
        # only the stocks within portfolio list will be select into sample
        trdwin %<>% `[`(pull(potfol, Stkcd))

    }

    # extract the current period trading data of stocks
    trdff <- lapply(trdwin, filter, TradingDate %within% interval(starting_point, Accprd[q]))

    
    ####### The explanatory variables, (mkt_rf, SMB, VMG...) ########
    
    # mkt_rf, the returns of market risk subtracted risk-free rate of that day 
    # select(.x, c(Stkcd, TradingDate, Dretnd, Dsmvosd, Markettype, Indus))
    trdff %<>% map(~ left_join(Nrrate, 
                               select(.x, c(Stkcd, TradingDate, Dretnd, Dsmvosd)),
                               by = 'TradingDate'
                               )
                   ) %>% 
            bind_rows() %>% 
            group_nest(TradingDate) %>% # Generate the calendar ordered column-list
            mutate("mkt_rf" = map_dbl(data, ~ with(.x, Dretnd %*% (Dsmvosd / sum(Dsmvosd)) - unique(Nrrdaydt))))
    
    # use the quantile of the factors to cluster stocks (contract portfolios)
    if (model_type %in% c("CH3", "CH4", "FF3", "FF4", "FF5")) {
                        
            potfol_x <- potfol %>% 
                    mutate('g_Size' = cut(Size, 
                                          breaks = quantile(Size, c(0, 0.5, 1)), 
                                          labels = c("Small", "Big")
                                          ),
                           'g_Value' = cut(Value, 
                                           breaks = quantile(Value, c(0, 0.3, 0.7, 1)), 
                                           labels = c("Growth", "V.Neutral", "Value")
                                           ),
                           # re-level the group structure
                           'g_Value' = fct_relevel(g_Value, c("Value", "V.Neutral", "Growth"))
                           ) 
                    
            if (model_type == "CH4") {
                
                    potfol_x %<>% mutate(
                            'g_Profitability' = cut(Profitability, 
                                                    breaks = quantile(Profitability, c(0, 0.3, 0.7, 1)), 
                                                    labels = c("Weak", "P.Neutral", "Robust")
                                                    ),
                            'g_Profitability' = fct_relevel(g_Profitability, c("Robust", "P.Neutral", "Weak"))
                            )
                    
            } else if(model_type == "FF4") {
            
            
                    
            } else if(model_type == "FF5") {
            
                    potfol_x %<>% mutate(
                            'g_Profitability' = cut(Profitability, 
                                          quantile(Profitability, c(0, 0.3, 0.7, 1)),
                                          labels = c("Weak", "P.Neutral", "Robust")
                                          ),
                            'g_Investment' = cut(Investment, 
                                          quantile(Investment, c(0, 0.3, 0.7, 1)),
                                          labels = c("Conservative", "I.Neutral", "Aggressive")
                                          ),
                            'g_Profitability' = fct_relevel(g_Profitability, c("Robust", "P.Neutral", "Weak"))
                            )
                                   
            }
            
            # join the trading data of stocks with portfolio structure (cluster)
            # Notice that this step will abandon the stocks which are not included in our sample                
            trdff_factor <- trdff %>% 
                    mutate(data = map(data, inner_join, 
                                      select(potfol_x, Stkcd, starts_with("g_")), 
                                      by = "Stkcd"
                                      )
                           )
                                
            if(model_type %in% c("CH3", "FF3")) {
        
                    # grouping by factor
                    trdff_factor %<>% mutate(data = map(data, group_by, g_Size, g_Value))  
                        
                    # calculate the weighted returns of different portfolios
                    trdff_factor %<>% mutate(
                            "Portfolio_Return" = map(data, summarise, 
                                                     "Ret" = Dretnd %*% (Dsmvosd / sum(Dsmvosd))
                                                     )
                            )
                        
                    # calculate the values of factors
                    trdff_factor %<>% mutate(
                            "SMB" = map_dbl(Portfolio_Return, 
                                            ~ summarise(group_by(.x, g_Size), 
                                                        "avgRet" = mean(Ret) 
                                                        ) %>% 
                                                    spread(g_Size, avgRet) %>% 
                                                    with(., Small - Big)
                                            ),
                            "VMG" = map_dbl(Portfolio_Return, 
                                            ~ summarise(group_by(.x, g_Value),
                                                        "avgRet" = mean(Ret) 
                                                        ) %>% 
                                                    spread(g_Value, avgRet) %>% 
                                                    with(., Value - Growth)
                                            )
                            )
                    
                    # through the calculate steps between CH3 with FF3 is same, 
                    # but the benchmark index is different, we need to rename 
                    # the factor name for easily distinguish to others
                    if (model_type == "FF3") {  
                        
                            trdff_factor %<>% rename("HML" = VMG) 
                        
                    }
                        
            } else if(model_type == "CH4") {
                    
                    # weighted returns of portfolios
                    trdff_factor %<>% mutate(
                            "SV_Ret" = map(data, 
                                           ~ summarise(group_by(.x, g_Size, g_Value), 
                                                       "Ret" = Dretnd %*% (Dsmvosd / sum(Dsmvosd)) 
                                                       )
                                           ),
                            "SP_Ret" = map(data, 
                                           ~ summarise(group_by(.x, g_Size, g_Profitability), 
                                                       "Ret" = Dretnd %*% (Dsmvosd / sum(Dsmvosd)) 
                                                       )
                                           )
                            )
                        
                    # calculate the values of factors
                    trdff_factor %<>% mutate(
                            "VMG" = map_dbl(SV_Ret, 
                                            ~ summarise(group_by(.x, g_Value),
                                                        "avgRet" = mean(Ret) 
                                                        ) %>% 
                                                    spread(g_Value, avgRet) %>% 
                                                    with(., Value - Growth)
                                            ),
                            "RMW" = map_dbl(SP_Ret, 
                                            ~ summarise(group_by(.x, g_Profitability),
                                                        "avgRet" = mean(Ret) 
                                                        ) %>% 
                                                    spread(g_Profitability, avgRet) %>% 
                                                    with(., Robust - Weak)
                                            ),
                            # SMB
                            "SMB_VMG" = map_dbl(SV_Ret, 
                                                ~ summarise(group_by(.x, g_Size),
                                                            "avgRet" = mean(Ret) 
                                                            ) %>% 
                                                        spread(g_Size, avgRet) %>% 
                                                        with(., Small - Big)
                                                ),
                            "SMB_RMW" = map_dbl(SP_Ret, 
                                                ~ summarise(group_by(.x, g_Size),
                                                            "avgRet" = mean(Ret) 
                                                            ) %>% 
                                                        spread(g_Size, avgRet) %>% 
                                                        with(., Small - Big)
                                                ),
                            "SMB" = (SMB_VMG + SMB_RMW) / 2
                            ) %>% 
                    select(-c("SV_Ret", "SP_Ret", "SMB_VMG", "SMB_RMW"))
                    
            } else if (model_type == "FF4") {
                
                
                
            } else if(model_type == "FF5") {
                
                    # weighted returns of portfolios
                    trdff_factor %<>% mutate(
                            "SV_Ret" = map(data, 
                                           ~ summarise(group_by(.x, g_Size, g_Value), 
                                                       "Ret" = Dretnd %*% (Dsmvosd / sum(Dsmvosd)) 
                                                       )
                                           ),
                            "SP_Ret" = map(data, 
                                           ~ summarise(group_by(.x, g_Size, g_Profitability), 
                                                       "Ret" = Dretnd %*% (Dsmvosd / sum(Dsmvosd)) 
                                                       )
                                           ),
                            "SI_Ret" = map(data, 
                                           ~ summarise(group_by(.x, g_Size, g_Investment), 
                                                       "Ret" = Dretnd %*% (Dsmvosd / sum(Dsmvosd)) 
                                                       )
                                           )
                            )
                    
                    # calculate the values of factors
                    trdff_factor %<>% mutate(
                            "HML" = map_dbl(SV_Ret, 
                                            ~ summarise(group_by(.x, g_Value), 
                                                        "avgRet" = mean(Ret) 
                                                        ) %>% 
                                                    spread(g_Value, avgRet) %>% 
                                                    with(., Value - Growth)
                                            ),
                            "RMW" = map_dbl(SP_Ret, 
                                            ~ summarise(group_by(.x, g_Profitability), 
                                                        "avgRet" = mean(Ret) 
                                                        ) %>% 
                                                    spread(g_Profitability, avgRet) %>% 
                                                    with(., Robust - Weak)
                                            ),
                            "CMA" = map_dbl(SI_Ret, 
                                            ~ summarise(group_by(.x, g_Investment), 
                                                        "avgRet" = mean(Ret) 
                                                        ) %>% 
                                                    spread(g_Investment, avgRet) %>% 
                                                    with(., Conservative - Aggressive)
                                            ),
                            # SMB
                            "SMB_HML" = map_dbl(SV_Ret, 
                                                ~ summarise(group_by(.x, g_Size),
                                                            "avgRet" = mean(Ret) 
                                                            ) %>% 
                                                        spread(g_Size, avgRet) %>% 
                                                        with(., Small - Big)
                                                ),
                            "SMB_RMW" = map_dbl(SP_Ret, 
                                                ~ summarise(group_by(.x, g_Size),
                                                            "avgRet" = mean(Ret) 
                                                            ) %>% 
                                                        spread(g_Size, avgRet) %>% 
                                                        with(., Small - Big)
                                                ),
                            "SMB_CMA" = map_dbl(SI_Ret, 
                                                ~ summarise(group_by(.x, g_Size),
                                                            "avgRet" = mean(Ret) 
                                                            ) %>% 
                                                        spread(g_Size, avgRet) %>% 
                                                        with(., Small - Big)
                                                ),
                            "SMB" = (SMB_HML + SMB_RMW + SMB_CMA) / 3
                            ) %>% 
                    select(-c("SV_Ret", "SP_Ret", "SI_Ret", "SMB_HML", "SMB_RMW", "SMB_CMA"))
                
            }
    
    }
    
    ##### The dependent variable, weighted daily returns of portfolios #####
    
    if (model_type %in% c("CH3", "CH4", "FF3", "FF4", "FF5")) {
            
            if (model_type %in% c("CH3", "FF3")) {

                    potfol_y <- mutate(potfol, 
                            'g_Size' = cut(Size, 
                                           quantile(Size, seq(0, 1, by = 0.2)), 
                                           labels = c("Small", "2", "3", "4", "Big")
                                           ),
                            'g_Value' = cut(Value, 
                                          quantile(Value, seq(0, 1, by = 0.2)), 
                                          labels = c("Growth", 2, 3, 4, "Value")
                                          ),
                            'g_Value' = fct_relevel(g_Value, c("Value", "4", "3", "2", "Growth"))
                            ) 
                    
            } else if(model_type == "CH4") {
                
                    potfol_y <- mutate(potfol, 
                            'g_Size' = cut(Size, 
                                          breaks = quantile(Size, c(0, 0.5, 1)), 
                                          labels = c("Small", "Big")
                                          ),
                            'g_Value' = cut(Value, 
                                          breaks = quantile(Value, seq(0, 1, by = 0.25)), 
                                          labels = c("Growth", 2, 3, "Value")
                                          ),
                            'g_Profitability' = cut(Profitability, 
                                          breaks = quantile(Profitability, seq(0, 1, by = 0.25)), 
                                          labels = c("Weak", 2, 3, "Robust")
                                          ),
                            'g_Value' = fct_relevel(g_Value, c("Value", "3", "2", "Growth")),
                            'g_Profitability' = fct_relevel(g_Profitability, c("Robust", "3", "2", "Weak"))
                            )
                                   
            } else if(model_type == "FF4") {
                
                
                
            } else if(model_type == "FF5") {
                
                    potfol_y <- mutate(potfol, 
                            'g_Size' = cut(Size, 
                                           breaks = quantile(Size, c(0, 0.5, 1)), 
                                           labels = c("Small", "Big")
                                           ),
                            'g_Value' = cut(Value, 
                                            breaks = quantile(Value, seq(0, 1, by = 0.25)), 
                                            labels = c("Growth", 2, 3, "Value")
                                            ),
                            'g_Profitability' = cut(Profitability, 
                                                    breaks = quantile(Profitability, seq(0, 1, by = 0.25)), 
                                                    labels = c("Weak", 2, 3, "Robust")
                                                    ),
                            'g_Investment' = cut(Investment, 
                                                 breaks = quantile(Investment, seq(0, 1, by = 0.25)), 
                                                 labels = c("Conservative", 2, 3, "Aggressive")
                                                 ),
                            'g_Value' = fct_relevel(g_Value, c("Value", "3", "2", "Growth")),
                            'g_Profitability' = fct_relevel(g_Profitability, c("Robust", "3", "2", "Weak"))
                            )
                
            }
            
            
            # join the trading data of stocks with portfolio structure (cluster)
            # Notice that this step will abandon the stocks which are not included in our sample
            portfolio_ret <- trdff %>%
                    mutate(data = map(data, inner_join, 
                                      select(potfol_y, Stkcd, starts_with('g_')), 
                                      by = "Stkcd"
                                      )
                           ) %>%
                    # calculate the weighted daily returns of portfolios 
                    transmute(TradingDate, 
                              "PR" = map(data, 
                                         ~ summarise(group_by_if(.x, is.factor), 
                                                     "ptf_Ret" = Dretnd %*% (Dsmvosd /sum(Dsmvosd))
                                                     )
                                         )
                              ) %>% 
                    unnest(cols = "PR")
                        
            # data visualization ====
            
            if (model_type %in% c("CH3", "Ch4", "FF3")) {  # plot line figure
                
                    if (model_type %in% c("CH3", "FF3")) { 
                            
                            title_char <- paste("Daily return of portfolios structured in size and value",
                                                model_type,
                                                sep = ", "
                                                )
                        
                            filter(portfolio_ret, `&`(g_Size %in% c("Small", "Big"),
                                                      g_Value %in% c("Value", "Growth")
                                                      )
                                   ) %>% 
                                ggplot(mapping = aes(x = TradingDate, y = ptf_Ret)) +
                                    geom_line(aes(colour = g_Size, linetype = g_Value)) + 
                                        labs(y = "Weighted daily return",
                                             title = title_char
                                             ) +
                                        scale_color_brewer(palette = "Set1") +
                                        theme_bw() +
                                        theme(axis.title.x = element_blank(),
                                              legend.position = "bottom",
                                              legend.title = element_blank(),
                                              legend.text = element_text(size = 15)
                                              )
                        
                    } else if(model_type == "CH4") {
                                
                            title_char <- paste(
                                "Daily return of portfolios which structured in size, value, and profitability", 
                                model_type,
                                sep = ", "
                                )
                            
                            filter(portfolio_ret, g_Value %in% c("Value", "Growth")) %>% 
                            filter(g_Profitability %in% c("Robust", "Weak")) %>% 
                                ggplot(mapping = aes(x = TradingDate, y = ptf_Ret)) +
                                    geom_line(aes(colour = g_Profitability, linetype = g_Value)) + 
                                        facet_wrap(~ g_Size, nrow = 2) + 
                                            labs(y = "Weighted daily return",
                                                 title = title_char
                                                 ) +
                                            scale_color_brewer(palette = "Set1") +
                                            theme_bw() +
                                            theme(axis.title.x = element_blank(),
                                                  legend.position = "bottom",
                                                  legend.title = element_blank(),
                                                  legend.text = element_text(size = 15)
                                                  )
                        
                    }
                    
                    if (model_type %in% c("CH3", "CH4")) {
                
                            figure_filename <- paste(Accprd[q], period_type, value_base, 
                                                     "portfolio_return.pdf",
                                                     sep = "_"
                                                     )
                    
                    } else if (model_type == "FF3") {
                
                            figure_filename <- paste(Accprd[q], period_type,
                                                     "portfolio_return.pdf",
                                                     sep = "_"
                                                     )
                    
                    }
            
                    ggsave(filename = figure_filename, 
                           path = file.path(model_type, "figure_portfolio_return"),
                           width = 16, height = 18
                           )
                    
            } else if(model_type == "FF5") {  # generate a summary table
                
                    
                
            }
            
            # retain the daily data for running regression
            trd_reg <- select(trdff_factor, TradingDate, mkt_rf, all_of(ff_term)) %>% 
                    inner_join(portfolio_ret, ., by = "TradingDate") %>% 
                    inner_join(Nrrate, by = "TradingDate")
    
    }
    

    # Running Regression ---------------------------------------------------
    
    if (model_type == "CAPM") {
        
            trd_reg <- select(trdff, TradingDate, mkt_rf) %>% 
                    lapply(trdwin, inner_join, ., by = "TradingDate") %>% 
                    map(~ ifelse(nrow(.x) <= 40L, return(NULL), return(.x))) %>% 
                    compact()
            
            potfolreg[[q]] <- map_dfr(trd_reg, 
                                      ~ tidy(lm(formula = model_formula, data = .x)),
                                      .id = "Stkcd"
                                      )
    
    } else if (model_type %in% c("CH3", "Ch4", "FF3", "FF4", "FF5")) {
            
            potfolreg[[q]] <- trd_reg %>% 
                    group_by_if(is.factor) %>% 
                    nest() %>%  
                    # run time series regression by portfolios
                    mutate("model" = map(data, ~ lm(formula = model_formula, data = .x)),
                           "Coef" = map(model, tidy)  # coefficients of regression results
                           ) 
            
    }
    
}

ifelse(model_type %in% c("CH3", "CH4"), 
       save(potfolreg, file = glue("./{model_type}/{period_type}_{value_base}_{model_type}.RData")),
       save(potfolreg, file = glue("./{model_type}/{period_type}_{model_type}.RData"))
       )

dbDisconnect(QEA_db)

# create tables ====

library(DT)
library(knitr)
library(kableExtra)

if (model_type %in% c("CH3", "CH4", "FF3", "FF4")) {
    
    # the estimate intercepts of portfolios
    potfolreg_table <- map_dfr(potfolreg, unnest, cols = c("Coef"), 
                               .id = "quarter"
                               ) %>% 
            filter(term == "(Intercept)") %>% 
            select(c(quarter, starts_with('g_'), statistic)) %>% 
            ungroup()
    
    if (model_type %in% c("CH3", "FF3")) {
        
            VMG_level <- levels(potfolreg_table$g_Value) 
        
            potfolreg_table %<>% spread(g_Value, statistic)
        
            datatable(potfolreg_table, 
                      rownames = FALSE,
                      caption = glue('The estimate intercepts of portfolios ({model_type})')
                      ) %>% 
            formatRound(columns = VMG_level, digits = 3)
            
            kable(potfolreg_table, 
                  format = "latex", 
                  digits = 3,
                  longtable = T, booktabs = T, 
                  caption = glue('The estimate intercepts of portfolios ({model_type})')
                  ) %>%
            kable_styling(latex_options = c("repeat_header"),
                          position = "center"
                          )

    } else if (model_type == "CH4") {

            potfolreg_table %<>% group_by(quarter, g_Size) %>% 
                    group_nest() %>% 
                    mutate(data = map(data, spread, g_Profitability, statistic)) %>% 
                    spread(g_Size, data) %>% 
                    unnest(cols = c(Small, Big), names_repair = "minimal")
                    
            datatable(potfolreg_table,
                      rownames = FALSE,
                      caption = glue('The estimate intercepts of portfolios ({model_type})')
                      ) %>% 
            formatRound(columns = c(3:6, 8:11), digits = 3)
                    
            kable(potfolreg_table, 
                  format = "latex", 
                  digits = 3,
                  longtable = T, booktabs = T, 
                  caption = glue('The estimate intercepts of portfolios ({model_type})')
                  ) %>%
            add_header_above(c(" " = 2, "Small" = 4, " " = 1, "Big" = 4)) %>% 
            kable_styling(latex_options = c("repeat_header"),
                          position = "center"
                          )
            
    } else if (model_type == "FF4") {
        
            
        
    }

}


# visualizing ====

if (model_type == "CAPM") { # the distribution of the estimator of beta (CAPM) ====
    
    variable <- "mkt_rf"
    estimator <- "estimate"
    
    measure <- lapply(potfolreg, group_nest, term) %>% 
            lapply(filter, term == variable) %>% 
            lapply(mutate, data = map(data, select, Stkcd, all_of(estimator))) %>% 
            lapply(unnest, cols = "data")
            
    for (i in seq_along(measure)) {
        
            ggplot(data = measure[[i]]) +
                geom_histogram(mapping = aes(x = get(estimator)), bins = 25) +
                    labs(title = "The distribution of the estimator of beta (CAPM)",
                         x = "value of beta (stock)",
                         subtitle = glue("{period_type} - {names(measure)[i]}")
                         ) +
                    theme_bw()
            
            ggsave(filename = glue('{Accprd[i]}_{model_type}_beta_hist.pdf'),
                   path = file.path(model_type),
                   width = 6, height = 6)
        
    }

} else if (model_type %in% c("CH3", "CH4", "FF4", "FF5")) {  
    
    # visualize the values of factors by month ====
    
    Accprd_year <- unique(year(Accprd))
    
    for (i in seq_along(Accprd_year)) {
    
        FF_factor <- lapply(potfolreg, unnest, cols = c("data")) %>% 
                lapply(as_tibble) %>%  # remove the group attributes
                map_dfr(select, c('TradingDate', all_of(ff_term))) %>% 
                distinct() %>%  # duplicated among different portfolios
                gather(Factor, Return, -TradingDate) %>% 
                filter(year(TradingDate) == Accprd_year[i]) %>% # annually
                mutate("month" = month(TradingDate, label = TRUE),
                       "day" = day(TradingDate),
                       Factor = factor(Factor, levels = all_of(ff_term))
                       )
        
        ggplot(data = FF_factor) +
            # The months that earnings report are released
            geom_rect(data = data.frame("month" = factor(c("Jan", "Apr", "Jul", "Oct"))),
                      mapping = aes(xmin = 1, xmax = 31, ymin = -Inf, ymax = Inf),
                      alpha = 0.1, fill = 'blue'
                      ) +
            geom_path(aes(x = day, y = Return, linetype = Factor, color = Factor)) + 
                facet_wrap( ~ month, ncol = 3) +
                    labs(y = "The value of Mimicking-risk factors",
                         title = "The first difference value between diversified portfolios"
                         ) +
                    scale_color_brewer(palette = "Set1") +
                    theme_bw() +
                    theme(legend.position = "bottom",
                          axis.title.x = element_blank()
                          )
        
        ifelse(model_type %in% c("CH3", "CH4"),
               figure_filename <- glue('{Accprd_year[i]}_{value_base}_{model_type}_factor_path.pdf'),
               figure_filename <- glue('{Accprd_year[i]}_{model_type}_factor_path.pdf')
               )

        ggsave(filename = figure_filename,
               path = file.path(model_type),
               width = 16, height = 9
               )
    
    }
    
}
