# This script is written for reading CSMAR data batched, 
# and then store them as tables in SQLite database and RData file.

## Importing data through querying the SQL database will solve
## the problem of memory shortage and process timeout,
## but the relative shortcoming is the data type defined in R will vanish,
## especially for the date and factor.
## So, after querying the data we desired, 
## we need to transform the data type of them carefully.

library(magrittr)
library(lubridate)
library(tidyverse)

library(DBI)
library(RSQLite)
library(dbplyr)

library(parallel)
library(doParallel)
CL <- makeCluster(detectCores(logical = FALSE) - 1L)
registerDoParallel(CL)

# working directory
setwd("~/OneDrive/Data.backup/QEAData")

# Part I, create tables with annual, quarter, and daily data ------------
# link to SQLite database
QEA_db <- dbConnect(RSQLite::SQLite(), "./QEA_db.sqlite")

#  annual =====
annual_path <- list.files(path = "./Acc_Annual", full.names = TRUE,
                          pattern = "^(CSR|FAR|MNM).*[^]]\\.txt$"
                          )
        
annual <- foreach(i = 1:length(annual_path),
                  # the combine approach is taking collection
                  .combine = full_join,  
                  # will be used package, `readr` to read data
                  .packages = c("readr")  
                  ) %dopar%
        read_delim(annual_path[i], delim = "\t", na = '',
                   col_types = cols(.default = col_double(),
                                    Stkcd = col_character(),
                                    Accper = col_date('%Y-%m-%d'),
                                    Annodt = col_date('%Y-%m-%d')
                                    )
                   )

arrange(annual, Accper, Stkcd) %>% 
        dbWriteTable(QEA_db, "annual", .)

# quarter =====
quarter_path <- list.files(path = "./Acc_Quarter", full.names = TRUE,
                           pattern = "[0-9]\\.txt$"
                           )

quarter <- foreach(i = 1:length(quarter_path),
                   .combine = full_join,
                   .packages = c("readr")
                   ) %dopar%
        read_delim(quarter_path[i], delim = "\t", na = '',
                   col_types = cols(.default = col_double(),
                                    Stkcd = col_character(),
                                    Accper = col_date('%Y-%m-%d'),
                                    Typrep = col_character(),
                                    Indcd = col_character()
                                    )
                   )

arrange(quarter, Accper, Stkcd) %>% 
        dbWriteTable(QEA_db, "quarter", .)

stopCluster(CL)

# all the accounting indicators in earnings report of listed firms ====
# cash flow statement
CashFlow <- read_delim(file = "./CashFlow_Statement/FS_Comscfd.txt", 
                       delim = '\t', na = '',
                       col_types = cols(.default = col_double(),
                                        Stkcd = col_character(),
                                        Accper = col_date(format = '%Y-%m-%d'),
                                        Typrep = col_character()
                                        )
                       )

arrange(CashFlow, Stkcd, Accper, Typrep) %>% 
        dbWriteTable(QEA_db, "CashFlow_Statement", .)

# Income statement
Income <- read_delim(file = "./Income_Statement/FS_Comins.txt", 
                     delim = '\t', na = '',
                     col_types = cols(.default = col_double(),
                                      Stkcd = col_character(),
                                      Accper = col_date(format = '%Y-%m-%d'),
                                      Typrep = col_character()
                                      )
                     )

arrange(Income, Accper, Stkcd, Typrep) %>% 
        dbWriteTable(QEA_db, "Income_Statement", .)

# Balance sheet
Balance <- read_delim(file = "./Balance_Sheet/FS_Combas.txt", 
                      delim = '\t', na = '',
                      col_types = cols(.default = col_double(),
                                       Stkcd = col_character(),
                                       Accper = col_date(format = '%Y-%m-%d'),
                                       Typrep = col_character()
                                       )
                      )

arrange(Balance, Accper, Stkcd, Typrep) %>% 
        dbWriteTable(QEA_db, "Balance_Sheet", .)

rm(annual, quarter, CashFlow, Income, Balance); gc()

# daily trading data=====
TRD_Dalyr <- read_delim(file = './Acc_Daily/TRD_Dalyr.txt', 
                        delim = '\t', na = '',
        col_types = cols(
                .default = col_double(), 
                Stkcd = col_character(),  # stocks symbol
                Trddt = col_date(format = '%Y-%m-%d'),  # trading date
                Dretwd = col_skip(), Adjprcwd = col_skip(), Adjprcnd = col_skip(),
                # 1=SH-A, 2=SH-B, 4=SZ-A, 8=SZ-B, 16=startup, 32=Tech
                Markettype = col_factor(levels = c(1,2,4,8,16,32)), 
                # trading status and the late date of its capitalization changed
                Trdsta = col_factor(levels = c(1:16)), Capchgdt = col_skip()
                )
        ) %>%
        # only the category of 1 indicate that stocks were traded normally 
        filter(Trdsta == "1") %>% select(-Trdsta) %>%  
        # focus on Chinese A-Share markets
        filter(Markettype %in% as.character(c(1, 4, 16))) %>% droplevels() %>%  
        # rename the column of trading date
        rename("TradingDate" = Trddt) %>% 
        # Select a specific time period
        filter(TradingDate %within% interval('2010-01-01', '2019-12-31')) %>% 
        # sorting by stock and trading date
        arrange(Markettype, Stkcd, TradingDate) %>% 
        # generate the time series lists sort by stocks
        split(.$Stkcd)
        
# Import the daily one-year-deposit-interest-rate, free-risk return in market 
Nrrate <- read_delim(file = 'TRD_Nrrate.csv', 
                     delim = '\t', na = '',
                     col_types = cols_only(
                            Clsdt = col_date(format = '%Y-%m-%d'),
                            Nrr1 = col_factor(levels = c('NRI01', 'TBC')),
                            Nrrdaydt = col_double() # rate
                            )
        ) %>%
        # select the one-year deposit interest rate as the free-risk return
        filter(Nrr1 == 'NRI01') %>% select(-Nrr1) %>% 
        # # we could either select the Treasury rate as risk-free interest
        # filter(Nrr1 == "TBC") %>% select(- Nrr1) %>%
        rename('TradingDate' = Clsdt)

# merge with free-risk interests
TRD_Dalyr %<>% lapply(left_join, Nrrate, by = 'TradingDate')

#calculate the daily returns of stocks using closing price (dependent variable)
# R_{it} = ln(P_{it}) - ln(P_{it-1})
trd_Dret <- map(TRD_Dalyr, ~ {
                    with(.x, 
                        log(Clsprc[-1] / Clsprc[-length(Clsprc)], base=exp(1))
                        )
                    }
                )

# confirm the daily returns of stocks we calculated is same with CSMAR's ====
DRet_lgl <- map2_lgl(trd_Dret, TRD_Dalyr, ~ { 
        any(  # for all date of a stock
            near(round(.x, digits = 4),  # daily return calculate by ourselves
                 round(pull(.y, Dretnd)[-1], digits = 4)  # CSMAR's
                 )
            )
        }
    )

# weather if the daily returns for all stocks from CSMAR are same with us
if(any(DRet_lgl)) {   
        TRD_Dalyr %<>% bind_rows()
        } else print("Attention! The daily returns from CSMAR aren't same with us.")

# the transaction derivative index
MKT_Dalyr <- read_delim(file = "./Acc_Daily/STK_MKT_Dalyr.txt", 
                        delim = "\t", na = '',
                        col_types = cols(.default = col_double(),
                                         # stocks symbol, need to rename
                                         Symbol = col_character(), 
                                         TradingDate = col_date("%Y-%m-%d"), 
                                         ShortName = col_skip(), 
                                         SecurityID = col_skip(),
                                         Ret = col_skip()
                                         )
        ) %>%
        rename('Stkcd' = Symbol) %>% 
        filter(TradingDate %within% interval('2010-01-01', '2019-12-31'))

left_join(TRD_Dalyr, MKT_Dalyr) %>% 
        dbWriteTable(QEA_db, "daily", .)

rm(TRD_Dalyr, MKT_Dalyr, trd_Dret, DRet_lgl); gc()



# Part II, prepare the data to reproduce Fama-French factor model  --------

# Import the daily trading in China A-Share markets
trddat <- dbGetQuery(QEA_db, 
        "SELECT Markettype, Stkcd, TradingDate, Clsprc, Dretnd, Dsmvosd, Nrrdaydt
         FROM daily") %>% 
        # transform the record of trading date to date form
        mutate(TradingDate = as.Date(TradingDate, origin = "1970-01-01")) %>% 
        # Merge the trading data with risk-free rate
        split(.$Stkcd) # split by stock

dbDisconnect(QEA_db)

# Input the listing date and industry of stocks 
# for wiping out the stocks listed newly 
AF_Co <- read_delim(file = 'AF_Co.csv', 
                    delim = '\t', na = '',
                    col_types = cols_only(Stkcd = col_character(),
                                          # industry divide standard
                                          IndClaCd = col_factor(),
                                          # industry symbol
                                          Indus = col_character(),  
                                          Listdt = col_date('%Y-%m-%d')
                                          )
        ) %>% 
        # just using the 2012 Edition Industry Classification
        # published by China Securities Regulatory Commission 
        filter(IndClaCd == 2) %>% select(-IndClaCd) 
# take a look at the numbers of stocks listed in a year
ggplot(data=count(AF_Co, year = year(Listdt), Industry = str_sub(Indus, 1, 1))) +
        geom_col(aes(x = year, y = n, fill = Industry)) +
        labs(x = "Year", y = "The count of firms listed in this year") +
        theme_bw()
# export this image
ggsave(filename = glue("./Stock-Indus-His.pdf"),
       width = 16, height = 9, dpi = 300, units = "in", limitsize = F
       ) 

# join the information of stocks listed date to trading data
trddat %<>% `[`(intersect(names(.), pull(AF_Co, Stkcd))) %>% 
        lapply(inner_join, AF_Co, by = 'Stkcd') %>% 
        lapply(arrange, TradingDate)

# the structure of shares of stocks (annual capitalization)
Nshr <- read_delim(file = "./Acc_Annual/CG_Capchg.txt", 
                   delim = '\t', na = '',
                   col_types = cols_only(Stkcd = col_character(),
                                         # the annual interval
                                         Reptdt = col_date(format = "%Y-%m-%d"),
                                         # non-circulation
                                         Nshrnn = col_double(), 
                                         # A-shares under circulation
                                         Nshra = col_double()
                                         )  
        ) %>% 
        # take a sum of non-circulation shares and A-shares
        transmute(Stkcd, Reptdt, shrttl = Nshrnn + Nshra) %>% 
        # for merge convenience, just extraction the year from report date
        # be sure that the capitalization is a annual data in earning reports
        mutate(Reptdt = year(Reptdt)) %>% 
        arrange(Reptdt)

# Setup trading day in Chinese A-share markets
trdday <- read_delim(file = 'TRD_Cale.csv', delim = '\t', na = '',
                     col_types = cols(
                            # 1=SH-A, 2=SH-B, 4=SZ-A, 8=SZ-B, 16=startup, 32=Tech
                            Markettype = col_factor(c(1,2,4,8,16,32)),
                            Clddt = col_date(format = "%Y-%m-%d"),
                            # c(0:6): Sun < Mon < Tue < Wed < Thu < Fri < Sat
                            Daywk = col_factor(c(0:6)),
                            # "Open" or "Closed", stock share were traded or not
                            State = col_factor(levels = c('C', 'O'))
                            )
        ) %>% 
        rename('TradingDate' = Clddt) %>%
        # trading date in China A-Share markets
        filter(Markettype %in% as.character(c(1,4,16))) %>% droplevels() %>% 
        # columns by market class
        spread(Markettype, State) %>% na.omit()
# confirm the trade calendar is same among different kinds of share market
if (sum(all.equal(trdday$`1`, trdday$`4`),
        all.equal(trdday$`1`, trdday$`16`),
        all.equal(trdday$`4`, trdday$`16`)
        ) == 3L
   ) {  # we take the trading dates of Shanghai A-share as China A-share market's
        trdday %<>% filter(.$`1` == "O") %>% pull(TradingDate)
} else print("Attention! The trading dates among China A-share markets are different.")

# for tidy convenience, we store above data as a image  
save(AF_Co, Nrrate, Nshr, trddat, trdday, 
     file = "./PrePotfol.RData")
