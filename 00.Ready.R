# This script is written for reading CSMAR data batched, 
# and then writen into SQLite database.
# Importing data through querying the SQL database will solve
# the problem of memory shortage and process timeout.
library(magrittr)
library(lubridate)
library(tidyverse)

library(DBI)
library(RSQLite)

library(parallel)
library(doParallel)
CL <- makeCluster(detectCores(logical = FALSE) - 1L)
registerDoParallel(CL)

# working directory
setwd("~/OneDrive/Data.backup/QEAData")
# link to database
QEA_db <- dbConnect(RSQLite::SQLite(), "./QEA_db.sqlite")

#  annual =====
annual_path <- list.files("./Acc_Annual", full.names = TRUE,
        pattern = "^(CSR|FAR|MNM).*[^]]\\.txt$")
        
annual <- foreach(i = 1:length(annual_path),
                # the combine pattern is taking inersection
                .combine = full_join,
                .packages = c("readr")) %dopar%
        read_delim(annual_path[i], delim = "\t", na = '',
                col_types = cols(.default = col_double(),
                                 Stkcd = col_character(),
                                 Accper = col_date('%Y-%m-%d'),
                                 Annodt = col_date()
                                 )
                )

dbWriteTable(QEA_db, "annual", annual)

# quarter =====
quarter_path <- list.files("./Acc_Quarter", full.names = TRUE,
          pattern = "[0-9]\\.txt$")

quarter <- foreach(i = 1:length(quarter_path),
                .combine = full_join,  # taking all set
                .packages = c("readr")) %dopar%
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
rm(annual, quarter); gc()

# daily =====
TRD_Dalyr <- read_delim('./Acc_Daily/TRD_Dalyr.txt', delim = '\t', na = '',
        col_types = cols(.default = col_double(), 
        Stkcd = col_character(),  # stocks symbol
        Trddt = col_date(format = '%Y-%m-%d'),  # trading date
        Dretwd = col_skip(), Adjprcwd = col_skip(), Adjprcnd = col_skip(),
        # 1=SH-A, 2=SH-B, 4=SZ-A, 8=SZ-B, 16=startup, 32=Tech
        Markettype = col_factor(levels = c(1,2,4,8,16,32)), 
        # trading status and the late date of its capitalization changed
        Trdsta = col_factor(levels = c(1:16)), Capchgdt = col_skip())
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
        
#calculate the daily returns of stocks using closing price (dependent variable)
# R_{it} = ln(P_{it}) - ln(P_{it-1})
trd_Dret <- map(TRD_Dalyr, ~ {with(.x, 
                log( Clsprc[-1] / Clsprc[-length(Clsprc)], base=exp(1)))})

# confirm the daily returns of stocks we calculated is same with CSMAR's
if(map2_lgl(trd_Dret, TRD_Dalyr, ~ {
        near(round(.x, digits = 4),  # daily return calculate by ourself
             round(pull(.y, Dretnd)[-1], digits = 4)) %>% # daily retrn form CSMAR
        any()}) %>%  # for all date of a stock
        any()  # for all stocks 
   ) {  # if the daily returns from CSMAR are same with us.
        TRD_Dalyr %<>% bind_rows(); rm(trd_Dret)
        } else print("Attention! The daily returns from CSMAR aren't same with us.")

# the transaction derivative index
MKT_Dalyr <- read_delim("./Acc_Daily/STK_MKT_Dalyr.txt", delim = "\t", na = '',
        col_types = cols(.default = col_double(),
                        Symbol = col_character(), # stocks symbol, need to rename
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

dbDisconnect(QEA_db)

rm(TRD_Dalyr, MKT_Dalyr, daily); gc()
