# This script is written for reading CSMAR data batched, 
# and then store them as tables in SQLite database and RData file.
# Importing data through querying the SQL database will solve the problem
# of memory shortage and process timeout, but the relative shortcoming is
# the data type defined in R will vanish, especially for the date and factor,
# and we need to transform the data type of them carefully.

library(tidyverse)
library(magrittr)
library(lubridate)

library(DBI)
library(RSQLite)
library(dbplyr)

library(parallel)
library(doParallel)
CL <- makeCluster(detectCores(logical = FALSE) - 1L)
registerDoParallel(CL)

# working directory
setwd("~/OneDrive/Data.backup/QEAData")
# link to SQLite database
QEA_db <- dbConnect(RSQLite::SQLite(), "./QEA_db.sqlite")

# Part I, create tables with annual, quarter, and daily data ------------
# annual =====
annual_path <- list.files(path = "./Acc_Annual",
                          pattern = "^(CSR|FAR|MNM).*[^]]\\.txt$",
                          full.names = TRUE)
annual <- foreach(i = 1:length(annual_path),
                  # the combine approach is taking collection
                  .combine = full_join,  
                  # will be used package, `readr` to read data
                  .packages = c("readr")  
                  ) %dopar%
        read_delim(annual_path[i], 
                   delim = "\t", na = '',
                   col_types = cols(.default = col_double(),
                                    Stkcd = col_character(),
                                    Accper = col_date('%Y-%m-%d'),
                                    Annodt = col_date('%Y-%m-%d'))
                   )
arrange(annual, Accper, Stkcd) %>% 
        dbWriteTable(QEA_db, "annual", .)
# quarter =====
quarter_path <- list.files(path = "./Acc_Quarter", 
                           pattern = "[0-9]\\.txt$",
                           full.names = TRUE)
quarter <- foreach(i = 1:length(quarter_path),
                   .combine = full_join,
                   .packages = c("readr")
                   ) %dopar%
        read_delim(quarter_path[i], 
                   delim = "\t", na = '',
                   col_types = cols(.default = col_double(),
                                    Stkcd = col_character(),
                                    Accper = col_date('%Y-%m-%d'),
                                    Typrep = col_character(),
                                    Indcd = col_character())
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
                                        Typrep = col_character())
                       )
arrange(CashFlow, Stkcd, Accper, Typrep) %>% 
        dbWriteTable(QEA_db, "CashFlow_Statement", .)
# Income statement
Income <- read_delim(file = "./Income_Statement/FS_Comins.txt", 
                     delim = '\t', na = '',
                     col_types = cols(.default = col_double(),
                                      Stkcd = col_character(),
                                      Accper = col_date(format = '%Y-%m-%d'),
                                      Typrep = col_character())
                     )
arrange(Income, Accper, Stkcd, Typrep) %>% 
        dbWriteTable(QEA_db, "Income_Statement", .)
# Balance sheet
Balance <- read_delim(file = "./Balance_Sheet/FS_Combas.txt", 
                      delim = '\t', na = '',
                      col_types = cols(.default = col_double(),
                                       Stkcd = col_character(),
                                       Accper = col_date(format = '%Y-%m-%d'),
                                       Typrep = col_character())
                      )
arrange(Balance, Accper, Stkcd, Typrep) %>% 
        dbWriteTable(QEA_db, "Balance_Sheet", .)

rm(annual, quarter, CashFlow, Income, Balance); gc()

# daily trading data =====
TRD_Dalyr <- read_delim(file = './Acc_Daily/TRD_Dalyr.txt', 
                        delim = '\t', na = '',
                        col_types = cols(.default = col_double(),
                                         Stkcd = col_character(),  # stocks symbol
                                         Trddt = col_date(format = '%Y-%m-%d'),  # trading date
                                         # the last date of stock capitalization changed
                                         Dretwd = col_skip(), Capchgdt = col_skip(),
                                         Adjprcwd = col_skip(), Adjprcnd = col_skip(),
                                         # 1=SH-A, 2=SH-B, 4=SZ-A, 8=SZ-B, 16=startup, 32=Tech
                                         Markettype = col_factor(levels = c(1,2,4,8,16,32)), 
                                         Trdsta = col_factor(levels = c(1:16)))  # trading status
                        ) %>%
        # only the category of 1 indicate that stocks were traded normally 
        filter(Trdsta == "1") %>% select(-Trdsta) %>%  
        # focus on Chinese A-Share markets
        filter(Markettype %in% as.character(c(1, 4, 16))) %>% droplevels() %>%  
        # rename the column of trading date
        rename("TradingDate" = Trddt) %>% 
        # sorting by stock and trading date
        arrange(Markettype, Stkcd, TradingDate) %>% 
        # confirm the daily returns of stocks we calculated is same with CSMAR's ====
        # when any value of daily returns calculated by ourselves 
        # is equal to the return data form CSMAR, we select CSMAR's data 
        group_nest(Stkcd) %>%  # generate the time series lists sort by stocks
        mutate('DR' = map_lgl(data, # R_{it} = ln(P_{it}) - ln(P_{it-1})
                              ~ `/`(.x$Clsprc[-1], .x$Clsprc[-length(.x$Clsprc)]) %>% 
                                log(base = exp(1)) %>%  
                                near(.x$Dretnd[-1]) %>% 
                                any() 
                              )
               ) %>% 
        subset(DR == TRUE, select = -DR) %>%
        unnest(cols = 'data')

# Import data of the transaction derivative index of stocks
MKT_Dalyr <- read_delim(file = "./Acc_Daily/STK_MKT_Dalyr.txt", 
                        delim = "\t", na = '',
                        col_types = cols(.default = col_double(),
                                         Symbol = col_character(), 
                                         TradingDate = col_date("%Y-%m-%d"), 
                                         ShortName = col_skip(), 
                                         SecurityID = col_skip(),
                                         Ret = col_skip())
                        ) %>%
        # stocks symbol, need to rename
        rename('Stkcd' = Symbol) 

# Import the daily one-year-deposit-interest-rate, free-risk return in market 
Nrrate <- read_delim(file = './TRD_Nrrate.txt', 
                     delim = '\t', na = '',
                     col_types = cols_only(Clsdt = col_date(format = '%Y-%m-%d'),
                                           Nrr1 = col_factor(levels = c('NRI01', 'TBC')),
                                           Nrrdaydt = col_double()  # risk-free interest
                                           )
                     ) %>%
        # select the one-year deposit interest rate as the free-risk return
        filter(Nrr1 == 'NRI01') %>% select(-Nrr1) %>% 
        # # we could either select the Treasury rate as risk-free interest
        # filter(Nrr1 == "TBC") %>% select(- Nrr1) %>%
        rename('TradingDate' = Clsdt)

left_join(TRD_Dalyr, MKT_Dalyr, by = c("Stkcd", "TradingDate")) %>% 
        left_join(Nrrate, by = 'TradingDate') %>% 
                dbWriteTable(QEA_db, "daily", .)

rm(TRD_Dalyr, MKT_Dalyr); gc()


# Part II, prepare the data to reproduce Fama-French factor model  --------
# Input the listing date and industry of stocks for wiping out the stocks listed newly 
AF_Co <- read_delim(file = 'AF_Co.csv', 
                    delim = '\t', na = '',
                    col_types = cols_only(Stkcd = col_character(),
                                          IndClaCd = col_factor(),  # industry divide standard
                                          Indus = col_character(),  # industry symbol
                                          Listdt = col_date('%Y-%m-%d'))
                    ) %>% 
        # just using the 2012 Edition Industry Classification
        # published by China Securities Regulatory Commission 
        filter(IndClaCd == 2) %>% select(-IndClaCd) 

# the paying attention resource disclosure by stock exchange, yearly data
# the market size, the number of analysis team following, 
# and how much survey papers are issued (opacity of company)
AF_Cfeature <- read_delim('./Acc_Annual/AF_CFEATUREPROFILE.txt', 
                          delim = '\t', na = '',
                          col_types = cols_only(Stkcd = col_character(),
                                                Accper = col_date(format = "%Y-%m-%d"),
                                                CompanySize = col_double(),
                                                # the number of analysis teams
                                                AnaAttention = col_integer(), 
                                                # the number of analysis reports
                                                ReportAttention = col_integer(), 
                                                # the information opacity of company 
                                                CompanyOpacity = col_character())
                          )

save(AF_Co, AF_Cfeature, file = "./FirmAttr.RData")

# Import the daily trading in China A-Share markets
trddat <- dbGetQuery(QEA_db, 
                    "SELECT Markettype, Stkcd, TradingDate, Clsprc, Dretnd, Dsmvosd
                     FROM daily"
                    ) %>% 
        # transform the record of trading date to date form
        mutate(TradingDate = as.Date(TradingDate, origin = "1970-01-01")) %>% 
        group_nest(Markettype, Stkcd) %>% 
        # join the information of stocks listed date and industry to trading data
        inner_join(AF_Co, by = 'Stkcd')

dbDisconnect(QEA_db)

# Setup trading day in Chinese A-share markets
trdday <- read_delim(file = 'TRD_Cale.csv', delim = '\t', na = '',
                     col_types = cols(# 1=SH-A, 2=SH-B, 4=SZ-A, 8=SZ-B, 16=startup, 32=Tech
                                      Markettype = col_factor(c(1,2,4,8,16,32)),
                                      Clddt = col_date(format = "%Y-%m-%d"),
                                      # c(0:6): Sun < Mon < Tue < Wed < Thu < Fri < Sat
                                      Daywk = col_factor(c(0:6)),
                                      # "Open" or "Closed", stock share were traded or not
                                      State = col_factor(levels = c('C', 'O')))
                     ) %>% 
        rename('TradingDate' = Clddt) %>%
        # trading date in China A-Share markets
        filter(Markettype %in% as.character(c(1,4,16))) %>% droplevels() %>% 
        # columns by market class
        spread(Markettype, State) %>% 
        na.omit()

# confirm the trade calendar is same among different kinds of share market
if (`&`(all.equal(trdday$`1`, trdday$`4`),
        all.equal(trdday$`4`, trdday$`16`))
    ) { # we take the trading dates of Shanghai A-share as China A-share market's
        trdday %<>% subset(`1` == "O", select = TradingDate, drop = TRUE)
} else print("Attention! The trading dates among China A-share markets are different.")

# the structure of shares of stocks (annual capitalization)
Nshr <- read_delim(file = "./Acc_Annual/CG_Capchg.txt", 
                   delim = '\t', na = '',
                   col_types = cols_only(Stkcd = col_character(),
                                         # annual interval
                                         Reptdt = col_date(format = "%Y-%m-%d"),  
                                         Nshrnn = col_double(),  # non-circulation
                                         Nshra = col_double()  # A-shares under circulation
                                         )  
                   ) %>% 
        # take a sum of non-circulation shares and A-shares
        transmute(Stkcd, Reptdt, shrttl = Nshrnn + Nshra) %>% 
        # for merge convenience, just extraction the year from report date
        # be sure that the capitalization is a annual data in earning reports
        mutate(Reptdt = year(Reptdt)) %>% 
        arrange(Reptdt)

# store above data that will be used in next script as a image  
save(trddat,  # the daily trading data, the listed date and industry of stocks
     trdday,  # the trading date of Chinese-A-Share markets
     Nrrate,  # the daily risk-free interests of capital market
     Nshr,    # the yearly situation of structure of shares of stocks
     file = "./PrePotfol.RData")

# Prepare data relative with quarterly reports in advance ====
# Import the status data of quarterly financial report 
ReptInfo <- read_delim('./Acc_Quarter/IAR_Rept.txt', 
                       delim = '\t', na = '',
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
                       )
# we could observe that the symbols of stocks belong to China A-Share markets
# are begin with number c(0, 3, 6),
# so we use regular expression and string function 'grepl' to filter others 
if (nrow(problems(ReptInfo)) >= 0L) {
        ReptInfo %<>% `[`(-unique(problems(.)$row), ) %>% 
                filter(grepl('^[0-6]', Stkcd)) %>% 
                arrange(Stkcd, Accper)
}

# structure the sub-sample according to pre-earnings report ==== 
# ForecFinReportType - Nine categories including turn to loss, continued loss, 
# turn to gain profit from loss, continued profitability, large increase,
# large decrease, slight increase, slight decrease, and uncertainty
PreRept <- read_delim('./Acc_Quarter/FIN_F_ForecFin.txt', 
                      delim = '\t', na = '', 
                      col_types = cols_only(# the symbol of stocks, need to be rename
                                            StockCode = col_character(),
                                            PubliDate = col_date(format = "%Y-%m-%d"),
                                            AccPeriod = col_date(format = "%Y-%m-%d"),
                                            # 0 = performance advance expose, 
                                            # 1 = regular disclosure
                                            Source = col_factor(),
                                            # the categories of report source
                                            ForecFinReportType = col_factor())
                      ) %>% 
        rename('Stkcd' = StockCode) %>% 
        filter(grepl('^[0-6]', Stkcd))

save(ReptInfo,  # the key accounting index in quarterly earnings reports
     PreRept,  # the pre-disclosure report of stocks which operated unexpected 
     file = "./ReportInfo.RData")

# take a look at the numbers of stocks listed in a year
ggplot(data = count(AF_Co, year = year(Listdt), Industry = str_sub(Indus, 1, 1))) +
    geom_col(aes(x = year, y = n, fill = Industry)) +
    labs(x = "Year", y = "The count of firms listed in this year") +
    theme_bw()

# export this image
ggsave(filename = glue::glue("./Stock-Indus-His.pdf"),
       width = 16, height = 9) 
