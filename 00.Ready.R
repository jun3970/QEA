# This script is written to batched read CSMAR data in txt format
library(magrittr)
library(tidyverse)
library(foreach)
library(parallel)
library(doParallel)
makeCluster(4L) %>% 
    registerDoParallel()

#  annual =====
filepath <- "~/OneDrive/Data.backup/QEAData/Acc_Annual"

annual_path <- list.files(filepath, full.names = TRUE,
           pattern = "[a-z]\\.txt$")

annual <- foreach(i = 1:length(annual_path),
        .combine = inner_join,
        .packages = c("readr")) %dopar%
    read_delim(annual_path[i], delim = "\t")

# quarter =====
filepath <- "~/OneDrive/Data.backup/QEAData/Acc_Quarter"

quarter_path <- list.files(filepath, full.names = TRUE,
                          pattern = "[0-9]\\.txt$")[-c(10,11)]

quarter <- foreach(i = 1:length(quarter_path),
                  .combine = inner_join,
                  .packages = c("readr")) %dopar%
    read_delim(quarter_path[i], delim = "\t")

quarter %<>% arrange(Accper, Stkcd)

# daily =====
