library(magrittr)
library(tidyverse)
library(lubridate)

library(glue)
library(broom)
#library(grid)
library(multipanelfigure)
library(RColorBrewer)
# display.brewer.all()

# function to read csv file according to type term 
mod_read <- function(x){ # just need the foot file name
    paste(Accprd, Pretype, Markettype, x, sep='_') %>% 
    paste0(getwd(), "/", ., ".csv") %>%
    read_csv(na = '', # for comma
         col_types = cols(.default = col_double(),
              Stkcd = col_character(),
              TradingDate = col_date(format = "%Y-%m-%d"),
              Listdt = col_date(format = "%Y-%m-%d"),
              Annodt = col_date(format = "%Y-%m-%d"),
              Accper = col_date(format = "%Y-%m-%d"),
              Markettype = col_factor(levels = c(1,4,16)),
              Indus = col_factor(),
              Annowk = col_factor())
               )
}


# function to write csv file according to type term 
mod_write <- function(x, y) { # the variable and the file foot name (character)
  paste(Accprd, Pretype, Markettype, Modeltype, grpnum, y, sep='_') %>%
    paste0(datdir, "/", ., '.csv') %>% 
    write.csv(x, file=., quote=F, row.names = F)
}


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


###### Part I, import data and assign paremeter ####### 

# assign basical parameter =======
setwd('~/OneDrive/Data.backup/QEAData/CH3/2017/')
# focus on a specific quarter ======
Accprd <- as.Date('2017-09-30', origin = '1970-01-01')
# specify parameters of cluster trem  
Pretype <- 6L
Markettype <- 21L
# export directory
datdir <- file.path(getwd(), Accprd)
if(!dir.exists(datdir)) dir.create(datdir)

# import data of estimate window
stkest <- mod_read("stkest") %>% split(.$Stkcd)
# import data of event window
stkeve <- mod_read("stkeve") %>% split(.$Stkcd)
# paste(Accprd, Pretype, Markettype, sep='_') %>% 
# paste0(".*?stkest[.]csv$") %>% 
# dir() %>% paste0(datdir, .) %>%
# read_delim(delim = ',', na = '')
stkcd <- read_csv("2017-09-30/2017-09-30_6_21_stkcd_sam.csv") %>% pull(1L)
stkest %<>% `[`(stkcd)
stkeve %<>% `[`(stkcd)

N <- length(stkeve)
T <- nrow(stkeve[[N]])
stkcd <- names(stkeve)
# the timeline of event window
timeline <- (-(T-1L)/2):((T-1L)/2)
# the length of estimate window
winlen <- nrow(stkest[[N]])


# Import data from working directory ====
setwd('~/OneDrive/Data.backup/QEAData/')
# the listing date and idustry infromation of stocks 
AF_Co <- read_delim('AF_Co.csv', delim='\t', na = '',
              col_types = cols_only(Stkcd = col_character(),
                      # industry divide standard
                      IndClaCd = col_factor(),
                      # industry symbol
                      Indus = col_character(), 
                      # # industry name
                      # Indnme = col_character(),
                      # listed date
                      Listdt = col_date('%Y-%m-%d'))) %>% 
  # 2012 Edition Industry Classification
  # published by China Securities Regulatory Commission 
  filter(IndClaCd == 2) %>% select(-IndClaCd) 

# the quarterly earnings report 
ReptInfo <- read_delim('IAR_Rept.csv', delim='\t', na = '',
                   col_types = cols_only(Stkcd = col_character(),
                           Sctcd = col_factor(levels = c(1, 2)),
                           Reptyp = col_factor(levels = c(1, 2, 3, 4)),
                           Accper = col_date(format = "%Y-%m-%d"),
                           Annodt = col_date(format = "%Y-%m-%d"),
                           Annowk = col_factor(), Profita = col_double(),
                           Erana = col_double())) %>%
  filter(Accper == Accprd) %>%
  # select the stocks belong to Chinese A-Share markets
  # wo could observe that these stocks's simpleCD are begin with number 0 to 6
  # so we use string function 'grepl'
  filter(grepl('^[0-6]', Stkcd))

# the paying attention resource
AF_Cfeature <- read_delim('AF_CfeatureProfile.csv', delim='\t', na = '',
                        col_types = cols_only(Stkcd = col_character(),
                                Accper = col_date(format = "%Y-%m-%d"),
                                CompanySize = col_double(),
                                # the number of analysis teams
                                AnaAttention = col_integer(), 
                                # the number of analysis reports
                                ReportAttention = col_integer(), 
                                CompanyOpacity= col_character())) %>%
  # just need to select annual data of this year
  filter(str_sub(Accper, 1, 4) == str_sub(Accprd, 1L, 4L))



########## classo #########
# we structure a sample to analysis the performance of abnormal returns
# set.seed(397007)
# stkeve <- sample(stkeve, 300)
# stkcd <- names(stkeve)
# stkest <- stkest[stkcd]
# 
# paste(Accprd, Pretype, Markettype, "stkcd_sam", sep='_') %>%
#   paste0(datdir, "/", ., '.csv') %>% 
#   write.csv(stkcd, file=., quote=F, row.names = F)
# 
# paste(Accprd, Pretype, Markettype, "stkest_sam", sep='_') %>%
#   paste0(datdir, "/", ., '.csv') %>% 
#   write.csv(bind_rows(stkest), file=., quote=F, row.names = F)
# 
# MATdex <- tibble("Stkcd" = rep(1:length(stkcd), each = winlen),
#                  "date" = rep(1:winlen, times=length(stkcd)))
# 
# glue("{datdir}/{Accprd}_{Pretype}_{Markettype}_stkest_MATLABindex.csv") %>%
#   write.csv(MATdex, file=., quote=F, row.names = F)

# CH3 <- bind_rows(stkest) %>% select(mkt_rf, SMB, VMG) %>% as.matrix()
# Ret <- bind_rows(stkest) %>% pull( Dret_rf) %>% as.matrix()
# lambda <- as.numeric( 0.5 * var(Ret) / (winlen^(1/3)) )
# library(classo)
# library(Rmosek)
# pls_out <- PLS.cvxr(N, winlen, Ret, CH3, K = 3, lambda = lambda)

########## Part II, Read the result from MATLAB, PLS(su, 2016) #########

# the file path of PLS result
PLSpath <- "~/R/QEA/Matlab_PLS"
# Group information
PLSclus <- file.path(PLSpath, paste("group", Accprd, Pretype, Markettype, 
                                    'CH3', "sam.csv", sep = '_')) %>% 
  read_csv() %>% rename("Stkcd" = stkcd, "g.PLS" = g_PLS)

grpnum <- PLSclus$g.PLS %>% unique() %>% length()
PLSclus$g.PLS <- factor(PLSclus$g.PLS, levels = 1:grpnum) 
PLSnum <- group_by(PLSclus, g.PLS) %>% summarise(count = n()) 
samnum <- pull(PLSnum, count)

# PLS coefficients
PLScoef <- file.path(PLSpath, paste("PLS", Accprd, Pretype, Markettype, 
                                    'CH3', 'sam.csv', sep = '_')) %>%  
  read_csv() 

for (i in 1:grpnum) {names(PLScoef)[seq(1:3) + 3*(i-1)] <- 
  paste0('g', i, '_', c('coef', 'sd', 't'))}

# assign the madel type term ====
# Asset pricing model, CAPM, CH3 or FF5? 
if (nrow(PLScoef)==3) {
    stkff <- lapply(stkest, subset, select=c(Stkcd, Dret_rf, mkt_rf, SMB, VMG))
    Modeltype <- 'CH3'
} else if (nrow(PLScoef)==5) {
    stkff <- lapply(stkest, subset, 
                    select=c(Stkcd, Dret_rf, mkt_rf, Five_SMB, Five_HML, RMW, CMA))
    Modeltype <- 'FF5'
} else {
    stkff <- lapply(stkest, subset, select=c(Stkcd, Dret_rf, mkt_rf))
    Modeltype <- 'CAPM'
}


############ Part III, visual the group relationship among stocks ##############
setwd('~/OneDrive/Data.backup/QEAData/CH3/2017/')
# we will just simply link the PLS cluster result and the basical information of stokcs 
# or quarterly earnings report
# PLS-group's properties ====
# In this part, for the axis name tidiness (in plot) we use the PLSclusname as cluster resource.  
# In Part IV, we calculate the abnarmal return, we still using the original cluster indentity 
# 1 = group one, 2 = group two, 3 = group three...
PLSclusname <- PLSclus
# rename the group identity
if(grpnum == 2) {grpname <- c("group one", "group two")
} else if(grpnum == 3)  {grpname <- c("group one", "group two", "group three")
} else if (grpnum == 4) {grpname <- c("group one", "group two", "group three", "group four")
} else print("The group number is not included in script!")
PLSclusname$g.PLS %<>% factor(levels = 1:grpnum, labels = grpname) 

# merge the PLS cluster result with firm industry information
g_pty <- inner_join(PLSclusname, AF_Co, by = "Stkcd") 

# history ======
ggplot(data = g_pty, aes(x = Listdt, group = g.PLS, colour = g.PLS)) +
  geom_density() +
  labs(title = "The density curve of stock's listed date",
       x = "Listed date", y = "Density") +
  scale_color_brewer(palette = "Set1") +
  theme_bw() + scale_y_continuous(breaks=NULL) +
  theme(plot.title = element_text(size=14), 
        axis.text.y = element_blank(),
        axis.title.y = element_text(face = "bold"),
        legend.title = element_blank(),
        legend.position = c(.10, .90),
        legend.text = element_text(size=12))


mod_figure("Density-ghistory", 2L, 1)

# industry =====
g_pty %>% group_by(Indus, g.PLS) %>% summarise(count = n()) %>% 
  ggplot(aes(x = Indus, y = count)) +
    geom_col(aes(group = g.PLS, fill = g.PLS)) +
    labs(x = "Industry", y = "Count") + 
    scale_color_brewer(palette = "Set1") +
    facet_grid(cols = vars(g.PLS)) + 
    coord_flip() + theme_bw() + 
    theme(legend.position = 'none') 

mod_figure("histogram-gIndustry", 1L, 1.4)


# accounting feature =====
# Notice that the information of AF_Cfeature is annually,
# but the ReptInfo's is quarterly
g_acc <- inner_join(ReptInfo, PLSclusname, by = "Stkcd")
g_accfea <- inner_join(AF_Cfeature, PLSclusname, by = "Stkcd")

# report information
g_rpt <- group_by(g_acc, g.PLS, Annowk) %>% summarise(Count = n())
g_rpt$Annowk %<>% factor(levels = c(0:6),
                         labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

ggplot(g_rpt, aes(x = Annowk, y = Count, group = g.PLS, fill = g.PLS)) +
  geom_col() +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        legend.position = c(0.1, 0.9),
        legend.title = element_blank(),
        legend.text = element_text(size=12))

mod_figure("histogram-gWeekday", 2L, 1.4)

# Attention 
# this figure maybe interesting linked to the path of car

g_accfea %<>% gather(AttentionType, Attention, AnaAttention, ReportAttention) %>% arrange(Stkcd)
g_accfea$AttentionType %<>% factor(levels = c("AnaAttention", "ReportAttention"),
                                  labels = c("Analysis team", "Report"))
ggplot(g_accfea, mapping = aes(x=g.PLS,  y = Attention)) +
  geom_boxplot() + 
  labs(y="The number of research reports or analysis teams paying attention to a stock") +
  facet_grid(CompanyOpacity ~ AttentionType) +
  theme_bw() +
  theme(axis.title.x = element_blank())

mod_figure("histogram-gAttention", 3L, 1.2)



########## Part IV, Calculate AR and CAR within event window #############

# the correlation coefficient between explanation variables: FF3
stkff %<>% bind_rows() %>% group_nest(Stkcd) %>% 
 mutate(cor = map(data, ~ .x %>% select(mkt_rf, SMB, VMG) %>% cor())) 

# Obtain the OLS estimate parameters =====
mod_lm <- function(df) lm(Dret_rf ~ mkt_rf + SMB + VMG, data = df)
stkff %<>% mutate(lm = map(data, mod_lm)) %>% 
  mutate(lm.estimate = map(lm, tidy))

OLScoef <- stkff %>% transmute(Stkcd, OLS = map(lm.estimate, as_tibble))
OLScoef <- OLScoef$OLS %>% bind_rows() %>% 
  add_column(Stkcd = rep(OLScoef$Stkcd, each = 1L + nrow(PLScoef) )) %>% 
  select(., ncol(.), 1:(ncol(.)-1) )
# save the result, we could campare it with MATLAB's
mod_write(OLScoef, "OLScoef")

# plot =====
ggOLS <- gather(OLScoef, key = statistics, value = Parameter, 
                          estimate, std.error, statistic, p.value) 
ggOLS$term %<>% factor(levels = c("(Intercept)", "mkt_rf", "SMB", "VMG"),
                       labels = c("Intercept", "mkt_rf", "SMB", "VMG"))
ggOLS$statistics %<>% factor(levels = c("estimate", "std.error", "statistic", "p.value"))
ggOLS %<>% full_join(PLSclusname, by = "Stkcd")


filter(ggOLS, term == "Intercept" & statistics == "estimate") %>% 
  ggplot(aes(x=Parameter, fill = g.PLS )) +
  geom_histogram(bins = 16L) +
  labs(x = "Intercepte",
       title = "The distribution of estimated intercept of Fama-French factor model") +
  theme_bw() +
  theme(legend.position = "none")

mod_figure("Intercept", 2L, 1)


############ Calculate AR ################
OLScoef %<>% split(.$Stkcd) 
QEA_AR <- c(stkeve, OLScoef) %>% split(names(.)) %>% 
  lapply(function(x){
    Dretfit <- "%*%"(as.matrix(subset(x[[1]], select = pull(x[[2]], term)[-1])), 
                     diag( pull(x[[2]], estimate)[-1] )) %>% 
                rowSums() %>% 
                `+`(pull(x[[2]], estimate)[1])
    AbRet <- pull(x[[1]], Dret_rf) - Dretfit
    AbRet <- tibble("Stkcd" = pull(x[[1]], Stkcd),
                    "TradingDate" = pull(x[[1]], TradingDate), 
                    "AbRet" = AbRet)
  })

bind_rows(QEA_AR) %>% inner_join(PLSclus) %>% mod_write("gAR")

# AR with group information and to calculate the mean of AR_tau
QEA_gAR <- map(QEA_AR, "AbRet") %>% bind_cols() %>% 
  t() %>% as.data.frame() %>% 
  set_names(timeline) %>% 
  rownames_to_column("Stkcd") %>% 
  inner_join(PLSclus, by = "Stkcd")


############ Calculate CAR ################

# calculate CAR ====
QEA_gCAR <- map(QEA_AR, "AbRet") %>% bind_cols() %>% 
  # make the AR rows by stock 
  t() %>% data.frame() %>% 
  set_names(timeline) %>%
  # cumulative sum day by day
  apply(1, cumsum) %>% 
  # This is transpose is very important!
  # Because of the real cell order in R (a matrix is stored by columns)
  t() %>% as.data.frame() %>% 
  rownames_to_column("Stkcd") %>% 
  # merge with group information
  inner_join(PLSclus, by = "Stkcd")


# plot the path of CAR  =====
# 
# to calculate the standard deviation of daily return 
QEA_Dret_sd <- lapply(stkeve, add_column, "timeline" = factor(timeline, ordered = TRUE)) %>% 
  bind_rows() %>% inner_join(PLSclus) %>% 
  group_by(g.PLS, timeline) %>% 
  summarise("Dretmrf_sd" = sd(Dret_rf))
QEA_Dret_sd$timeline %<>% as.character() %>% as.integer()


# gathered CAR with group for data visualization with ggplot
QEA_ggCAR <- gather(QEA_gCAR, Timeline, CAR, as.character(timeline))
# make the timeline as ordered factor (-40:40)
QEA_ggCAR$Timeline %<>% factor(levels = as.character(timeline))
QEA_ggCAR %<>% group_by(g.PLS, Timeline) %>% 
  # using the group and mean function calculate the average-CAR among stocks
  summarise("avgCAR" = mean(CAR)) %>% 
  # add the unclassified CAR (mean of whole market)
  spread(g.PLS, avgCAR) %>%
  add_column(colMeans(QEA_gCAR[,-c(1, ncol(QEA_gCAR))])) 

if(grpnum == 2) {colnames(QEA_ggCAR) <- 
     c("Timeline", "group one", "group two", "Unclassified")
} else if(grpnum == 3)  {colnames(QEA_ggCAR) <- 
     c("Timeline", "group one", "group two", "group three", "Unclassified")
} else if (grpnum == 4) {colnames(QEA_ggCAR) <- 
     c("Timeline", "group one", "group two", "group three", "group four", "Unclassified")
} else print("The group number is not included in script!")

# gather in group
QEA_ggCAR %<>% gather(g.PLS, CAR, "+"(1:(grpnum+1L),1L))

# re-difine the group level (order)
if(grpnum == 2) {
  grplevel <- c("Unclassified", "group one", "group two")
} else if(grpnum == 3)  {
  grplevel <- c("Unclassified", "group one", "group two", "group three")
} else if (grpnum == 4) {
  grplevel <- c("Unclassified", "group one", "group two", "group three", "group four")
} else print("The group number is not included in script!")
QEA_ggCAR$g.PLS %<>% factor(levels = grplevel)

# make the timeline as continues number for path plot (factor is category data)
QEA_ggCAR$Timeline %<>% as.character() %>% as.integer()


# plot title 
titchar <- glue("The path of grouped cumulative abnormal return (g-CAR) that could be attributed to the quarterly earnings announcement of accounting period {(Accprd + days(1)) %m+% months(-3)} ~ {Accprd}")

# create a multi-figure
f_acc <- multi_panel_figure(
  width = 16, height = 15,
  unit = "in",
  columns = 4, rows = 5)

# CAR
f_car <- ggplot(QEA_ggCAR, aes(x = Timeline, y = CAR, colour = g.PLS)) + 
  geom_line(aes(linetype = g.PLS)) + 
  geom_point(aes(shape = g.PLS), size = 2) + 
  scale_linetype_manual(values=c("dotted", "solid", 'solid')) +
  scale_color_manual(values = c("#999999", "#377EB8", "#E41A1C")) +
  scale_x_continuous(breaks=seq(-30, 30, by = 5), labels = seq(-30, 30, by=5)) + 
  labs(title = titchar, x = "Time line", y = 'Cumulative Abnormal Return (CAR)',
       caption = "Data source: WIND and CSMAR") + 
  theme_bw() +
  theme(plot.title = element_text(size=14, hjust = 0.5),
        axis.title.y = element_text(face = c("italic")),
        plot.caption = element_text(size=12, hjust = 1),
        legend.title = element_blank(),
        legend.position = "bottom", 
        legend.text = element_text(size=14, face = "bold")) 

# OLS coefficients of CH3
f_coef <- filter(ggOLS, term != "Intercept" & statistics == "estimate") %>% 
  ggplot(mapping = aes(x = Parameter, fill = g.PLS )) +
  geom_histogram(bins = 60) + 
  scale_fill_manual(values = c("#377EB8", "#E41A1C")) +
  facet_grid( term ~ .) +
  labs(y = "Count", 
       title = "The distribution of OLS estimators of Fama-French multi-factor model") +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(face = c("italic")),
        plot.title = element_text(size = 12),
        legend.position = "none")

# standard deviation of stock's real daily return
f_Dret_sd <- ggplot(QEA_Dret_sd, aes(x=timeline, y=Dretmrf_sd, colour=g.PLS)) +
  geom_smooth() + geom_point(aes(shape = g.PLS), size = 2) +
  scale_color_manual(values = c("#377EB8", "#E41A1C")) +
  scale_x_continuous(breaks=seq(-30, 30, by = 5), labels = seq(-30, 30, by=5)) + 
  labs(x = "Time line", title = 'The standard deviation of stock\'s real daily returns') + 
  theme_bw() +
  theme(plot.title = element_text(size=12),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.text = element_text(size=12))


# size 
f_acc_s <- ggplot(g_accfea, aes(x = g.PLS, y = log(CompanySize), fill = g.PLS)) +
  geom_boxplot() +
  labs(title = glue("Logarithm of firm size at year {str_sub(Accprd, 1, 4)}")) +
  scale_fill_manual(values = c("#377EB8", "#E41A1C")) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 12),
        legend.position = "none",
        legend.title = element_blank())

# EPS
f_acc_e <- ggplot(g_acc, aes(x = g.PLS, y = Erana, fill = g.PLS)) +
  geom_boxplot() + ylim(-1, 3) +
  labs(title = glue("Earnings per share in earnings reports")) +
  scale_fill_manual(values = c("#377EB8", "#E41A1C")) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        plot.title = element_text(size = 12))


# Export
f_acc %<>%
  fill_panel(f_car, column = 1:4, row = 1:2) %<>%
  fill_panel(f_coef, column = 1:2, row = 3:5) %>% 
  fill_panel(f_Dret_sd, column = 3:4, row = 3) %<>%
  fill_panel(f_acc_s, column = 3, row = 4:5) %<>%
  fill_panel(f_acc_e, column = 4, row = 4:5) %<>%
  save_multi_panel_figure(glue("{datdir}/{Accprd}_Figure_histogram-gSizeEarn.png"),
                          dpi = 300, limitsize = F)

