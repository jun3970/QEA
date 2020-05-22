library(tidyverse)
library(magrittr)
library(lubridate)

library(glue)
library(broom)
library(multipanelfigure)
library(RColorBrewer)
library(ggthemes)
# display.brewer.all()

# function to read csv file with term 
mod_read <- function(file_name){ # just need the foot file name
    paste(Accprd, Pretype, Markettype, file_name, sep = '_') %>% 
    paste0(getwd(), "/", ., ".csv") %>%
    read_csv(na = '', # for comma
        col_types = cols(.default = col_double(),
            Stkcd = col_character(),
            TradingDate = col_date("%Y-%m-%d"), 
            Listdt = col_date("%Y-%m-%d"),  # listed date
            Accper = col_date("%Y-%m-%d"),  # accounting period
            Annodt = col_date("%Y-%m-%d"),  # announcement date 
            Markettype = col_factor(levels = c(1,4,16)),
            Indus = col_factor(),  # industry category
            Annowk = col_factor()  # the day of week
            )
        )
    }

# function to write csv file with term 
mod_write <- function(var, file_name) { 
    paste(Accprd, Pretype, Markettype, Modeltype, grp_num, file_name, sep = '_') %>%
        paste0(datdir, "/", ., '.csv') %>% 
            write.csv(var, file = ., quote = F, row.names = F)
}

# function to export figure with term 
mod_figure <- function(file_name, fig_ratio, fig_size) { 

    full_path <- paste(Accprd, Pretype, Markettype, Modeltype, grp_num, 
                       "Figure", file_name, sep = '_') %>%
        paste0(datdir, "/", ., '.pdf')
    
    if (fig_ratio == 1L) {  # square
        ggsave(full_path, width = 9 * fig_size, height = 9 * fig_size,
               dpi = 300, units = "in", limitsize = F)
    } else if (fig_ratio == 2L) {  # 4/3 ration
        ggsave(full_path, width = 8 * fig_size, height = 6 * fig_size,
               dpi = 300, units = "in", limitsize = F)
    } else if (fig_ratio == 3L) {  # 16/9 ration
        ggsave(full_path, width = 16 * fig_size, height = 9 * fig_size,
               dpi = 300, units = "in", limitsize = F)
    } else {print("Please customize the image output process!")}
  
}


###### Part I, import data and assign paremeter ####### 
setwd('~/OneDrive/Data.backup/QEAData/')

# the listing date and idustry infromation of stocks 
AF_Co <- read_delim('AF_Co.csv', delim = '\t', na = '',
        col_types = cols_only(Stkcd = col_character(),
            IndClaCd = col_factor(),  # industry category
            Indus = col_character(),  # industry symbol
            # Indnme = col_character(),  # industry name
            Listdt = col_date('%Y-%m-%d')  # listed date
            ) 
        ) %>% 
        # 2012 Edition Industry Classification (China Securities Regulatory Commission)
        filter(IndClaCd == "2") %>% select(-IndClaCd) 

# the paying attention resource
AF_Cfeature <- read_delim('./Acc_Annual/AF_CFEATUREPROFILE.txt', delim = '\t', na = '',
        col_types = cols_only(Stkcd = col_character(),
            Accper = col_date(format = "%Y-%m-%d"),
            CompanySize = col_double(),
            # the number of analysis teams
            AnaAttention = col_integer(), 
            # the number of analysis reports
            ReportAttention = col_integer(), 
            # the information opacity of company (disclosure by stock exchange)
            CompanyOpacity = col_character()
            )
        )

# Import the status data of quarterly financial report 
## Note that we observe that the symbols of these stocks are begin with number c(0, 3, 6),
## so we use regular expression and string function 'grepl' to get them through function `filter` 
ReptInfo <- read_delim('./Acc_Quarter/IAR_Rept.txt', delim = '\t', na = '',
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
             Profita = col_double(), Erana = col_double()
             )
        ) %>% 
        arrange(Stkcd, Accper) %>% 
        # select the stocks which are belong to China A-Share markets
        filter(grepl('^[0-6]', Stkcd)) 

# Attention! there has some problem observations, we choose to delete them
if (nrow(problems(ReptInfo)) >= 0L)  ReptInfo %<>% `[`(-problems(.)$row, ) 


# assign basical parameter =====

# focus on a specific quarter 
Accprd <- ymd('2017-09-30')
# specify the parameters of type property
Pretype <- 6L
Markettype <- 21L
# set working directory
setwd(file.path(getwd(), "CH3", year(Accprd)))
# set export directory
datdir <- file.path(getwd(), Accprd)
if (!dir.exists(datdir))  dir.create(datdir)

# import data of estimate window
stkest <- mod_read("stkest") %>% split(.$Stkcd)
# import data of event window
stkeve <- mod_read("stkeve") %>% split(.$Stkcd)
# just need to select annual data of this year
AF_Cfeature %<>% filter(year(Accper) == year(Accprd))

# if we just want to analysis a sub-sample?
## if the value of `subsam` is a number,
## we will structure a sample to implement our analysis
subsam <- FALSE
# subsam <- 300L
if (is.numeric(subsam)) { 
    set.seed(subsam)
    stk_sam <- names(stkeve) %>% sample(size = subsam)
    stkest %<>% `[`(stk_sam)
    stkeve %<>% `[`(stk_sam)
} else if (`==`(subsam, FALSE)) print("We will analysis the whole sample.")

# the number of stocks in our sample
N <- length(stkeve)
# the time series period of event window
TS <- nrow(stkeve[[N]])
# the timeline of event window
timeline <- (-(TS - 1L) / 2):((TS - 1L) / 2)

########## classo (zhan gao)#########

# library(classo)
# library(Rmosek)

# K_num <- 3L  # the number of groups we want
# winlen <- nrow(stkest[[N]])  # the length of estimate window
# Ret <- bind_rows(stkest) %>% pull(Dret_rf) %>% as.matrix()
# CH3 <- bind_rows(stkest) %>% select(mkt_rf, SMB, VMG) %>% as.matrix()
# lambda <- as.numeric( 0.5 * var(Ret) / (winlen^(1/3)) )

# pls_out <- PLS.cvxr(N, winlen, Ret, CH3, K = K_num, lambda = lambda)


########## Part II, Read the result from MATLAB, PLS(su, 2016) #########

# Group information
PLSclus <- '~/OneDrive/Data.backup/QEAData/Matlab_PLS' %>% 
        file.path(paste("group", Accprd, Pretype, Markettype,
                        'CH3.csv', sep = '_')) %>% 
        read_csv(col_types = cols( g_PLS = col_factor() ))

# the number of groups by PLS (su, 2016)
grp_num <- PLSclus$g_PLS %>% levels() %>% length()
# relevel the prder of group levels
# This arrange process is very important!
# without it, the order of groups in plot will be incorrect.
PLSclus$g_PLS %<>% fct_relevel( as.character(1:grp_num) )

# rename the group identity
if (grp_num == 2) {
        grp_name <- c("group one", "group two")
} else if (grp_num == 3)  {
        grp_name <- c("group one", "group two", "group three")
} else if (grp_num == 4) {
        grp_name <- c("group one", "group two", "group three", "group four")
} else print("The group number is not included in script!")

# PLS coefficients
PLScoef <- '~/OneDrive/Data.backup/QEAData/Matlab_PLS' %>% 
      file.path(paste("PLS", Accprd, Pretype, Markettype, 
                      'CH3.csv', sep = '_')) %>%  
      read_csv(col_types = cols(.default = col_double())) 
# rename the columns name
if (`!=`(names(PLScoef)[1:3], 
         paste0('g', 1L, '_', c('coef', 'sd', 't'))) %>% any()
    ) {for (i in 1:grp_num) {
           names(PLScoef)[seq(1:3) + 3*(i - 1)] <-
           paste0('g', i, '_', c('coef', 'sd', 't'))
           }
       }

# assign the model type term ====
# Asset pricing model, CAPM, CH3 or FF5? 
if (nrow(PLScoef) == 3) {
        Modeltype <- 'CH3'
        fct_name <- c("mkt_rf", "SMB", "VMG")
        mod_lm <- function(df) lm(Dret_rf ~ mkt_rf + SMB + VMG, data = df)
} else if (nrow(PLScoef) == 5) {
        Modeltype <- 'FF5'
        fct_name <- c("mkt_rf", "Five_SMB", "Five_HML", "RMW", "CMA")
        mod_lm <- function(df) lm(Dret_rf ~ mkt_rf + Five_SMB + Five_HML + RMW + CMA, data = df)
} else {
        Modeltype <- 'CAPM'
        fct_name <- c("mkt_rf")
        mod_lm <- function(df) lm(Dret_rf ~ mkt_rf, data = df)
        }


####### Part III, visual the group external relationship #########
# link the PLS cluster result with stokcs feature and 
# accounting indicators from quarterly earnings report 

# rename the group identity using in plot (legend guide)
PLSclus$g_PLS %<>% factor(levels = 1:grp_num, labels = grp_name) 
# 1 = group one, 2 = group two, 3 = group three...

# history 
left_join(PLSclus, AF_Co, by = "Stkcd") %>% 
    ggplot(aes(x = Listdt, group = g_PLS)) +
        geom_density(aes(color = g_PLS)) +
        labs(title = "The density curve of stock's listed date") +
        scale_color_brewer(palette = "Set1") +
        theme_economist() + # scale_y_continuous(breaks = NULL) +
        theme(axis.title = element_blank(),
              axis.text.y = element_blank(),
              legend.title = element_blank()
              )

mod_figure("Density-ghistory", 2L, 1)

# industry 
left_join(PLSclus, AF_Co, by = "Stkcd") %>% 
    mutate("Indus" = str_sub(Indus, 1L, 1L)) %>% 
        count(Indus, g_PLS) %>% 
            ggplot(aes(x = Indus, y = n, group = g_PLS, fill = g_PLS)) +
                geom_col(position = "stack") +
                labs(title = "The count of the number of stocks in a industry",
                     x = "Industry", y = "Count") +
                scale_color_brewer(palette = "Set1") + 
                coord_flip() + 
                theme_economist() + 
                theme(legend.title = element_blank(),
                      axis.title.y = element_text(margin = margin(r = 10))
                      ) 

mod_figure("histogram-gIndustry", 2L, 1.2)


# the information from ReptInfo ====
# EPS, quarterly 
f_acc_eps <- left_join(PLSclus, filter(ReptInfo, Accper == Accprd), by = "Stkcd") %>% 
    ggplot(aes(x = g_PLS, y = Erana, fill = g_PLS)) +
        geom_boxplot() + ylim(-1, 3) +
        labs(title = glue("Earnings per share (current quarter)")) +
        scale_fill_brewer(palette = "Set1") +
        theme_bw() +
        theme(plot.title = element_text(size = 12),
            axis.title = element_blank(),
            legend.position = "none"
            )

# the day of week that the report was announced
left_join(PLSclus, filter(ReptInfo, Accper == Accprd), by = "Stkcd") %>%
    count(g_PLS, Annowk) %>% 
        ggplot(aes(x = Annowk, y = n, group = g_PLS, fill = g_PLS)) +
            geom_col(position = "dodge") +
            labs(y = "Count", 
title = "The count of quarterly earning reports announcemented at which day of week") + 
            scale_x_discrete(breaks = c("0","1","2", "3", "4", "5", "6"),
                labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")) +
            scale_color_brewer(palette = "Set1") +
            theme_economist() +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_text(margin = margin(r = 10)),
                  legend.title = element_blank()
                  ) 

mod_figure("histogram-gWeekday", 2L, 1.4)


# the information from AF_Cfeature
g_accfea <- left_join(PLSclus, AF_Cfeature, by = "Stkcd") 
# size, annual
f_acc_size <- ggplot(g_accfea, aes(x = g_PLS, y = log(CompanySize), fill = g_PLS)) +
  geom_boxplot() +
  labs(title = glue("Firm size (log) at annual {year(Accprd)}")) +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  theme(plot.title = element_text(size = 12),
        axis.title = element_blank(),
        legend.position = "none"
        )

# company size and company opacity
select(g_accfea, Stkcd, g_PLS, CompanySize, CompanyOpacity) %>% 
    na.omit() %>% # remove NA in opacity category
    ggplot(aes(x = CompanyOpacity, y = log(CompanySize))) +
        geom_boxplot(aes(color = CompanyOpacity)) + 
        labs(y = "The logarithm of company size",
    title = "The relationship of the company size (log) and company opacity") +
        facet_grid( ~ g_PLS) +
        theme_bw() +
        scale_color_brewer(palette = "Set1") +
        theme(axis.title.x = element_blank(),
              legend.position = "none")

mod_figure("histogram-gAttention", 2L, 1.2)

# the internal distrubution of different opacity levels firms in wach group
count(g_accfea, g_PLS, CompanyOpacity) %>% na.omit() %>%
    ggplot(aes(x = g_PLS, y = n)) +
        geom_col(aes(fill = CompanyOpacity), position = "dodge") +
        labs(y = "Count", fill = "Company Opacity",
    title = "Companies that with different levels of opacity within each group") +
        theme_economist() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(margin = margin(r = 10))
              )

mod_figure("histogram-gOpacity", 2L, 1)

g_accfea %<>% gather(AttentionType, Attention, AnaAttention, ReportAttention) %>% 
        arrange(Stkcd)

g_accfea$AttentionType %<>% factor(levels = c("AnaAttention", "ReportAttention"),
                                   labels = c("Analysis team", "Disclosure report"))

title_char <- paste0("The relationship of the number of research reports or ", 
    "analysis teams paying attention to a stock ",
    "\nand the company opacity category disclosured by Chinese stock exchange")

group_by(g_accfea, CompanyOpacity, AttentionType) %>% 
    summarise("avgAttention" = mean(Attention, na.rm = TRUE)) %>% 
        na.omit() %>%  # remove NA in opacity category
            ggplot(aes(x = CompanyOpacity, y = avgAttention, fill = AttentionType)) +
                geom_col(position = "dodge") +
                labs(title = title_char,
                     x = "The category of company opacity", 
                     y = "Count (average)") +
                scale_color_brewer(palette = "Set1") +
                theme_economist() +
                theme(legend.title = element_blank(),
                      axis.title.y = element_text( margin =  margin(r = "10") ),
                      axis.title.x = element_text( margin =  margin(t = "10") )
                      )

mod_figure("histogram-Opacity", 2L, 1.5)


######## Part IV, Calculate AR and CAR within event window ##########

# the correlation coefficient between explanation variables ====
stkest_ff <- lapply(stkest, select, 
                    c("Stkcd", "Dret_rf", all_of(fct_name))
                    ) %>% 
    bind_rows() %>% group_nest(Stkcd) %>% 
    mutate("cor_var" = map(data, ~ cor( select(.x, - c("Dret_rf")) ))) 

# Obtain the OLS estimate parameters =====
# function `tidy` return estimate, std.error, statistic, p.value 
stkest_ff %<>% mutate("lm" = map(data, ~ mod_lm(.x) %>% tidy))
OLScoef <- select(stkest_ff, Stkcd, lm) %>% unnest(cols = "lm")
# save the OLS estimate, compared with MATLAB's
mod_write(OLScoef, "OLScoef")

# gather the data in panel form to plot the distrubution of estimators
QEA_ggOLS <- gather(OLScoef, key = Statistics, value = Parameter, estimate:p.value) %>% 
        # term obtain the explanation variables and intercept in our model
        arrange(Stkcd, term) %>% 
        # add the cluster result of PLS
        full_join(PLSclus, ., by = "Stkcd")

# the distribution of values of intercept
filter(QEA_ggOLS, term == "(Intercept)" & Statistics == "estimate") %>% 
    ggplot(aes(x = Parameter, fill = g_PLS )) +
        geom_histogram(bins = 30L) +
        labs(x = "Estimated intercept",
title = "The distribution of estimated intercept (OLS) of Fama-French multi-factor model") +
        theme_economist() +
        theme(legend.title = element_blank(),
              axis.title.y = element_blank(),
              axis.title.x = element_text(margin = margin(t = 7)),
              axis.text.y = element_blank()
              )

mod_figure("Intercept", 1L, 1)

# the distribution of coefficients of explanation variables 
f_coef <- filter(QEA_ggOLS, term != "(Intercept)" & Statistics == "estimate") %>% 
    ggplot(aes(x = Parameter, fill = g_PLS )) +
        geom_histogram(bins = 60) + 
        scale_fill_brewer(palette = "Set1") +
        facet_grid( term ~ .) +
        labs(y = "Count", 
title = "The distribution of OLS estimators of Fama-French multi-factor model") +
        theme_bw() +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_text(face = c("italic"), margin = margin(r = 10)),
              plot.title = element_text(size = 12),
              legend.position = "none")


############ AR and CAR ################

# calculate AR ====
OLScoef %<>% split(.$Stkcd) 

if (any(names(OLScoef) == names(stkeve))) {
  
    QEA_AR <- map2(stkeve, OLScoef, function(x, y) {
        Dret_pred <- `%*%`(as.matrix( select(x, all_of(fct_name)) ), 
            diag( subset(y, term %in% fct_name, select = "estimate", drop = TRUE) )
                           ) %>% 
            rowSums() %>% 
            `+`(subset(y, term == "(Intercept)", select = "estimate", drop = TRUE))
        
        AbRet <- tibble("Stkcd" = pull(x, Stkcd),
                        "TradingDate" = pull(x, TradingDate), 
                        "AbRet" = pull(x, Dret_rf) - Dret_pred)
        } 
    )
} else print("Please correct the order of the stocks in the file `OLScoef` and `stkeve`.")


# add the number of stocks in each group with group identity using in path of CAR
grp_car_name <- count(PLSclus, g_PLS) %>% # the number of stocks in each group
        unite(g_PLS, n, col = "grp_car_name", sep = " / ") %>% 
        pull("grp_car_name")

# to scientific display the group identify, we return to the integer
PLSclus$g_PLS %<>% factor(levels = grp_name, labels = as.character(1:grp_num)) 
# we structure the abnormal return in two form
# the first, we composition their values in panel
bind_rows(QEA_AR) %>% full_join(PLSclus, ., by = "Stkcd") %>% mod_write("gAR")
# the second, layout in columns (tau)  and use it to calculate the mean of AR_tau
QEA_gAR <- map(QEA_AR, "AbRet") %>% bind_cols() %>% 
        t() %>% as.data.frame() %>% 
        set_names(timeline) %>% 
        rownames_to_column("Stkcd") %>% 
        # add the group information
        inner_join(PLSclus, ., by = "Stkcd")


# calculate CAR ====
# Note that the transpose process `t` is very important,
# Because of the matrix is stored by columns in R
QEA_gCAR <- select(QEA_gAR, -c(Stkcd, g_PLS)) %>% as.matrix() %>% 
        apply(1, cumsum) %>%  # cumulative sum day by day
        t() %>% as.data.frame() %>%
        cbind(select(QEA_gAR, c(Stkcd, g_PLS)), .)


# plot the path of CAR  =====
QEA_ggCAR <- gather(QEA_gCAR, Timeline, CAR, as.character(timeline))
# order timeline as the squence of -40:40
QEA_ggCAR$Timeline %<>% factor(levels = as.character(timeline), ordered = TRUE)

QEA_ggCAR %<>% group_by(g_PLS, Timeline) %>% 
        # using the group and mean function calculate the average-CAR among stocks
        summarise("avgCAR" = mean(CAR)) %>% 
        spread(g_PLS, avgCAR) %>%
        # add the unclassified CAR (mean of whole market)
        add_column(select(QEA_gAR, -c("Stkcd", "g_PLS")) %>% 
                      colMeans() %>% 
                      cumsum() %>% 
                      unname()
                   )

# Please pay attention at there!
print(names(QEA_ggCAR)) 
print(grp_car_name)
# we use the group information within PLSclus when we calculate the AR and CAR,
# and we will use the group identity from PLSclus set the column names.
# So there is easy to emerge a bug, we need to confirm their orders is matching.
QEA_ggCAR %<>% set_names(c("Timeline", grp_car_name, "Unclassified")) %>% 
        gather(g_PLS, CAR, -Timeline)
# re-difine the order of group level
QEA_ggCAR$g_PLS %<>% fct_relevel(c(grp_car_name, "Unclassified"))
# make the timeline as continues number for path plot (factor is category data)
QEA_ggCAR$Timeline %<>% as.character() %>% as.integer()

# plot title 
title_char <- paste0("The path of grouped cumulative abnormal returns ", 
    "\nfollowing the stocks's quarterly earnings reports at accounting period ", 
    glue("{(Accprd + days(1)) %m+% months(-3)} ~ {Accprd} were announced"))

# the index of the rect in plot of CAR
rect_index <- tibble::tribble(
    ~tier, ~xmin, ~xmax, ~ymin, ~ymax,
    1, -10,  0,  0.0025,  0.0175,
    1,   0,  5,   -0.01, -0.0025,
    2, +10, 20,   -0.02,    0.02,
    3, +25, 30,  0.0070,  0.0130,
    3, +25, 30, -0.0275,   -0.02
)

# CAR
f_car <- ggplot(QEA_ggCAR, aes(x = Timeline, y = CAR, colour = g_PLS)) + 
    geom_line() + 
    geom_point(aes(shape = g_PLS), size = 2.5) + 
        scale_color_manual(values = c(brewer.pal(grp_num, "Set1"), "#999999")) +
        scale_x_continuous(breaks = seq(-30, 30, by = 5)) + 
        labs(title = title_char, x = "Time line", y = 'Cumulative Abnormal Return (CAR)') + 
        geom_hline(yintercept = 0L, linetype = "dashed", size = 1.0) +
        theme_bw() +
        theme(plot.title = element_text(size = 14),
              axis.title.y = element_text(face = c("italic"), margin = margin(r = 10)),
              legend.position = "bottom", 
              legend.title = element_blank(),
              legend.text = element_text(size = 14, face = "bold")
              ) +
    geom_rect(data = rect_index, inherit.aes = FALSE, 
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = factor(tier)), 
        alpha = 0.2, show.legend = FALSE
              ) 
    
# calculate the standard deviation of daily returns of stocks for plot at last
QEA_Dret_sd <- lapply(stkeve, add_column, 
                      "timeline" = factor(timeline, ordered = TRUE)) %>% 
        bind_rows() %>% inner_join(PLSclus, ., by = "Stkcd") %>% 
        group_by(g_PLS, timeline) %>% 
        summarise("Dretmrf_sd" = sd(Dret_rf))

QEA_Dret_sd$timeline %<>% as.character() %>% as.integer()
QEA_Dret_sd$g_PLS %<>% factor(levels = c(1:grp_num), labels = grp_name) 
# standard deviation of stock's real daily return
f_Dret_sd <- ggplot(QEA_Dret_sd, aes(x = timeline, y = Dretmrf_sd, colour = g_PLS)) +
    geom_smooth() + 
    geom_point(aes(shape = g_PLS), size = 2) +
    scale_color_brewer(palette = "Set1") +
    scale_x_continuous(breaks = seq(-30, 30, by = 5), labels = seq(-30, 30, by = 5)) + 
    labs(x = "Time line", title = 'The standard deviation of stock\'s real daily returns') + 
    theme_bw() +
    theme(plot.title = element_text(size = 12),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "none",
          )


# Export the comprehensiv plot
multi_panel_figure(width = 320, height = 300,
                   columns = 4, rows = 5) %>%  # create a multi-figure
    fill_panel(f_car, column = 1:4, row = 1:2) %>%
    fill_panel(f_coef, column = 1:2, row = 3:5) %>% 
    fill_panel(f_Dret_sd, column = 3:4, row = 3) %>%
    fill_panel(f_acc_size, column = 3, row = 4:5) %>%
    fill_panel(f_acc_eps, column = 4, row = 4:5) %>%
    save_multi_panel_figure(glue("{datdir}/{Accprd}_Figure_histogram-gSizeEarn.pdf"),
                            dpi = 300, limitsize = F)

