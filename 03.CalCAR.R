# This script is mainly written for calculate the abnormal returns 
# and visualizing some key features about firms attribute and accounting indicator 

library(tidyverse)
library(magrittr)
library(lubridate)

library(modelr)
library(glue)
library(broom)

library(multipanelfigure)
library(RColorBrewer)
# display.brewer.all()
library(ggthemes)

source('~/R/QEA/QEA.R/function_QEA.R')
setwd('~/OneDrive/Data.backup/QEAData')
# Import the listing date, industry, and paying attention resource of stocks
load(file = "./FirmAttr.RData")
# Import the status data of quarterly financial report 
load(file = "./ReportInfo.RData"); rm(PreRept)

# Part I, import data and assign parameter -------------------------------

# assign basic parameters about quarter and sample attributes ====
# quarters sequence
Accprd_seq <- months(seq(from = 0, by = 3, length = 4)) %>% 
        mapply('%m+%', ymd('2017-03-31'), .) %>% 
        base::as.Date(origin = '1970-01-01')
# the type of stocks that weather had publish pre-report
Pretype <- 6L  
# the market types that the stocks in our sample
Markettype <- 21L
# if we just want to analysis a sub-sample?
subsam <- FALSE
## if the value of `subsam` is a number, for example, subsam <- 300L
## the analysis results of below script will just based on a sub-sample 


for (q in seq_along(Accprd_seq)) {

    Accprd <- Accprd_seq[q] %>% print()
   
    # Part II, Read the result from MATLAB, PLS (su, 2016) ---------------------
    
    # Group information
    PLSclus <- dir(path = file.path('~/OneDrive/Data.backup/QEAData',
                                    'Matlab_PLS', 
                                    year(Accprd)
                                    ),
                   pattern = paste("group", Accprd, Pretype, Markettype, sep = '_'),
                   recursive = TRUE,
                   full.names = TRUE
                   ) %>% 
            read_csv(col_types = cols(
                            Stkcd = col_character(),
                            g_PLS = col_factor()
                            )
                     )
    # the number of groups by PLS (su, 2016)
    grp_num <- levels(PLSclus$g_PLS) %>% length()
    # re-level the order of group levels, this process is important
    PLSclus$g_PLS %<>% fct_relevel(as.character(1:grp_num))
    # rename the group identity
    if (grp_num == 2) {
            grp_name <- c("group one", "group two")
    } else if (grp_num == 3)  {
            grp_name <- c("group one", "group two", "group three")
    } else if (grp_num == 4) {
            grp_name <- c("group one", "group two", "group three", "group four")
    } else stop("The group number is not included in script!")
    
    # PLS coefficients
    PLScoef <- dir(path = file.path('~/OneDrive/Data.backup/QEAData',
                                    'Matlab_PLS', 
                                    year(Accprd)
                                    ),
                   pattern = paste("PLS", Accprd, Pretype, Markettype, sep = '_'),
                   recursive = TRUE,
                   full.names = TRUE
                   ) %>% 
            read_csv(col_types = cols(.default = col_double()))
    # rename the columns name
    for (i in 1:grp_num) names(PLScoef)[seq(1:3) + 3*(i-1)] <- 
            paste0(c('coef', 'sd', 't'), '_', 'g', i)
    
    # Asset pricing model, CAPM, CH3 or FF5? 
    if (nrow(PLScoef) == 3) {
            
            model_type <- 'CH3'
            ff_term <- c("mkt_rf", "SMB", "VMG")
            model_formula <- as.formula(Dret_rf ~ mkt_rf + SMB + VMG)
            
    } else if (nrow(PLScoef) == 4) {
            
            model_type <- 'CH4'
            ff_term <- c("mkt_rf", "SMB", "VMG", "RMW")
            model_formula <- as.formula(Dret_rf ~ mkt_rf + SMB + VMG + RMW)
            
    } else if (nrow(PLScoef) == 5) {
            
            model_type <- 'FF5'
            ff_term <- c("mkt_rf", "SMB", "Five_HML", "RMW", "CMA")
            model_formula <- as.formula(Dret_rf ~ mkt_rf + SMB + HML + RMW + CMA)
            
    } else if (nrow(PLScoef) == 1) {
            
            model_type <- 'CAPM'
            ff_term <- c("mkt_rf")
            model_formula <- as.formula(Dret_rf ~ mkt_rf)
            
    }
    
    PLScoef %<>% add_column("term" = ff_term, .before = 1)
    
    # reset the working directory
    setwd(file.path('~/OneDrive/Data.backup/QEAData', 
                    model_type, year(Accprd), Accprd
                    )
          )
    # import data of estimate window
    stkest <- mod_read("stkest") %>% split(.$Stkcd)
    # import data of event window
    stkeve <- mod_read("stkeve") %>% split(.$Stkcd)
    # the sample size
    if (is.numeric(subsam)) { # take a subset sample
        
            set.seed(subsam)
            stk_sam <- sample(names(stkeve), size = subsam)
            stkest %<>% `[`(stk_sam)
            stkeve %<>% `[`(stk_sam)
            
    } else if (`==`(subsam, FALSE)) print("Analysis the whole sample.")
    # the number of stocks in our sample
    N <- length(stkeve)
    # the time series period of event window
    TS <- nrow(stkeve[[N]])
    # the time line of event window
    timeline <- seq(-(TS - 1L) / 2, (TS - 1L) / 2)
    
    # classo (zhan gao) -------------------------------------------------------
    # library(classo)
    # library(Rmosek)
    
    # K_num <- 3L  # the number of groups we want
    # winlen <- nrow(stkest[[N]])  # the length of estimate window
    # Ret <- bind_rows(stkest) %>% pull(Dret_rf) %>% as.matrix()
    # CH3 <- bind_rows(stkest) %>% select(mkt_rf, SMB, VMG) %>% as.matrix()
    # lambda <- as.numeric( 0.5 * var(Ret) / (winlen^(1/3)) )
    
    # pls_out <- PLS.cvxr(N, winlen, Ret, CH3, K = K_num, lambda = lambda)
    
    
    # Part III, visual the group external relationship ------------------------
    
    # link the PLS cluster result with stocks feature and 
    # accounting indicators from quarterly earnings report 
    
    # rename the group identity using in plot (legend guide)
    PLSclus$g_PLS %<>% factor(levels = 1:grp_num, labels = grp_name) 
    # 1 = group one, 2 = group two, 3 = group three...
    
    # history 
    left_join(PLSclus, AF_Co, by = "Stkcd") %>% 
            ggplot(aes(x = g_PLS, y = year(Listdt), group = g_PLS)) +
                geom_violin(scale = "area") +
                    labs(y = "listed year", 
                         title = "The distribution of listed year of grouped stocks"
                         ) +
                    theme_bw() + 
                    theme(axis.title.x = element_blank())
        
    mod_figure(file_name = "Density-ghistory", fig_ratio = 2L, scale = 1)
    
    # industry 
    left_join(PLSclus, AF_Co, by = "Stkcd") %>% 
            mutate("Indus" = str_sub(Indus, 1L, 1L)) %>% 
            count(Indus, g_PLS) %>% 
                    ggplot(aes(x = Indus, y = n, group = g_PLS, fill = g_PLS)) +
                        geom_col(position = "stack") +
                            labs(x = "Industry", y = "Count",
                                 title = "The count of the number of stocks in a industry"
                                 ) +
                            scale_color_brewer(palette = "Set1") + 
                            coord_flip() + 
                            theme_economist() + 
                            theme(legend.title = element_blank(),
                                  axis.title.y = element_text(margin = margin(r = 10))
                                  ) 
    
    mod_figure("histogram-gIndustry", fig_ratio = 2L, scale = 1.2)
    
    
    # the information from ReptInfo ====
    # EPS, quarterly 
    f_acc_eps <- left_join(PLSclus, filter(ReptInfo, Accper == Accprd), by = "Stkcd") %>% 
            ggplot(aes(x = g_PLS, y = Erana, fill = g_PLS)) +
                geom_violin() + ylim(-1, 3) +
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
            mutate("Annowk" = factor(Annowk, 
                                     levels = c("0","1","2", "3", "4", "5", "6"), 
                                     labels = c("Sun","Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
                                     )
                   ) %>%
            ggplot(aes(x = g_PLS, y = n, group = Annowk)) +
                geom_col(aes(fill = Annowk),
                         position = "dodge"
                         ) +
                    labs(y = "Count", 
                         title = "The weekday of quarterly earning reports announcemented",
                         fill = "Weekday"
                         ) + 
                    scale_fill_brewer(palette = "Greys") +
                    theme_dark() +
                    theme(axis.title.x = element_blank(),
                          axis.title.y = element_text(margin = margin(r = 10))
                          ) 
    
    mod_figure("histogram-gWeekday", fig_ratio = 2L, scale = 1)
    
    
    # the information from AF_Cfeature
    # just need to select annual data of this year
    g_accfea <- filter(AF_Cfeature, year(Accper) == year(Accprd)) %>% 
            left_join(PLSclus, ., by = "Stkcd") 
    # size, annual
    f_acc_size <- ggplot(g_accfea, aes(x = g_PLS, y = log(CompanySize))) +
            geom_violin(aes(fill = g_PLS)) +
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
                         title = "The relationship of the company size (log) and company opacity"
                         ) +
                    facet_grid( ~ g_PLS) +
                    theme_bw() +
                    scale_color_brewer(palette = "Set1") +
                    theme(axis.title.x = element_blank(),
                          legend.position = "none")
    
    mod_figure("histogram-gAttention", fig_ratio = 2L, scale = 1)
    
    # the internal distrubution of different opacity levels firms in each group
    count(g_accfea, g_PLS, CompanyOpacity) %>% na.omit() %>%
            ggplot(aes(x = g_PLS, y = n)) +
                geom_col(aes(fill = CompanyOpacity), position = "dodge") +
                    labs(y = "Count", fill = "Company Opacity",
                         title = "Companies that with different levels of opacity within each group"
                         ) +
                    scale_fill_brewer(palette = "Greys") +
                    theme_dark() +
                    theme(axis.title.x = element_blank(),
                          axis.title.y = element_text(margin = margin(r = 10)),
                          legend.position = "top"
                          )
    
    mod_figure("histogram-gOpacity", fig_ratio = 2L, scale = 1)
    
    g_accfea %<>% gather(AttentionType, Attention, AnaAttention, ReportAttention) %>% 
            arrange(Stkcd)
    
    g_accfea$AttentionType %<>% factor(levels = c("AnaAttention", "ReportAttention"),
                                       labels = c("Analysis team", "Disclosure report")
                                       )
    
    title_char <- paste0("The relationship of the number of research reports or ", 
        "analysis teams paying attention to a stock ",
        "\nand the company opacity category disclosured by Chinese stock exchange")
    
    group_by(g_accfea, CompanyOpacity, AttentionType) %>% 
        summarise("avgAttention" = mean(Attention, na.rm = TRUE),
                  .groups = "drop"
                  )%>% 
            na.omit() %>%  # remove NA in opacity category
                ggplot(aes(x = AttentionType, y = avgAttention, group = CompanyOpacity)) +
                    geom_col(aes(fill = CompanyOpacity),
                             position = "dodge"
                             ) +
                    labs(title = title_char,
                         x = "The category of company opacity", 
                         y = "Count (average)"
                         ) +
                    scale_fill_brewer(palette = "Greys") +
                    theme_dark() +
                    theme(legend.title = element_blank(),
                          legend.position = "top",
                          axis.title.y = element_text(margin =  margin(r = "10")),
                          axis.title.x = element_text(margin =  margin(t = "10")),
                          plot.title = element_text(size = 12),
                          )
    
    mod_figure("histogram-Opacity", fig_ratio = 2L, scale = 1)
    
    
    
    # Part IV, Calculate AR and CAR within event window -----------------------
    
    # the correlation coefficient between explanation variables ====
    stkest_ff <- map_dfr(stkest, select, c("Stkcd", "Dret_rf", all_of(ff_term))) %>% 
            group_nest(Stkcd) %>% 
            mutate("cor_var" = map(data, ~ cor( select(.x, - c("Dret_rf")) ))) 
    
    # Obtain the OLS estimate parameters =====
    # function `tidy` return estimate, std.error, statistic, p.value 
    stkest_ff %<>% mutate("lm" = map(data, ~ tidy(lm(formula = model_formula, data = .x))))
    OLScoef <- select(stkest_ff, Stkcd, lm) %>% unnest(cols = "lm")
    # save the OLS estimate, compared with MATLAB's
    mod_write(OLScoef, file_name = "OLScoef")
    
    # gather the data in panel form to plot the distribution of estimators
    QEA_ggOLS <- gather(OLScoef, key = Statistics, value = Parameter, estimate:p.value) %>% 
            # term obtain the explanation variables and intercept in our model
            arrange(Stkcd, term) %>% 
            # add the cluster result of PLS
            full_join(PLSclus, ., by = "Stkcd")
    
    # the distribution of values of intercept
    filter(QEA_ggOLS, term == "(Intercept)" & Statistics == "estimate") %>% 
        ggplot(aes(x = Parameter, fill = g_PLS)) +
            geom_area(stat = "bin", bins = 30) +
            labs(x = "Estimated intercept",
                 title = paste("The distribution of estimate intercepts",
                               model_type, Accprd, sep = ", "
                               )
                 ) +
            theme_economist() +
            theme(legend.title = element_blank(),
                  axis.title.y = element_blank(),
                  axis.title.x = element_text(margin = margin(t = 7))
                  )
    
    mod_figure("Intercept", fig_ratio = 2L, scale = 1)
    
    # the distribution of coefficients of explanation variables 
    f_coef <- filter(QEA_ggOLS, term != "(Intercept)" & Statistics == "estimate") %>% 
        ggplot(aes(x = Parameter, fill = g_PLS)) +
            geom_area(stat = "bin", bins = 60) + 
            scale_fill_brewer(palette = "Set1") +
            facet_grid(term ~ .) +
            labs(y = "Count", 
                 title = paste("The distribution of factors estimators",
                               model_type, Accprd, sep = ", "
                               )
                 ) +
            theme_bw() +
            theme(axis.title.x = element_blank(),
                  axis.title.y = element_text(margin = margin(r = 10)),
                  plot.title = element_text(size = 12),
                  legend.position = "none"
                  )
    
    
    # calculate AR ====
    OLScoef %<>% split(.$Stkcd) 
    
    if (any(names(OLScoef) == names(stkeve))) {
      
        QEA_AR <- map2(stkeve, OLScoef, function(x, y) {
            
            Dret_pred <- `%*%`(as.matrix(select(x, all_of(ff_term))), 
                               diag(subset(y, term %in% ff_term, select = "estimate", drop = TRUE))
                               ) %>% 
                    rowSums() %>% 
                    `+`(subset(y, term == "(Intercept)", select = "estimate", drop = TRUE))
            
            AbRet <- tibble("Stkcd" = pull(x, Stkcd),
                            "TradingDate" = pull(x, TradingDate), 
                            "AbRet" = pull(x, Dret_rf) - Dret_pred
                            )
            } 
        )
        
    } else print("Please correct the order of the stocks in the file `OLScoef` and `stkeve`.")
    
    # we reshape the value of abnormal returns in panel form with group identity
    mutate(PLSclus, g_PLS = factor(g_PLS, 
                                   levels = grp_name, 
                                   labels = as.character(1:grp_num)
                                   )
           ) %>% 
            right_join(bind_rows(QEA_AR), by = "Stkcd") %>% 
            # export as csv file
            mod_write("gAR")
    
    # the second, layout in columns (tau)  and use it to calculate the mean of AR_tau
    QEA_gAR <- map(QEA_AR, "AbRet") %>% 
            bind_cols() %>% 
            t() %>% 
            as.data.frame() %>% 
            set_names(timeline) %>% 
            rownames_to_column("Stkcd") %>% 
            # add the group information
            inner_join(PLSclus, ., by = "Stkcd")
    
    # the unclassified CAR (mean of whole market)
    QEA_CAR <- select(QEA_gAR, -c("Stkcd", "g_PLS")) %>% 
            colMeans() %>% 
            cumsum()
    
    # calculate CAR ====
    # Note that the transpose process `t` is very important,
    # Because of the matrix is stored by columns in R
    QEA_gCAR <- select(QEA_gAR, -c(Stkcd, g_PLS)) %>% as.matrix() %>% 
            apply(1, cumsum) %>%  # cumulative sum day by day
            t() %>% 
            as.data.frame() %>%
            cbind(select(QEA_gAR, c(Stkcd, g_PLS)), .)
    
    
    # plot the path of CAR  =====
    QEA_ggCAR <- gather(QEA_gCAR, Timeline, CAR, as.character(timeline))
    # order timeline as the squence of -40:40
    QEA_ggCAR$Timeline %<>% factor(levels = as.character(timeline), ordered = TRUE)
    
    QEA_ggCAR %<>% group_by(g_PLS, Timeline) %>% 
            # using the group and mean function calculate the average-CAR among stocks
            summarise("avgCAR" = mean(CAR), .groups = "drop") %>% 
            spread(g_PLS, avgCAR) %>%
            # add the unclassified CAR (mean of whole market)
            add_column("Unclassified" = unname(QEA_CAR))
    
    QEA_ggCAR %<>% gather(key = g_PLS, value = CAR, -Timeline)
    
    # rename the group identity with stocks number ====
    # the number of stocks in each group
    grp_car_name <- count(PLSclus, g_PLS) %>% 
            # add the number of stocks with group identity
            unite(g_PLS, n, col = "grp_car_name", sep = " / ") %>% 
            pull("grp_car_name")
    QEA_ggCAR$g_PLS %<>% factor(levels = c(grp_name, "Unclassified"),
                                labels = c(grp_car_name, "Unclassified")
                                )
    
    # plot title 
    title_char <- paste("The path of grouped cumulative abnormal returns", 
            "\nfollowing the stocks's quarterly earnings reports at accounting period", 
            glue("{(Accprd + days(1)) %m+% months(-3)} ~ {Accprd} were announced"),
            sep = " "
            )
    
    # # the index of the rect in plot of CAR
    # rect_index <- tibble::tribble(
    #     ~tier, ~xmin, ~xmax, ~ymin, ~ymax,
    #     1, -10,  0,  0.0025,  0.0175,
    #     1,   0,  5,   -0.01, -0.0025,
    #     2, +10, 20,   -0.02,    0.02,
    #     3, +25, 30,  0.0070,  0.0130,
    #     3, +25, 30, -0.0275,   -0.02
    # )
    
    # CAR
    # make the timeline as continues number for path plot (factor is category data)
    f_car <- ggplot(QEA_ggCAR, aes(x = as.integer(as.character(Timeline)), 
                                   y = CAR, 
                                   colour = g_PLS
                                   )
                    ) + 
        geom_line() + 
        geom_point(aes(shape = g_PLS), size = 2.5) + 
            scale_color_manual(values = c(brewer.pal(grp_num, "Set1"), "#999999")) +
            scale_x_continuous(breaks = seq(-30, 30, by = 5)) + 
            labs(title = title_char, x = "Time line", y = 'Cumulative Abnormal Return (CAR)') + 
            geom_ref_line(h = 0, colour = "#999999") +
            theme_bw() +
            theme(plot.title = element_text(size = 14),
                  axis.title.y = element_text(face = c("italic"), margin = margin(r = 10)),
                  legend.position = "bottom", 
                  legend.title = element_blank(),
                  legend.text = element_text(size = 14, face = "bold")) # +
        # geom_rect(data = rect_index, inherit.aes = FALSE,
        #     aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = factor(tier)),
        #     alpha = 0.2, show.legend = FALSE
        #           )
        
    # calculate the standard deviation of daily returns of stocks for plot at last
    QEA_Dret_sd <- lapply(stkeve, add_column, 
                          "timeline" = factor(timeline, ordered = TRUE)
                          ) %>% 
            bind_rows() %>% inner_join(PLSclus, ., by = "Stkcd") %>% 
            group_by(g_PLS, timeline) %>% 
            summarise("Dretmrf_sd" = sd(Dret_rf), .groups = "drop")
    
    QEA_Dret_sd$timeline %<>% as.character() %>% as.integer()
    # standard deviation of stock's real daily return
    f_Dret_sd <- ggplot(QEA_Dret_sd, aes(x = as.integer(as.character(timeline)), 
                                         y = Dretmrf_sd, 
                                         colour = g_PLS
                                         )
                        ) +
            geom_smooth() + 
            geom_point(aes(shape = g_PLS), size = 2) +
            scale_color_brewer(palette = "Set1") +
            scale_x_continuous(breaks = seq(-30, 30, by = 5)) + 
            labs(x = "Time line", 
                 title = 'The standard deviation of stock\'s real daily returns'
                 ) + 
            theme_bw() +
            theme(plot.title = element_text(size = 12),
                  axis.ticks = element_blank(),
                  axis.text = element_blank(),
                  axis.title = element_blank(),
                  legend.position = "none",
                  )
    
    # Export the comprehensive plot
    multi_panel_figure(width = 320, height = 300, columns = 4, rows = 5) %>%
            fill_panel(f_car, column = 1:4, row = 1:2) %>%
            fill_panel(f_coef, column = 1:2, row = 3:5) %>% 
            fill_panel(f_Dret_sd, column = 3:4, row = 3) %>%
            fill_panel(f_acc_size, column = 3, row = 4:5) %>%
            fill_panel(f_acc_eps, column = 4, row = 4:5) %>%
                    save_multi_panel_figure(glue("{Accprd}_Figure_histogram-gSizeEarn.pdf"))

}
