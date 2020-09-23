# This script is mainly written for calculate the abnormal returns 
# and visualizing some key features about firms attribute and accounting indicator 

library(tidyverse)
library(magrittr)
library(lubridate)

library(Formula)
library(modelr)
library(glue)
library(broom)

library(multipanelfigure)
library(RColorBrewer)
# display.brewer.all()
library(ggthemes)

# Part I, import data and assign parameter -------------------------------

setwd('~/OneDrive/Data.backup/QEAData')
# Import the listing date, industry, and paying attention resource of stocks
load(file = "./FirmAttr.RData")
# Import the status data of quarterly financial report 
load(file = "./ReportInfo.RData"); rm(PreRept)

# assign basic parameters about quarter and sample attributes ====
# quarter sequence
Accprd_seq <- months(seq(from = 0, by = 3, length = 1)) %>% 
        mapply('%m+%', ymd('2017-09-30'), .) %>% 
        base::as.Date(origin = '1970-01-01') %>% 
        # split by year
        split(., f = year(.))

# the type of stocks that weather had publish pre-report
Pretype <- 6L  
# the market types that the stocks in our sample
Markettype <- 21L
# the model to measure the normal return
model_type <- "CH4"
# whether assign a character to value_base, EPS, CFPS,
# if not, represent that we will regress traditional Fama-French multi-factor model
value_base <- "EPS"
# if we just want to analysis a sub-sample?
subsam <- FALSE
## if the value of `subsam` is a number, for example, subsam <- 300L
## the analysis results of below script will just based on a sub-sample


# Asset pricing model, CAPM, CH3 or FF5? 
if (model_type == "CAPM") {
    
        ff_term <- c("mkt_rf")
        model_formula <- Formula(Dret_rf ~ mkt_rf)
        
} else if (model_type == 'FF3') {
            
        ff_term <- c("mkt_rf", "SMB", "HML")
        model_formula <- Formula(Dret_rf ~ mkt_rf + SMB + HML)
        
} else if (model_type == "FF4") { 
            
        ff_term <- c("mkt_rf", "SMB", "HML", "WML")
        model_formula <- Formula(Dret_rf ~ mkt_rf + SMB + HML + WML)
        
} else if (model_type == "FF5") {
        
        ff_term <- c("mkt_rf", "SMB", "HML", "RMW", "CMA")
        model_formula <- Formula(Dret_rf ~ mkt_rf + SMB + HML + RMW + CMA)
        
} else if (model_type == "CH3") {
        
        if (value_base %in% c("EPS", "CFPS")) {    
    
                ff_term <- c("mkt_rf", "SMB", "VMG")
                model_formula <- Formula(Dret_rf ~ mkt_rf + SMB + VMG)
        
        } else stop("Please input the class information of value.")
        
} else if (model_type == "CH4") {
        
        if (value_base %in% c("EPS", "CFPS")) {
            
                ff_term <- c("mkt_rf", "SMB", "VMG", "RMW")
                model_formula <- Formula(Dret_rf ~ mkt_rf + SMB + VMG + RMW)
                
        } else stop("Please input the class information of value.")

}           

# Part II, calculate the grouped abnormal returns (AR) and cumulative AR ----

# create a complicate list to save quarterly classification results from PLS
PLS_grp <- vector(mode = "list", length = length(Accprd_seq)) %>% 
        set_names(names(Accprd_seq)) %>%  # year
        map(~ vector(mode = "list", length = 4L) %>%  
                set_names(c("1st", "2nd", "3rd", "4th"))  # quarter 
            ) %>%  
        map(map, ~ vector(mode = "list", length = 2L) %>% 
                set_names(c("cluster", "estimator"))  # quarter 
            )

for (y in seq_along(Accprd_seq)) {  # loop in year
    
    for (q in seq_along(Accprd_seq[[y]])) {  # loop in quarter
    
        Accprd <- Accprd_seq[[y]][q] %>% print()
       
        # Read the result from MATLAB, PLS (su, 2016) --------------------
        
        # Group information ====
        PLSclus <- dir(path = file.path('~/OneDrive/Data.backup/QEAData',
                                        'Matlab_PLS', 
                                        year(Accprd)
                                        ),
                       pattern = paste("group", Accprd, Pretype, Markettype, 
                                       model_type, 
                                       sep = '_'
                                       ),
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
        
        # save the quarterly cluster result (PLS)
        PLS_grp[[y]][[q]]$cluster <- PLSclus
        
        # PLS coefficients ====
        PLScoef <- dir(path = file.path('~/OneDrive/Data.backup/QEAData',
                                        'Matlab_PLS', 
                                        year(Accprd)
                                        ),
                       pattern = paste("PLS", Accprd, Pretype, Markettype, 
                                       model_type, 
                                       sep = '_'
                                       ),
                       recursive = TRUE,
                       full.names = TRUE
                       ) %>% 
                read_csv(col_types = cols(.default = col_double()))
        # rename the columns name
        for (i in 1:grp_num) {
                names(PLScoef)[seq(1:3) + 3*(i-1)] <- 
                        paste0(c('coef', 'sd', 't'), '_', 'g', i)
        }
        
        # add the factor identity column
        PLScoef %<>% add_column("term" = ff_term, .before = 1)
        # save the PLS estimators
        PLS_grp[[y]][[q]]$estimator <- PLScoef
        
        # import the quarterly window trading data ------------------------
        
        # reset the working directory
        setwd(file.path('~/OneDrive/Data.backup/QEAData', 
                        model_type, year(Accprd), Accprd
                        )
              )
        # import data of estimate window
        stkest <- dir(pattern = paste(Accprd, Pretype, Markettype, model_type, 
                                      "stkest", 
                                      sep = '_'
                                      ),
                      full.names = TRUE
                      ) %>%
                read_csv(na = '',
                         col_types = cols(
                                 Stkcd = col_character(),
                                 Markettype = col_factor(levels = c(1,4,16)),
                                 Indus = col_factor(),  # industry category
                                 Annowk = col_factor()  # the day of week
                                 )
                         ) %>% 
                split(.$Stkcd)
        
        # import data of event window
        stkeve <- dir(pattern = paste(Accprd, Pretype, Markettype, model_type, 
                                      "stkeve", 
                                      sep = '_'
                                      ),
                      full.names = TRUE
                      ) %>%
                read_csv(na = '',
                         col_types = cols(
                                 Stkcd = col_character(),
                                 Markettype = col_factor(levels = c(1,4,16)),
                                 Indus = col_factor(),  # industry category
                                 Annowk = col_factor()  # the day of week
                                 )
                         ) %>% 
                split(.$Stkcd)
        
        # the sample size
        if (is.numeric(subsam)) { # take a subset sample
            
                set.seed(subsam)
                stk_sam <- sample(names(stkeve), size = subsam)
                stkest %<>% `[`(stk_sam)
                stkeve %<>% `[`(stk_sam)
                
        } else if (`==`(subsam, FALSE)) print("Whole sample will be analysis.")
        # the number of stocks in our sample
        N <- length(stkeve)
        # the time series period of event window
        if(`==`(length(unique(map_int(stkeve, nrow))), 1)) {
    
                TS <- unique(map_int(stkeve, nrow))
    
        } else stop("The time series number of window trading data are not same.")
        # the time line of event window
        timeline <- seq(-(TS - 1L) / 2, (TS - 1L) / 2)
        
        # classo (zhan gao) -----------------------------------------------
        # library(classo)
        # library(Rmosek)
        
        # K_num <- 3L  # the number of groups we want
        # winlen <- nrow(stkest[[N]])  # the length of estimate window
        # Ret <- bind_rows(stkest) %>% pull(Dret_rf) %>% as.matrix()
        # CH3 <- bind_rows(stkest) %>% select(mkt_rf, SMB, VMG) %>% as.matrix()
        # lambda <- as.numeric( 0.5 * var(Ret) / (winlen^(1/3)) )
        
        # pls_out <- PLS.cvxr(N, winlen, Ret, CH3, K = K_num, lambda = lambda)
        
        
        # visual the group external relationship --------------------------
        figure_term <- paste(Accprd, Pretype, Markettype, model_type, sep = '_')
        
        # link the PLS cluster result with stocks feature and 
        # accounting indicators from quarterly earnings report 
        
        # rename the group identity using in plot (legend guide)
        if (grp_num == 2) {
                grp_name <- c("group one", "group two")
        } else if (grp_num == 3)  {
                grp_name <- c("group one", "group two", "group three")
        } else if (grp_num == 4) {
                grp_name <- c("group one", "group two", "group three", "group four")
        } else stop("The group number is not included in script!")
        
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
            
        ggsave(filename = paste(figure_term, "history.pdf", sep = '_'), 
               width = 8, height = 6, 
               scale = 1
               )
        
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
        
        ggsave(filename = paste(figure_term, "Industry.pdf", sep = '_'),
               width = 8, height = 6, 
               scale = 1.2
               )
        
        
        # the information from ReptInfo 
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
        
        ggsave(filename = paste(figure_term, "Weekday.pdf", sep = '_'),
               width = 8, height = 6, 
               scale = 1
               )
        
        
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
        
        ggsave(filename = paste(figure_term, "Attention.pdf", sep = '_'),
               width = 8, height = 6, 
               scale = 1
               )
        
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
        
        ggsave(filename = paste(figure_term, "Opacity.pdf", sep = '_'), 
               width = 8, height = 6,
               scale = 1
               )
        
        g_accfea %<>% gather(c(AnaAttention, ReportAttention),
                             key = 'AttentionType', value = 'Attention'
                             ) %>% 
                arrange(Stkcd)
        
        g_accfea$AttentionType %<>% factor(levels = c("AnaAttention", "ReportAttention"),
                                           labels = c("Analysis team", "Disclosure report")
                                           )
        
        title_char <- paste("The relationship of the number of research reports or", 
                            "analysis teams paying attention to a stock",
                            "\nand the company opacity category disclosured by Chinese stock exchange",
                            sep = " "
                            )
        
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
        
        ggsave(filename = paste(figure_term, "Opacity-cor.pdf", sep = '_'),
               width = 8, height = 6, 
               scale = 1
               )
        
        
        # Calculate AR and CAR within event window -----------------------
        
        # the correlation coefficient between explanation variables ====
        stkest_ff <- tibble('Stkcd' = names(stkest),
                            'data' = map(stkest, select, c("Dret_rf", all_of(ff_term)))
                            ) %>% 
                mutate("cor_var" = map(data, ~ cor( select(.x, - c("Dret_rf")) ))) 

        # Obtain the OLS estimate parameters =====
        # function `tidy` return estimate, std.error, statistic, p.value 
        stkest_ff %<>% mutate("lm" = map(data, ~ lm(data = .x,
                                                    formula = formula(model_formula, lhs = 1) 
                                                    ) %>% 
                                                 tidy()
                                         )
                              )
        OLScoef <- select(stkest_ff, Stkcd, lm) %>% unnest(cols = "lm")
        # save the OLS estimate, compared with MATLAB's
        write.csv(OLScoef, 
                  file = paste(Accprd, Pretype, Markettype, model_type, 
                               'OLScoef.csv', 
                               sep = '_'
                               ),
                  quote = F, row.names = F
                  )
        
        # gather the data in panel form to plot the distribution of estimators
        QEA_ggOLS <- gather(OLScoef, estimate:p.value, 
                            key = 'Statistics', value = 'Parameter'
                            ) %>% 
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
        
        ggsave(filename = paste(figure_term, "Intercept.pdf", sep = '_'),
               width = 8, height = 6, 
               scale = 1
               )
        
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
                write.csv(quote = F, row.names = F,
                          file = paste(Accprd, Pretype, Markettype, model_type, 
                                       'gAR.csv', 
                                       sep = '_'
                                       )
                          )
                
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
        QEA_ggCAR <- gather(QEA_gCAR, as.character(timeline), 
                            key = 'Timeline', value = 'CAR'
                            )
        # order timeline as the squence of -40:40
        QEA_ggCAR$Timeline %<>% factor(levels = as.character(timeline), ordered = TRUE)
        
        QEA_ggCAR %<>% group_by(g_PLS, Timeline) %>% 
                # using the group and mean function calculate the average-CAR among stocks
                summarise("avgCAR" = mean(CAR), .groups = "drop") %>% 
                spread(g_PLS, avgCAR) %>%
                # add the unclassified CAR (mean of whole market)
                add_column("Unclassified" = unname(QEA_CAR))
        
        QEA_ggCAR %<>% gather(key = 'g_PLS', value = 'CAR', -Timeline)
        
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
    
}

# take a look at the PLS estimators of grouped stocks
map_df(PLS_grp, ~ map_df(.x, "estimator", .id = "quarter"), .id = "year")


# Part III, focus on the stocks which classification results are continually ----
# one stock is clustered into a same class among quarters of a year 

library(DBI)
library(RSQLite)
library(dbplyr)

# function to query accounting data in reports from SQLite database
tbl_query <- function(database = QEA_db, 
                      table_name,  # name of table in database
                      stock_cd,  # the symbols of stocks
                      report_period,  # quarterly or annually
                      accounting_period,  # specific accounting period
                      report_type = "A"  # 'A' is whole firm, 'B' is parent firm 
                      ) {

    query_data <- tbl(database, table_name) %>% 
            filter(Stkcd %in% !!stock_cd)
    
    if (report_period == "yearly") {  # for annual report, just need the characters of year
        
            query_data %<>% 
                    collect() %>% 
                    mutate("Accper" = as.Date(Accper, origin = "1970-01-01")) %>% 
                    filter(as.character(year(Accper)) %in% !!accounting_period)
        
    } else if (report_period == "quarterly") {
        
            # for quarterly report, character of year or date both are allowed 
            
            if (nchar(accounting_period) == 4) {  # character of particular year 
                
                    # manually convert the year character to the date of a end accounting period
                    accounting_period %<>% stringr::str_c(c("0331", "0630", "0930", "1231"))
                
            } 
        
            if (all(nchar(accounting_period) %in% c(8, 10))) {  # date in form of '%Y%m%d' or `%Y-%m-%d' 
                
                    accounting_period %<>% lubridate::ymd() %>% as.integer()
        
                    query_data %<>% 
                            filter(Accper %in% !!accounting_period & Typrep == report_type) %>% 
                            collect() %>% 
                            mutate("Accper" = as.Date(Accper, origin = "1970-01-01"))
                    
            } else stop("Please comfirm the pattern of date record is correct.")
        
    }
    
    return(mutate(query_data, "Accper" = as_factor(as.character(Accper))))
}

# function to compose the functions of select, unnest, and group_nest 
select_nest <- function(x, select_col = "g_PLS", value_col, key_name) {
    
    select(x, all_of(select_col), all_of(value_col)) %>% 
            unnest(cols = value_col) %>% 
            group_nest(Accper, g_PLS, .key = key_name)
    
}


# link to database
QEA_db <- dbConnect(RSQLite::SQLite(), "~/OneDrive/Data.backup/QEAData/QEA_db.sqlite")

# taking the intersection of quarterly classification results,
# the quarterly PLS classification results will be reshaped to yearly clustering
PLS_grp_inter <- vector(mode = "list", length = length(Accprd_seq)) %>% 
        set_names(names(Accprd_seq))

for (y in seq_along(PLS_grp)) {
    
    PLS_grp_level <- map_df(PLS_grp[[y]], ~ as.double(levels(pull(.x$cluster, g_PLS))))
    
    if (sum(is.na.data.frame(PLS_grp_level)) != 0) {
            stop(glue("The quarterly group numbers aren't same with within year {names(PLS_grp[y])}"))
    }
    
    if (  # assure the levels of classification at each quarters are same
        
        apply(PLS_grp_level, 2, sum) %>% 
                `names<-`(NULL) %>% 
                all.equal(rep(sum(1:grp_num), times = 4))
    
        ) {  # take the intersect of stocks belong to same class among quarters within one year
        
        PLS_grp_inter[[y]] <- map(PLS_grp[[y]], "cluster") %>% 
                                reduce(inner_join, by = c("Stkcd", "g_PLS"))
        
    } else stop(glue("The quarterly level of groups aren't same with within year {names(PLS_grp[y])}"))

}

# take the complement of stocks existed in `PLS_grp_inter` with sample,
# the stocks in this set are the PLS classification of them
# is unstable among quarters within a accounting annual 
PLS_grp_unstable <- lapply(PLS_grp, map, "cluster") %>% 
        map2(PLS_grp_inter, 
             ~ map(.x, .f = anti_join, y = .y, by = c("Stkcd", "g_PLS"))
             )

# query accounting information in quarterly reports 
# accounting to symbols of stocks and accounting date
PLS_grp_unstable %<>% 
        map_dfr(~ bind_rows(.x, .id = "quarter"), .id = "year") %>% 
        group_nest(year, quarter, g_PLS, .key = "Stkcd") %>% 
        mutate("Stkcd" = map(Stkcd, pull, Stkcd),
               "date_prefix" = year,
               "date_suffix" = factor(quarter, 
                                      levels = c("1st", "2nd", "3rd", "4th"),
                                      labels = c("0331", "0630", "0930", "1231")
                                      )
               ) %>% 
        unite(date_prefix, date_suffix, col = "Accper", sep = "") %>% 
        mutate("accounting_quarter" = map2(Stkcd, Accper,
                                           ~ tbl_query(table_name = "quarter",
                                                       report_period = "quarterly",
                                                       stock_cd = .x,
                                                       accounting_period = .y
                                                       )
                                          )
               ) %>% 
        select(-Accper)

# query accounting information in yearly and quarterly reports 
# accounting to symbols of stocks and particular year 
# for stocks which PLS classification are coherent
PLS_grp_inter %<>% 
        map(group_nest, g_PLS, .key = "Stkcd") %>% 
        imap(function(y, z) {
                mutate(y, 
                       "Stkcd" = map(Stkcd, pull, "Stkcd"),
                       "accounting_year" = map(Stkcd, 
                                               ~ tbl_query(table_name = "annual", 
                                                           report_period = "yearly", 
                                                           stock_cd = .x,
                                                           accounting_period = z
                                                           )
                                               ),
                       "accounting_quarter" = map(Stkcd, 
                                                  ~ tbl_query(table_name = "quarter",
                                                              report_period = "quarterly",
                                                              stock_cd = .x,
                                                              accounting_period = z
                                                              )
                                                  )
                       )
                }
        ) %>% 
        bind_rows(.id = "year")

dbDisconnect(QEA_db)

# focus on the accounting indicators in quarterly report -------------------
# price-to-earning (PE), F100101B, and earning per share (EPS), F020108
# the theory foundation and basis idea of analysis them 
# is the price measure identical equation, P = PE \times EPS 

PLS_grp_unstable %<>% 
        mutate("accounting_quarter_PE" = map(.x = accounting_quarter, 
                                             ~ select(.x, "Accper", "Stkcd", "F100101B") %>% 
                                                 rename('PE' = F100101B)
                                             )
               )

PLS_grp_inter %<>% 
        mutate("accounting_quarter_PE" = map(.x = accounting_quarter, 
                                             ~ select(.x, "Accper", "Stkcd", "F100101B") %>% 
                                                 rename('PE' = F100101B)
                                             )
               )

# compose and compare accounting indicator between `inter` with `unstable`
PLS_grp_PE <- full_join(by = c('g_PLS', 'Accper'),
                        select_nest(PLS_grp_inter, 
                                    value_col = "accounting_quarter_PE", 
                                    key_name = "inter_PE"
                                    ),
                        select_nest(PLS_grp_unstable, 
                                    value_col = "accounting_quarter_PE", 
                                    key_name = "unstable_PE"
                                    )
                        ) %>% 
        mutate('g_PLS' = fct_relabel(g_PLS, ~ str_c('g_', .)))

# focus on PE
pivot_longer(PLS_grp_PE, 
             cols = c('inter_PE', 'unstable_PE'), 
             names_to = 'class', values_to = 'value'
             ) %>% 
        unnest(cols = value) %>% 
        filter(PE <= quantile(PE, 0.875, na.rm = TRUE),
               PE >= quantile(PE, 0.125, na.rm = TRUE)
               ) %>% 
        # take a look at the distribution of PE of grouped stocks
        ggplot(mapping = aes(x = class, y = PE)) + 
                    geom_violin() + 
                    facet_wrap(facets = g_PLS ~ Accper) +
                    labs(y = "Price-to-earning(PE)")

ggsave(filename = "distribution_EP_grp.pdf",
       path = file.path('~/OneDrive/Data.backup/QEAData/', model_type),
       width = 16, height = 9
       )

