# This script is mainly written for calculate the abnormal returns 
# and visualizing some key features about firms attribute and accounting indicator 
library(tidyverse)
library(magrittr)
library(lubridate)

library(Formula)
library(modelr)
library(glue)
library(broom)

library(ggthemes)
library(hrbrthemes)
library(multipanelfigure)
library(RColorBrewer)
library(ggnewscale)
library(latex2exp)
# display.brewer.all()
suppressMessages(extrafont::loadfonts())

theme_set(
    theme_ipsum() + theme(axis.title.x = element_blank())
)

# Part I, import data and assign parameter -------------------------------
setwd('~/OneDrive/Data.backup/QEAData')
# Import the listing date, industry, and paying attention resource of stocks
load(file = "./FirmAttr.RData")
# Import the status data of quarterly financial report 
load(file = "./ReportInfo.RData"); rm(PreRept)
# assign basic parameters about quarter and sample attributes
Accprd <- months(seq(from = 0, by = 3, length = 4*1)) %>% 
        mapply('%m+%', ymd('2013-03-31'), .) %>% `[`(.!=ymd("2016-06-30")) %>%
        base::as.Date(origin = '1970-01-01') %>% 
        split(f = year(.))  # split by year
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
# if the value of `subsam` is a number, for example, subsam <- 300L
# the analysis results of below script will just based on a sub-sample
subsam <- FALSE

# Part II, calculate the grouped abnormal returns (AR) and cumulative AR ----
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
                
        } else stop("Please input the class information of value factor.")
}
# create a complicate list to save quarterly classification results from PLS
PLS_grp <- vector(mode = "list", length = length(Accprd)) %>% 
        set_names(names(Accprd)) %>%  # year
        map(~ vector(mode = "list", length = 4L) %>%  
                set_names(c("1st", "2nd", "3rd", "4th"))  # quarter 
            ) %>%  
        map(map, ~ vector(mode = "list", length = 2L) %>% 
                set_names(c("cluster", "estimator"))  # saving cluster and estimator
            )

for (y in seq_along(Accprd)) {  # loop in year
    
    for (q in seq_along(Accprd[[y]])) {  # loop in quarter
    
        print(Accprd[[y]][q])
        # reset the working directory
        setwd(file.path('~/OneDrive/Data.backup/QEAData', model_type, 
                        names(Accprd)[y], Accprd[[y]][q])
              )
        # import the quarterly window trading data ------------------------
        win_stk <- full_join(by = 'Stkcd',
                # import data of estimate window
                dir(pattern = paste(Accprd[[y]][q], Pretype, Markettype, model_type, 
                                      "stkest", sep = '_'),
                    full.names = TRUE) %>%
                read_csv(col_types = cols(Stkcd = col_character(),
                                          Markettype = col_factor(levels = c(1,4,16)),
                                          Indus = col_factor(),  # industry category
                                          Annowk = col_factor()  # the day of week
                                          )
                         ) %>% 
                group_nest(Stkcd, .key = 'win_est'), 
                # import data of event window             
                dir(pattern = paste(Accprd[[y]][q], Pretype, Markettype, model_type, 
                                      "stkeve", sep = '_'),
                    full.names = TRUE) %>%
                read_csv(col_types = cols(Stkcd = col_character(),
                                          Markettype = col_factor(levels = c(1,4,16)),
                                          Indus = col_factor(),  # industry category
                                          Annowk = col_factor()  # the day of week
                                          )
                         ) %>% 
                group_nest(Stkcd, .key = 'win_eve') 
                )
        
        # reshape the sample and assign its attributes ====
        # the sample size
        if (is.numeric(subsam)) { # take a subset sample
            
                set.seed(subsam)
                stk_sam <- sample(pull(win_stk, Stkcd), size = subsam)
                win_stk %<>% filter(Stkcd %in% stk_sam)
                
        } else if (`==`(subsam, FALSE)) print("Whole sample will be analysis.")
        # the number of stocks in our sample
        N <- nrow(win_stk)
        # the time series period of event window
        if(`==`(length(unique(map_int(win_stk$win_eve, nrow))), 1)) {
    
                TS <- unique(map_int(win_stk$win_eve, nrow))
                # the time line of event window
                timeline <- seq(-(TS - 1L) / 2, (TS - 1L) / 2)
    
        } else stop("The time series number of window trading data are not same.")
        
        # Read the result from MATLAB, PLS (su, 2016) --------------------
        # Group information ====
        PLSclus <- dir(path = file.path('~/OneDrive/Data.backup/QEAData/Matlab_PLS',
                                        names(Accprd)[y]),
                       pattern = paste("group", Accprd[[y]][q], Pretype, Markettype, 
                                       model_type, sep = '_'),
                       recursive = TRUE, full.names = TRUE
                       ) %>% 
                read_csv(col_types = cols(Stkcd = col_character(),
                                          g_PLS = col_factor())
                         )
        # the number of groups by PLS (su, 2016)
        grp_num <- levels(PLSclus$g_PLS) %>% length()
        # re-level the order of group levels, this process is important
        PLSclus$g_PLS %<>% fct_relevel(as.character(1:grp_num))
        # add PLS group identity
        win_stk %<>% left_join(PLSclus, by = "Stkcd")
        # save the quarterly cluster result (PLS)
        PLS_grp[[y]][[q]]$cluster <- PLSclus
        
        # PLS coefficients ====
        PLScoef <- dir(path = file.path('~/OneDrive/Data.backup/QEAData/Matlab_PLS',
                                        names(Accprd)[y]),
                       pattern = paste("PLS", Accprd[[y]][q], Pretype, Markettype, 
                                       model_type, sep = '_'),
                       recursive = TRUE, full.names = TRUE
                       ) %>% 
                read_csv(col_types = cols(.default = col_double()))
        # rename the columns name
        for (i in 1:grp_num)
            names(PLScoef)[seq(1:3) + 3*(i-1)] <- paste0(c('coef', 'sd', 't'), '_g', i)
        # add the factor identity column
        PLScoef %<>% add_column("term" = ff_term, .before = 1)
        # save the PLS estimators
        PLS_grp[[y]][[q]]$estimator <- PLScoef

        # link and visual PLS cluster result with stocks feature ----------
        # and accounting indicators from quarterly earnings report 
        figure_term <- paste(Accprd[[y]][q], Pretype, Markettype, model_type, sep = '_')
        # rename the group identity using in plot (legend guide)
        if (grp_num == 2) {
                grp_name <- c("group one", "group two")
        } else if (grp_num == 3)  {
                grp_name <- c("group one", "group two", "group three")
        } else if (grp_num == 4) {
                grp_name <- c("group one", "group two", "group three", "group four")
        } else stop("The group number is not included in script!")
        # relabel the group name
        # 1 = group one, 2 = group two, 3 = group three...
        PLSclus$g_PLS %<>% factor(levels = 1:grp_num, labels = grp_name) 
        # the context of caption, accounting period
        caption_char <- paste("accounting period,", Accprd[[y]][q] + days(1) - months(3),
                              '~', Accprd[[y]][q], sep = ' ')
        
        # industry 
        left_join(PLSclus, AF_Co, by = "Stkcd") %>% 
        mutate("Indus" = factor(str_sub(Indus, 1L, 1L))) %>% 
        count(Indus, g_PLS) %>% group_by(g_PLS) %>% 
        mutate('freq' = n / sum(n)) %>% 
        filter(freq >= 0.03) %>% 
            ggplot(mapping = aes(x = factor(-freq), y = freq, fill = Indus)) + 
                geom_col(position = 'dodge') +
                labs(title = "The distribuion frequency of stocks in a industry",
                     y = "frequency", x = 'Industry', fill = 'Industry', 
                     caption = caption_char) + 
                facet_wrap(~ g_PLS, ncol = grp_num, scales = 'free_x', drop = TRUE) + 
                scale_fill_brewer(palette = 'Set1') + 
                theme(axis.text.x = element_blank())
        
        ggsave(filename = paste(figure_term, "Industry.pdf", sep = '_'),
               width = 8, height = 6, scale = 1.2)
        
        # the day of week that the report was announced
        left_join(PLSclus, filter(ReptInfo, Accper == Accprd[[y]][q]), by = "Stkcd") %>%
        count(g_PLS, Annowk) %>% group_by(g_PLS) %>% 
        mutate('freq' = n / sum(n),
               'Annowk' = factor(Annowk, 
                                 levels = c("0", "1", "2", "3", "4", "5", "6"), 
                                 labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
               ) %>%
            ggplot(aes(x = factor(freq), y = freq, fill = fct_infreq(Annowk))) +
                geom_col(position = "dodge") + 
                    labs(title = "The weekday of quarterly earning reports announcemented",
                         y = "frequency", x = "Weekday", fill = "Weekday",
                         caption = caption_char) + 
                    facet_wrap(~g_PLS, ncol = grp_num, scales = 'free_x', drop = TRUE) + 
                    scale_fill_brewer(palette = "Blues") + 
                    theme(axis.text.x = element_blank())
        
        ggsave(filename = paste(figure_term, "Weekday.pdf", sep = '_'),
               width = 8, height = 6)
        
        # history
        f_history <- left_join(PLSclus, AF_Co, by = "Stkcd") %>% 
                ggplot(mapping = aes(x = g_PLS, y = year(Listdt))) +
                geom_boxplot(aes(fill = g_PLS), width = 0.1) + 
                geom_violin(alpha = 0.1) +
                    labs(y = "Stock's listed year") + 
                    scale_fill_brewer(palette = "Set1") + 
                    theme(legend.position = 'none')
        
        # the information from AF_Cfeature ====
        # just need to select annual data of this year
        g_accfea <- left_join(PLSclus, filter(AF_Cfeature, year(Accper) == year(Accprd[[y]][q])), 
                              by = "Stkcd") %>% 
                # remove stocks that Analysis Attention or Report Attention is NA
                na.omit() 
        
        # The relationship between the number of research reports or analysis teams
        # paying attention to a stock and the company opacity category
        f_opacity <- g_accfea %>% 
                gather(key = 'AttentionType', value = 'Attention',
                       AnaAttention, ReportAttention
                       ) %>% 
                mutate(AttentionType = factor(AttentionType,
                                              levels = c("AnaAttention", "ReportAttention"),
                                              labels = c("Analysis team", "Disclosure report"))
                       ) %>% 
                group_by(CompanyOpacity, AttentionType) %>% 
                summarise("avgAttention" = mean(Attention, na.rm = TRUE),
                          .groups = "drop") %>% 
                    ggplot(aes(x = CompanyOpacity, y = avgAttention, fill = CompanyOpacity)) +
                        geom_col(position = "dodge" ) +
                        labs(x = "The category of company opacity", y = "Count (average)",
                             fill = "Company Opacity") + 
                        facet_wrap(vars(AttentionType), nrow = grp_num, scales = "free_y") + 
                        scale_fill_brewer(palette = "Greys") + 
                        theme(axis.title.x = element_text(inherit.blank = FALSE))
        
        # the internal distribution of different opacity levels firms in each group
        f_group_opacity <- g_accfea %>% 
                count(g_PLS, CompanyOpacity) %>% group_by(g_PLS) %>% 
                mutate('freq' = n / sum(n)) %>% 
                    ggplot(aes(x = CompanyOpacity, y = freq)) +
                        geom_col(aes(fill = g_PLS), position = "dodge") +
                            labs(y = "frequency", x = "The category of company opacity",
                                 fill = "group (PLS)") +
                            scale_fill_brewer(palette = "Set1")
        
        # company size and company opacity
        title_char <- "The relationship between company size (logrithm) and company opacity"
        f_size_opacity <- g_accfea %>% 
                select(Stkcd, g_PLS, CompanySize, CompanyOpacity) %>% 
                ggplot(aes(x = g_PLS, y = log(CompanySize))) +
                geom_violin(aes(fill = CompanyOpacity), width = 1.0) + 
                    scale_fill_brewer(palette = "Greys") + 
                new_scale_fill() + 
                geom_boxplot(aes(fill = g_PLS), width = 0.1) + 
                    scale_fill_brewer(palette = "Set1") + 
                    labs(title = title_char, y = "The logarithm of company size") + 
                    facet_wrap(vars(CompanyOpacity), nrow = grp_num) + 
                    theme(legend.position = 'none')
        
        multi_panel_figure(width = 320, height = 320, columns = 2, rows = 3,
                           panel_label_type = "decimal") %>%
        fill_panel(f_opacity, column = 1, row = 1) %>%
        fill_panel(f_group_opacity, column = 2, row = 1) %>% 
        fill_panel(f_size_opacity, column = 1:2, row = 2:3) %>% 
                save_multi_panel_figure(paste(figure_term, "Opacity.pdf", sep = '_'))
                
        # size, annual
        f_acc_size <- g_accfea %>% 
                ggplot(mapping = aes(x = g_PLS, y = log(CompanySize))) +
                geom_boxplot(aes(fill = g_PLS), width = 0.1) + 
                geom_violin(alpha = 0.1) +
                    labs(y = "Stock's market size (taked logrithm)") +
                    scale_fill_brewer(palette = "Set1") +
                    theme(legend.position = "none")
        
        # Calculate AR and CAR within event window -----------------------
        
        # Obtain the OLS estimate parameters from estimate window =====
        win_stk %<>% mutate(
                "lm_win_est" = map(win_est, 
                                   ~ lm(data = .x, formula = formula(model_formula, lhs = 1))
                                   )
                )
                
        lm_tidy_stk <- win_stk %>% 
                transmute(Stkcd, 'lm_tidy' = map(lm_win_est, tidy)) %>% 
                unnest('lm_tidy') 
        
        # save the OLS estimate, compared with MATLAB's
        write.csv(lm_tidy_stk,
                  file = paste(Accprd[[y]][q], Pretype, Markettype, model_type, 
                               'OLScoef.csv', sep = '_'), 
                  quote = F, row.names = F)
        
        # gather the data in panel form to plot the distribution of estimators
        lm_gstk_tidy <- lm_tidy_stk %>% 
                gather(key = 'Statistics', value = 'Parameter', estimate:p.value) %>% 
                full_join(PLSclus, ., by = "Stkcd")  # add the cluster result of PLS
        
        # the distribution of values of intercept 
        filter(lm_gstk_tidy, term == "(Intercept)", Statistics == "estimate") %>% 
        ggplot(aes(x = Parameter, y = ..scaled.., fill = g_PLS)) +
            geom_density(alpha = 0.85) + 
            labs(title = "The distribution of estimate intercepts",
                 y = "Density", x = "The value of estimated intercept",
                 fill = "Classification (PLS)") +
            scale_fill_brewer(palette = 'Set1') + 
            theme_bw() + 
            theme(legend.position = c(.80,.85))
        
        ggsave(filename = paste(figure_term, "Intercept.pdf", sep = '_'),
               width = 8, height = 6)
        
        # the distribution of coefficients of explanation variables
        f_coef <- lm_gstk_tidy %>% 
                filter(term != "(Intercept)", Statistics == "estimate") %>% 
                ggplot(aes(x = Parameter, y=..scaled.., fill = g_PLS)) +
                    geom_density(alpha = 0.85) + 
                    scale_fill_brewer(palette = "Set1") +
                    facet_wrap(vars(term), nrow = length(ff_term)) +
                    labs(title = "The distribution of estimate factors",
                         y = "Density", x = "The value of estimated coefficient") +
                    theme_bw() +
                    theme(legend.position = "none",
                          axis.text.y = element_blank(),
                          axis.ticks.y = element_blank()
                          )
        
        # calculate NR, AR, and CAR 
        win_stk %<>% mutate(
                # normal return
                'NR' = map2(win_eve, lm_win_est,
                            ~ `%*%`(as.matrix(select(.x, all_of(ff_term))), 
                                    diag(subset(tidy(.y), term %in% ff_term, select = "estimate", 
                                                drop = TRUE)
                                         )
                                    ) %>% 
                            rowSums() %>% 
                            `+`(subset(tidy(.y), term == "(Intercept)", select = "estimate", 
                                       drop = TRUE)
                                )
                            ),
                # abnormal return
                'AR' = map2(win_eve, NR, ~ subset(.x, select = 'Dret_rf', drop = TRUE) %>% `-`(.y)),
                'CAR' = map(AR, cumsum)
                 )
        
        # export AR as csv file
        win_stk %>% 
        transmute(Stkcd, g_PLS,
                  'AR' = map2(win_eve, AR,
                              ~ tibble(select(.x, TradingDate),  # TradingDate
                                       'tau' = timeline, 
                                       'AR' = .y)
                              )
                  ) %>% 
        unnest(cols = 'AR') %>% 
        write.csv(quote = F, row.names = F,
                  file = paste(Accprd[[y]][q], Pretype, Markettype, model_type, 
                               'gAR.csv', sep = '_')
                  )
        
        # draw the path plot of  CAR --------------------------------------
        # the number of stocks in each group
        grp_car_name <- count(PLSclus, g_PLS) %>% 
                # add the number of stocks with group identity
                unite(g_PLS, n, col = "grp_car_name", sep = " / ") %>% 
                pull("grp_car_name")
        
        QEA_gCAR <- win_stk %>% 
                transmute(g_PLS, 'CAR' = map(CAR, ~ tibble('tau' = timeline, 'CAR' = .x))) %>% 
                unnest(cols = 'CAR') %$%  
                bind_rows(# group avg-CAR
                          group_by(., tau, g_PLS) %>% 
                          summarise('gCAR' = mean(CAR), .groups ='drop'),
                          # unclassified avg-CAR (mean of whole market)
                          group_by(., tau) %>% 
                          summarise('gCAR' = mean(CAR), .groups ='drop') %>% 
                          add_column('g_PLS' = 'Unclassified', .after = 'tau')
                          ) %>% 
                mutate('tau' = factor(tau, ordered = TRUE),  # order the time-line
                        # rename the group identity with stocks number 
                       'g_PLS' = factor(g_PLS, 
                                        levels = c(1:grp_num, "Unclassified"),
                                        labels = c(grp_car_name, "Unclassified"))
                       ) 
        
        # plot title 
        title_char <- paste0("The path of cumulative abnormal return (CAR) ", 
                             "following annocement of quarterly earnings report")
        
        # the index of the rect in plot of CAR
        rect_index <- tibble::tribble(
            ~tier, ~x_min, ~x_max, ~y_min, ~y_max,
                1,   -Inf,    -21,   -Inf,    Inf,
                2,     15,    Inf,   -Inf,    Inf,
            )
        
        # path of CAR
        f_car <- QEA_gCAR %>% 
            ggplot(aes(x = as.integer(as.character(tau)), y = gCAR, colour = g_PLS)) + 
            geom_line(aes(linetype = g_PLS)) + 
            geom_point(aes(shape = g_PLS), size = 1.5) + 
                scale_color_manual(values = c(brewer.pal(grp_num, "Set1"), "#999999")) +
                scale_x_continuous(breaks = seq(-30, 30, by = 5)) + 
                labs(title = title_char, caption = caption_char,
                     x = TeX("Timeline ($\\tau$)"), y = 'Cumulative Abnormal Return (CAR)', 
                     colour = 'Classification (PLS)', linetype = 'Classification (PLS)',
                     shape = 'Classification (PLS)'
                     ) + 
                geom_hline(yintercept = 0, linetype = "dashed", color = "grey", size = 1.5) +
            geom_rect(data = rect_index, 
                      aes(xmin = x_min, xmax = x_max,
                          ymin = y_min, ymax = y_max, 
                          # fill = factor(tier)
                          ),
                      alpha = 0.3, show.legend = FALSE, inherit.aes = FALSE
                      ) + 
                theme(legend.position = "bottom",
                      axis.title.x = element_text(inherit.blank = FALSE))
            
        ggsave(filename = paste(figure_term, "gCAR.pdf", sep = '_'),
               plot = f_car, width = 16, height = 9, scale = 0.75)
        
        # Export the comprehensive plot
        multi_panel_figure(width = 280, height = 240, columns = 7, rows = 6) %>%
            fill_panel(f_car, column = 1:7, row = 1:3) %>%
            fill_panel(f_coef, column = 1:3, row = 4:6) %>% 
            fill_panel(f_acc_size, column = 4:5, row = 4:6) %>%
            fill_panel(f_history, column = 6:7, row = 4:6) %>%
                save_multi_panel_figure(paste(figure_term, "gCAR-Size-History.pdf", sep = '_'))
    }
}
# take a look at the PLS estimators of grouped stocks
map_df(PLS_grp, ~ map_df(.x, "estimator", .id = "quarter"), .id = "year")

# Part III, focus on the stocks which classification results are continually ----
# one stock is clustered into a same class among quarters of a year 
library(DBI)
library(RSQLite)
library(dbplyr)
# link to database
QEA_db <- dbConnect(RSQLite::SQLite(), "~/OneDrive/Data.backup/QEAData/QEA_db.sqlite")

# function to query accounting data in reports from SQLite database
tbl_query <- function(database = QEA_db, 
                      table_name,  # name of table in database
                      stock_cd,  # the symbols of stocks
                      report_period,  # quarterly or annually
                      accounting_period,  # specific accounting period
                      report_type = "A"  # 'A' is whole firm, 'B' is parent firm 
                      ) {

    query_data <- tbl(database, table_name) %>% filter(Stkcd %in% !!stock_cd)
    
    if (report_period == "yearly") {  # for annual report, just need the characters of year
        
            query_data %<>% collect() %>% 
                    mutate("Accper" = as.Date(Accper, origin = "1970-01-01")) %>% 
                    filter(as.character(year(Accper)) %in% !!accounting_period)
        
    } else if (report_period == "quarterly") {
            # for quarterly report, character of year or date both are allowed 
            if (nchar(accounting_period) == 4) {  # character of particular year 
                    # manually convert the year character to the date of a end accounting period
                    accounting_period %<>% stringr::str_c(c("0331", "0630", "0930", "1231"))
            } 
         
            if (all(nchar(accounting_period) %in% c(8, 10))) {
                    # date in form of '%Y%m%d' or `%Y-%m-%d' 
                    accounting_period %<>% lubridate::ymd() %>% as.integer()
                 
                    query_data %<>% filter(Accper %in% !!accounting_period,
                                           Typrep == report_type) %>% 
                            collect() %>% 
                            mutate("Accper" = as.Date(Accper, origin = "1970-01-01"))
                
            } else stop("Please comfirm the pattern of date record is correct.")
    }
    
    return(mutate(query_data, "Accper" = as_factor(as.character(Accper))))
}

# function to compose the functions of select, unnest, and group_nest 
select_nest <- function(x, select_col = "g_PLS", value_col, key_name) {
    
    select(x, all_of(select_col), all_of(value_col)) %>% 
            unnest(cols = all_of(value_col)) %>% 
            group_nest(Accper, g_PLS, .key = key_name)
}

# taking the intersection of quarterly classification results,
# the quarterly PLS classification results will be reshaped to yearly clustering
PLS_grp_inter <- vector(mode = "list", length = length(Accprd)) %>% 
        set_names(names(Accprd))

for (y in seq_along(PLS_grp)) {
    
    if (# assure the levels of classification at each quarters are same
        map_dbl(PLS_grp[[y]], ~ length(levels(pull(.x$cluster, g_PLS)))) %>% 
        unique() %>% length() %>% `==`(1)
        ) {
    
        PLS_grp_inter[[y]] <- map(PLS_grp[[y]], "cluster") %>% 
                                reduce(inner_join, by = c("Stkcd", "g_PLS"))
        
    } else stop(paste("The quarterly level of groups aren't same with at year",
                      names(PLS_grp)[y])
                )
}

# take the complement of stocks existed in `PLS_grp_inter` with sample,
# the stocks in this set are the PLS classification of them
# is unstable among quarters within a accounting annual 
PLS_grp_unstable <- lapply(PLS_grp, map, "cluster") %>% 
        map2(PLS_grp_inter, ~ map(.x, .f = anti_join, y = .y, by = c("Stkcd", "g_PLS")))

# query accounting information in quarterly reports 
# accounting to symbols of stocks and accounting date
PLS_grp_unstable %<>% 
        map_dfr(~ bind_rows(.x, .id = "quarter"), .id = "year") %>% 
        group_nest(year, quarter, g_PLS, .key = "Stkcd") %>% 
        mutate("Stkcd" = map(Stkcd, pull, Stkcd),
               "date_prefix" = year,
               "date_suffix" = factor(quarter, 
                                      levels = c("1st", "2nd", "3rd", "4th"),
                                      labels = c("0331", "0630", "0930", "1231"))
               ) %>% 
        unite(date_prefix, date_suffix, col = "Accper", sep = "") %>% 
        mutate("accounting_quarter" = map2(Stkcd, Accper,
                                           ~ tbl_query(table_name = "quarter",
                                                       report_period = "quarterly",
                                                       stock_cd = .x,
                                                       accounting_period = .y)
                                           )
               ) %>% 
        select(-Accper)

# query accounting information in yearly and quarterly reports 
# accounting to symbols of stocks and particular year 
# for stocks which PLS classification are coherent
PLS_grp_inter %<>% map(group_nest, g_PLS, .key = "Stkcd") %>% 
        imap(function(y, z) {
                mutate(y, 
                       "Stkcd" = map(Stkcd, pull, "Stkcd"),
                       "accounting_year" = map(Stkcd, 
                                               ~ tbl_query(table_name = "annual", 
                                                           report_period = "yearly", 
                                                           stock_cd = .x,
                                                           accounting_period = z)
                                               ),
                       "accounting_quarter" = map(Stkcd, 
                                                  ~ tbl_query(table_name = "quarter",
                                                              report_period = "quarterly",
                                                              stock_cd = .x,
                                                              accounting_period = z)
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
# compose and compare accounting indicator PE between `inter` with `unstable`
PLS_grp_PE <- full_join(by = c('g_PLS', 'Accper'),
                        select_nest(PLS_grp_inter, 
                                    value_col = "accounting_quarter_PE", 
                                    key_name = "inter_PE"),
                        select_nest(PLS_grp_unstable, 
                                    value_col = "accounting_quarter_PE", 
                                    key_name = "unstable_PE")
                        ) %>% 
        mutate('g_PLS' = fct_relabel(g_PLS, ~ str_c('g_', .)),
               'year' = year(Accper)
               ) %>% 
        arrange(Accper, g_PLS)

for (y in seq_along(Accprd)) {

    filter(PLS_grp_PE, year == names(Accprd)[y]) %>% 
    pivot_longer(cols = c('inter_PE', 'unstable_PE'),
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

    ggsave(filename = glue("{names(Accprd)[y]}_EP_grp_dist.pdf"),
           path = file.path('~/OneDrive/Data.backup/QEAData', model_type, names(Accprd)[y]),
           width = 16, height = 9)
}
