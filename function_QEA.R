# There are my self function to read and write file conveniently with specific tags

# function to read csv file with term 
mod_read <- function(file_name, ...) { # just need the foot file name
    
        dir(pattern = paste(Accprd, Pretype, Markettype, model_type, file_name, sep = '_'),
            full.names = TRUE
            ) %>%
        read_csv(na = '',  # for comma
                 col_types = cols(
                         .default = col_double(),
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
mod_write <- function(df, file_name, ...) { 
    
    write.csv(df, quote = F, row.names = F, ...,
              file = paste(Accprd, Pretype, Markettype, model_type, grp_num, 
                           paste0(file_name, '.csv'), 
                           sep = '_'
                           )
            )
    
	}

# function to export figure with term 
mod_figure <- function(file_name, fig_ratio, ...) { 

    full_path <- paste(Accprd, Pretype, Markettype, model_type, grp_num, 
                       glue('{file_name}_figure.pdf'), 
                       sep = '_'
                       )
    
    if (fig_ratio == 1L) {  # square
        
        ggsave(full_path, width = 9, height = 9, ..., device = "pdf")
        
    } else if (fig_ratio == 2L) {  # 4/3 ration
        
        ggsave(full_path, width = 8, height = 6, ..., device = "pdf")
        
    } else if (fig_ratio == 3L) {  # 16/9 ration
        
        ggsave(full_path, width = 16, height = 9, ..., device = "pdf")
        
    } else {print("Please customize the image output process!")}
  
}


