# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this::path, tidyverse, arules)
# ------------------------------------------------------------------------------
#
#
#
#
#
# ------------------------------------------------------------------------------
# Constants
# ------------------------------------------------------------------------------
FILE_PATH = "../data/transctiosn.csv"
# ------------------------------------------------------------------------------
#
#
#
#
#
# ------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------
generate_transactions <- function(df_features) {
  df_features$tid  <- 1:nrow(df_features)
  
  feature_columns <- setdiff(colnames(df_features),c('artist', 'track', 'album'))

  transactions <- reshape2::melt(
    data = df_features[,feature_columns], 
    id.vars = c('tid')
  ) %>%
    filter(value !=0)
  
  term_transactions <- transactions %>%
    filter(value==1) %>%
    mutate(variable = paste0("TERM_", variable)) %>%
    select(-c(value))
  
  cat_transactions  <- transactions %>%
    filter(value!=1) %>%
    mutate(variable = paste0(gsub('cat_', '', variable) , '=', value)) %>%
    select(-c(value))

  transactions <- rbind(term_transactions, cat_transactions)
  
  transactions %>% rename(item = variable)
}

save <- function(transactions, file_path = FILE_PATH) {
    write.table(transactions, file=file_path, row.names = FALSE)
}

load <- function(file_path = FILE_PATH) {
  read.transactions(
    file   = file_path, 
    format = 'single',
    header = TRUE, 
    sep    = ' ',
    cols   = c('tid','item'),
    quote  = '"'
  )
}


