# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this::path, tidyverse)
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
  df_features$id  <- 1:nrow(df_features)
  
  feature_columns <- setdiff(colnames(df_features),c('artist', 'track', 'album'))
  
  transactions <- reshape2::melt(data = df_features[,feature_columns], id.vars = c('id')) %>%
    filter(value !=0)
  
  term_transactions <- transactions %>%
    filter(value==1) %>%
    mutate(variable = paste0("TERM_", variable)) %>%
    select(-c(value))
  
  cat_transactions  <- transactions %>%
    filter(value!=1) %>%
    mutate(variable = paste0(gsub('cat_', '', variable) , '=', value)) %>%
    select(-c(value))
  
  rbind(term_transactions, cat_transactions)
}