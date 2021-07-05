# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load_gh("EmilHvitfeldt/textdata", "juliasilge/tidytext")
p_load(this::path, tidyverse, tidytext)
# ------------------------------------------------------------------------------
#
#
#
#
# ------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------
append_afinn_sentiment_features <- function(transactions) {
  transactions %>%
    inner_join(as.data.frame(get_sentiments("afinn")), by=c('item' = 'word')) %>%
    mutate(item = paste('positive=', value, sep='')) %>%
    select('tid', 'item') %>%
    union_all(transactions)
}

append_nrc_sentiment_features <- function(transactions) {
  transactions %>%
    inner_join(get_sentiments("nrc"), by=c('item' = 'word')) %>%
    mutate(item = paste('sentiment=', sentiment, sep='')) %>%
    select('tid', 'item') %>%
    union_all(transactions)
}