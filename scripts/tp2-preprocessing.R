# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this::path, tidyverse)

setwd(this.path::this.dir())
source('../lib/data-access.R')
source('./lib/corpus_preprocessing.R')
source('./lib/features_preprocessing.R')
source('./lib/transactions.R')
source('./lib/sentiment.R')
# ------------------------------------------------------------------------------
#
#
#
#
# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------
df_track_features <- get_tracks('track_features_top_10_lyric')

df_features       <- generate_features(df_track_features)

df_document_term  <- generate_document_term_df(df_features, n_terms = 500)

df_features       <- cbind(df_features, df_document_term) %>% select(-lyric)

transactions      <- generate_transactions(df_features)

transactions      <- append_sentiment_properties(transactions)

save(transactions)

View(transactions)
