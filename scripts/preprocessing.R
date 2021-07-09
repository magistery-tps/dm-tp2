# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, tidyverse)
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

df_features <- generate_features(
  df_track_features, 
  disc_callback = function(df) {
    #
    # Aca definimos que columnas queremos agregar como features...
    #
    r <- join(disc_danceability(df), disc_energy(df))
    r <- join(r, disc_liveness(df))
    r <- join(r, disc_speechiness(df))
    r <- join(r, disc_valence(df))
    r <- join(r, disc_position(df))
    r <- join(r, disc_top_1_track(df))
    r <- join(r, disc_best_album(df))
    r
  }
)
# View(df_features)

df_document_term  <- generate_document_term_df(
  df_features, 
  n_terms = 50000,
  extra_stopwords = c('like', 'got')
)

df_features_tmp <- cbind(df_features %>% select(-lyric), df_document_term)

transactions <- generate_transactions(df_features_tmp)

# transactions <- append_nrc_sentiment_features(transactions)

transactions <- append_afinn_sentiment_features(transactions)

nrow(transactions)

save(transactions)

# View(transactions)
  