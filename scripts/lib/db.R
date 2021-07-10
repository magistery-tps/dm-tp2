# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, tidyverse)

source('../lib/data-access.R')
# ------------------------------------------------------------------------------
#
#
#
#
#
# ------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------
get_tracks <- function(collection) {
  collection <- get_collection(collection)
  collection$find(
    '{}',
    fields = '{
      "_id": false,
      "artist": true,
      "name": true,
      "album": true,
      "position": true,
      "danceability": true,
      "energy": true,
      "loudness": true,
      "speechiness": true,
      "acousticness": true,
      "instrumentalness": true,
      "liveness": true,
      "valence": true,
      "tempo": true,
      "duration_ms": true,
      "lyric": true,
      "reproductions": true,
      "language": true
    }'
  ) %>%
    drop_na %>%
    rename(track = name) %>%
    unique()
}

show_language <- function(df) {
    cat('Track by language: ')
    df %>% group_by(language) %>% tally()
}