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
track_features_mean <- function(track_features) {
  aggregate(
    cbind(
      danceability , 
      acousticness ,
      energy,
      duration_ms,
      liveness , 
      loudness ,
      speechiness , 
      tempo ,
      valence)
    ~
      track + artist + album
    , 
    data=track_features, 
    FUN=function(x) mean(x,na.rm = T)
  )
}

get_track_artist_album_mean_reproductions <- function(track_features) {
  aggregate(
    reproductions 
    ~
      track + artist + album
    , 
    data=track_features, 
    FUN=function(x) mean(x,na.rm = T)
  )
}

get_track_artist_album_min_position <- function(track_features) {
  aggregate(
    position 
    ~
      track + artist + album
    , 
    data=track_features,
    FUN=min
  )
}


generate_features <- function(track_features) {
  df_mean_features <- track_features_mean(track_features)
  df_mean_reproductions <- get_track_artist_album_mean_reproductions(track_features)
  df_best_positions <- get_track_artist_album_min_position(track_features)
  
  
  result <- merge(
    x=df_mean_features,
    y=df_mean_reproductions,
    by.x = c("artist","track", "album"), 
    by.y = c("artist","track", "album")
  )
  
  result <- merge(
    x=result, 
    y=df_best_positions,
    by.x = c("artist","track", "album"), 
    by.y = c("artist","track", "album")
  )
  
  
  result <-discretize_features(result)
  
  
  df_lyrics <- track_features %>% distinct(track, artist, album, lyric)
  
  result <- merge(
    x=result, 
    y=df_lyrics,
    by.x = c("artist","track", "album"), 
    by.y = c("artist","track", "album")
  )

  # Eliminamos los pasos intermedios
  rm(df_best_positions, df_mean_features, df_mean_reproductions, df_lyrics)
  
  result
}

discretize_features <- function(
  df_features, 
  feature_columns = c(
    "danceability",
    "acousticness",
    "energy", 
    "duration_ms",
    "liveness",
    "loudness", 
    "speechiness",
    "tempo",
    "valence",
    "position"
  ),
  levels = c("low", "medium", "high", "very_high")
) {
  result <- df_features %>% select('artist', 'track', 'album')
  
  for (feature_column in feature_columns) {
    cat_column_name <- paste('cat_', feature_column, sep='')
    column_values <- df_features[[feature_column]]

    result[cat_column_name] = cut(
      column_values, 
      breaks=quantile(column_values),
      labels=levels
    )
  }
  result
}


