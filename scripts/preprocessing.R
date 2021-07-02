# ------------------------------------------------------------------------------
# Importamos dependencias
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
# Funciones
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


mean_track_features <- function(track_features) {
  df_mean_features = track_features_mean(track_features)
  
  df_mean_reproductions <- get_track_artist_album_mean_reproductions(track_features)
  df_best_positions <- get_track_artist_album_min_position(track_features)
  
  # Unión de todos los df
  df_tmp <- merge(
    x=df_mean_features,
    y=df_mean_reproductions,
    by.x = c("artist","track", "album"), 
    by.y = c("artist","track", "album")
  )
  
  result <- merge(
    x=df_mean_features, 
    y=df_best_positions,
    by.x = c("artist","track", "album"), 
    by.y = c("artist","track", "album")
  )
  
  
  # Eliminamos los pasos intermedios
  rm(df_best_positions, df_mean_features, df_mean_reproductions, df_tmp)
  
  
  result
}



