# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, tidyverse, stringr)
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


generate_features <- function(track_features, disc_callback) {
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
  

  result <- disc_callback(result)
  
  
  df_lyrics <- track_features %>% distinct(track, artist, album, lyric)
  
  result <- merge(
    x=result, 
    y=df_lyrics,
    by.x = c("artist","track", "album"), 
    by.y = c("artist","track", "album")
  )

  # Eliminamos los pasos intermedios
  rm(df_best_positions, df_mean_features, df_mean_reproductions, df_lyrics)
  
  # Quitamos las filas con nulos
  result[complete.cases(result), ]
}


discretize_features <- function(df, feature_columns, level_fn, labels) {
  result <- df %>% select('artist', 'track', 'album')
  
  for (feature_column in feature_columns) {
    cat_column_name <- paste('cat_', feature_column, sep='')
    column_values <- df[[feature_column]]

    result[cat_column_name] = cut(
      column_values,
      breaks = level_fn(column_values),
      labels = labels
    )

    print(str_to_title(paste(feature_column, ' levels:', sep='')))
    cat('\t- Names: ')
    print(paste(labels, collapse=', '))
    cat('\t- Values: ')
    print(paste(level_fn(column_values), collapse=', '))
  }

  result
}


disc_danceability <- function(df) {
  discretize_features(
    df, 
    feature_columns = c('danceability'),
    level_fn        = function (values) c(0, 0.5, 0.75, 1),
    labels          = c("low", "medium", 'high')
  )
}

disc_energy <- function(df) { 
  discretize_features(
    df, 
    feature_columns = c('energy'),
    level_fn        = function (values) c(0, 0.52, 0.7, 1),
    labels          = c("low", "medium", 'high')
  )
}
  

disc_acousticness <- function(df) {
  discretize_features(
    df, 
    feature_columns = c('acousticness'),
    level_fn        = function (values) c(0, 0.5, 0.75, 1),
    labels          = c("low", "medium", 'high')
  )
}

disc_liveness <- function(df) {
  discretize_features(
    df, 
    feature_columns = c('liveness'),
    level_fn        = function (values) c(0, 0.5, 0.75, 1),
    labels          = c("low", "medium", 'high')
  )
}


disc_speechiness <- function(df) {
  discretize_features(
    df, 
    feature_columns = c('speechiness'),
    level_fn        = function (values) c(0, 0.5, 0.75, 1),
    labels          = c("low", "medium", 'high')
  )
}

disc_valence <- function(df) {
  discretize_features(
    df, 
    feature_columns = c('valence'),
    level_fn        = function (values) c(0, 0.52, 0.7, 1),
    labels          = c("low", "medium", 'high')
  )
}

disc_position <- function(df) {
  discretize_features(
    df, 
    feature_columns = c('position'),
    level_fn        = function (values) c(0.5 , 1.5, 4.5, 10.5),
    labels          = c("high", "medium", "low")
  )
}




#
# Temas en el top 1
#
disc_top_1_track <- function(df) { 
  df %>% mutate(
    top1 = case_when(
      track %in% c(
        "7 rings", 
        "All I Want for Christmas Is You", 
        "bad guy",  
        "Better Now", 
        "Blinding Lights", 
        "Call Out My Name", 
        "cardigan",  
        "Circles", 
        "DÁKITI", 
        "Dance Monkey", 
        "God's Plan", 
        "Havana (feat. Young Thug)",
        "HIGHEST IN THE ROOM", 
        "I Don't Care (with Justin Bieber)", 
        "I Love It (& Lil Pump)",  
        "In My Feelings", 
        "Lose You To Love Me", 
        "Lucky You (feat. Joyner Lucas)",
        "MIA (feat. Drake)", 
        "Mood (feat. iann dior)", 
        "Nice For What",  
        "Nonstop", 
        "positions", 
        "Rain On Me (with Ariana Grande)", 
        "ROCKSTAR (feat. Roddy Ricch)",  
        "SAD!", 
        "Señorita", 
        "Sunflower - Spider-Man: Into the Spider-Verse",  
        "Taki Taki (with Selena Gomez, Ozuna & Cardi B)", 
        "thank u, next",  
        "This Is America", 
        "WAP (feat. Megan Thee Stallion)"
      ) ~ 'yes',
      TRUE ~ 'no'
    )
  ) %>%
  select('artist', 'track', 'album', 'top1')
}


#
# Los 6 albums con mas canciones en el top 10
#
disc_best_album <- function(df) {
  df %>% mutate(
    best_album = case_when(
      album %in% c(
        'beerbongs & bentleys',
        'thank u, next',
        'WHEN WE ALL FALL ASLEEP, WHERE DO WE GO?', 
        'Scorpion', 
        'No.6 Collaborations Project', 
        "Hollywood's Bleeding", 
        'ye',
        'My Dear Melancholy,',
        'Legends Never Die',
        'Kamikaze',
        'After Hours',
        '?'
      ) ~ 'yes',
      TRUE ~ 'no'
   )
  ) %>%
  select('artist', 'track', 'album', 'best_album')
}

join <- function(a, b) a %>% inner_join(b, by=c('artist', 'track', 'album'))

discretize_all_features <- function(df) {
  r <- join(disc_danceability(df), disc_energy(df))
  r <- join(r, disc_liveness(df))
  r <- join(r, disc_speechiness(df))
  r <- join(r, disc_valence(df))
  r <- join(r, disc_position(df))
  r <- join(r, disc_top_1_track(df))
  r <- join(r, disc_best_album(df))
  r
}

