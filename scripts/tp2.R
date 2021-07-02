# ------------------------------------------------------------------------------
# Importamos dependencias
# ------------------------------------------------------------------------------
library(pacman)
p_load_gh("EmilHvitfeldt/textdata", "juliasilge/tidytext")
p_load(this::path, tidyverse, tidyverse, tidytext)

setwd(this.path::this.dir())
source('../lib/data-access.R')
source('./functions.R')

# Cuando se pide get_sentiments ofrece opcion para instalar el paquete nrc 
# que tiene los sentimientos
get_sentiments("nrc")

# Esta es otra libreria numerica
get_sentiments("afinn")
# ------------------------------------------------------------------------------
#
#
#
#
#
# ------------------------------------------------------------------------------
# Programa principal
# ------------------------------------------------------------------------------
# VIDEO EJEMPLO: https://www.youtube.com/watch?v=Av3mC1Jaetk&t=1169s

track_features <- get_tracks('track_features_top_10_lyric')
colnames(track_features)


df_lyrics = track_features %>% select(lyric)
colnames(df_lyrics)


#
# Preprocesamiento Features
#
df_means_feat = aggregate(
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
  data=track_features, FUN=function(x) mean(x,na.rm = T))

names(df_means_feat)


df_means_streams = aggregate(
  reproductions 
  ~
    track + artist + album
  , 
  data=track_features, FUN=function(x) mean(x,na.rm = T))


df_best_position = aggregate(
  position 
  ~
    track + artist + album
  , 
  data=track_features, FUN=min)



# Unión de todos los df
df_feats_ag = merge(x=df_means_feat,
                    y=df_means_streams,
                    by.x = c("artist","track", "album"), 
                    by.y = c("artist","track", "album")
)

df_feats_ag = merge(x=df_feats_ag, 
                    y=df_best_position,
                    by.x = c("artist","track", "album"), 
                    by.y = c("artist","track", "album")
)



# Eliminamos los pasos intermedios
rm(df_best_position, df_means_feat, df_means_streams)



#
# Pre-procesamiento del corpus 
#



# Corremos la función (operaciones del LAB08)
corpus.pro = df2corpus.pro(df_lyrics$lyric, pro.stemm = FALSE)

# Recuperamos la letra de la primera canción
# Carlos Vives (Robarte un beso)
inspect(corpus.pro[1])
df_lyrics[1,]


# Generación de la Matríz Término-Documento del corpus
matriz <- corpus.pro2tdm(corpus.pro, "weightTfIdf", 10000)
dim(matriz)




# Matriz es el resultado de matriz <- corpus.pro2tdm(corpus.pro, "weightTfIdf", 1000)
df_tm = as.data.frame(matriz)

letras<-colnames(df_tm)
nombre<-rep('cancion1',length(letras))

df_letras<-data.frame(nombre,letras)
colnames(df_letras)[2]<-'word'

# necesita pasar las palabras a formato unnest_tokens Split a column into tokens, de la libreria tidytext
df_letras %>% unnest_tokens(linenumber,nombre)

nrc_joy <- get_sentiments("nrc") %>% filter(sentiment == "joy")
columnas_joy<-df_letras %>% inner_join(nrc_joy) %>% count(word, sort = TRUE)

nrc_sadness <- get_sentiments("nrc") %>% filter(sentiment == "sadness")
columnas_sad<-df_letras %>% inner_join(nrc_sadness) %>% count(word, sort = TRUE)


df_joy<-df_tm[, columnas_joy$word]
totales_joy<-rowSums(df_joy)

df_sad<-df_tm[, columnas_sad$word]
totales_sad<-rowSums(df_sad)


