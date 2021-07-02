# ------------------------------------------------------------------------------
# Importamos dependencias
# ------------------------------------------------------------------------------
library(pacman)
p_load_gh("EmilHvitfeldt/textdata", "juliasilge/tidytext")
p_load(this::path, tidyverse, tidyverse, tidytext)

setwd(this.path::this.dir())
source('../lib/data-access.R')
source('./functions.R')
source('./preprocessing.R')
# ------------------------------------------------------------------------------
#
#
#
#
#
# ------------------------------------------------------------------------------
# Pre-procesamiento Features
# ------------------------------------------------------------------------------
# VIDEO EJEMPLO: https://www.youtube.com/watch?v=Av3mC1Jaetk&t=1169s

track_features <- get_tracks('track_features_top_10_lyric')
colnames(track_features)

# Nos quedamos con la media de todos los features agrupando por 
# track, arstist y album:
df_mean_track_features <- mean_track_features(track_features)
#
#
#
# ------------------------------------------------------------------------------
# Pre-procesamiento del corpus
# ------------------------------------------------------------------------------
corpus <- generate_corpus(track_features$lyric, pro.stemm = FALSE)

# Recuperamos la letra de la primera canción:
# - Artista: Arizona Zervas
# - Track: ROXANNE
inspect(corpus[-1])


# Generación de la Matríz Término-Documento del corpus
matriz <- generate_term_document_matrix(
  corpus, 
  ponderacion = "weightTfIdf", 
  n_terms     = 10000
)
dim(matriz)


# matriz es el resultado de matriz <- corpus.pro2tdm(corpus.pro, "weightTfIdf", 1000)
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


