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
generate_corpus <- function(
  data, 
  pro.genius       = TRUE, 
  pro.symbols      = TRUE, 
  pro.stopwords    = TRUE,
  idioma_stopwords = "english",
  pro.min          = TRUE, 
  pro.num          = TRUE, 
  pro.accents      = FALSE,
  pro.spaces       = TRUE, 
  pro.stemm        = TRUE
) {
  library(tm)
  library(stringi)
  library(stringr)
  library(SnowballC) # para Stemming
  
  corpus = Corpus(VectorSource(enc2utf8(data)))
  
  if(pro.genius){
    # Elimino todo lo que aparece antes del primer []
    corpus.pro <- tm_map(corpus, content_transformer(
      function(x) sub('^.+?\\[.*?\\]',"", x)))
    
    # Elimino las aclaraciones en las canciones, por ejemplo:
    # [Verso 1: Luis Fonsi & Daddy Yankee]
    corpus.pro <- tm_map(corpus.pro, content_transformer(
      function(x) gsub('\\[.*?\\]', '', x)))
    
    # Elimino todo lo que aparece luego de 'More on Genius'
    corpus.pro <- tm_map(corpus.pro, content_transformer(function(x) gsub("More on Genius.*","", x)))
  }
  
  if(pro.min){
    # Convertimos el texto a minúsculas
    corpus.pro <- tm_map(corpus.pro, content_transformer(tolower))
  }
  
  if(pro.stopwords){
    # Removemos palabras vacias en español
    corpus.pro <- tm_map(corpus.pro, removeWords, stopwords(idioma_stopwords))
  }
  
  if(pro.num){
    # removemos números
    corpus.pro <- tm_map(corpus.pro, removeNumbers)
  }
  
  if(pro.symbols){
    # Removemos puntuaciones
    corpus.pro <- tm_map(corpus.pro, removePunctuation)
    
    # Removemos todo lo que no es alfanumérico
    corpus.pro <- tm_map(corpus.pro, content_transformer(function(x) str_replace_all(x, "[[:punct:]]", " ")))
    
    # Removemos puntuaciones
    corpus.pro <- tm_map(corpus.pro, removePunctuation)
  }
  
  if(pro.accents){
    replaceAcentos <- function(x) {stri_trans_general(x, "Latin-ASCII")}
    corpus.pro <- tm_map(corpus.pro, replaceAcentos)
  }
  
  if(pro.spaces){
    # Se eliminan los espacios adicionales
    corpus.pro <- tm_map(corpus.pro, stripWhitespace)
  }
  
  if(pro.stemm){
    # Se eliminan los espacios adicionales
    corpus.pro <- tm_map(corpus.pro, stemDocument, language=language)
  }
  
  rm(corpus)
  
  return(corpus.pro)
}


#
# Generación de la Matríz Término-Documento del corpus
#
generate_term_document_matrix <- function(corpus, ponderacion, n_terms, language="english") {
  
  # Genero la matriz TD y la transformo en una matriz
  dtm <- TermDocumentMatrix(corpus, control = list(weighting = ponderacion, language = language))
  matriz_td <- as.matrix(dtm)
  
  # Me quedo con los n_terms más frecuentes
  terminos_frecuentes = head(sort(rowSums(matriz_td), decreasing = T), n_terms)
  
  # Me quedo con la matriz transpuesta de los n_terms más frecuentes
  # Cada fila es un tema, cada columna un término
  matriz_mf = t(matriz_td[sort(names(terminos_frecuentes)),])
  
  # Paso a binaria la matriz (está o no está el término)
  matriz_mf[matriz_mf > 0] <- 1
  
  return(matriz_mf)
}

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
      "reproductions": true
    }'
  ) %>%
    drop_na %>%
    rename(track = name) %>%
    unique()
}


