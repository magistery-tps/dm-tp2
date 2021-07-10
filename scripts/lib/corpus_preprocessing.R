# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, tidyverse, stopwords, tm, stringi, stringr, SnowballC)
# ------------------------------------------------------------------------------
#
#
#
#
#
# ------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------
generate_corpus <- function(
  data, 
  pro.genius         = TRUE, 
  pro.symbols        = TRUE, 
  pro.stopwords      = TRUE,
  language_stopwords = "english",
  extra_stopwords    = c(),
  pro.min            = TRUE, 
  pro.num            = TRUE, 
  pro.accents        = TRUE,
  pro.spaces         = TRUE, 
  pro.stemm          = TRUE
) {
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
    corpus.pro <- tm_map(
      corpus.pro, 
      content_transformer(function(x) gsub("More on Genius.*","", x))
    )
  }
  
  
  # Convertimos el texto a minúsculas
  if(pro.min){
    corpus.pro <- tm_map(corpus.pro, content_transformer(tolower))
  }
  
  
  # Removemos palabras vacias
  if(pro.stopwords){
    stopwords_list <- c(extra_stopwords, stopwords(language_stopwords))
    corpus.pro <- tm_map(corpus.pro, removeWords, stopwords_list)
  }
  
  
  # Removemos números
  if(pro.num){
    corpus.pro <- tm_map(corpus.pro, removeNumbers)
  }
  
  
  if(pro.symbols){
    # Removemos puntuaciones
    corpus.pro <- tm_map(corpus.pro, removePunctuation)
    
    # Removemos todo lo que no es alfanumérico
    corpus.pro <- tm_map(
      corpus.pro, 
      content_transformer(function(x) str_replace_all(x, "[[:punct:]]", " "))
    )
    
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
generate_term_document_matrix <- function(
  corpus, 
  ponderacion, 
  n_terms, 
  language="english"
) {

  # Genero la matriz TD y la transformo en una matriz
  dtm <- TermDocumentMatrix(
    corpus, 
    control = list(weighting = ponderacion, language = language)
  )
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

generate_document_term_df <- function(
  df_features, 
  n_terms         = 1000, 
  extra_stopwords = c()
) {
  corpus <- generate_corpus(
    df_features$lyric, 
    pro.stemm = FALSE,
    extra_stopwords = extra_stopwords
  )
  
  # Generación de la Matríz Término-Documento del corpus
  document_term_matrix <- generate_term_document_matrix(
    corpus, 
    ponderacion = "weightTfIdf", 
    n_terms     = n_terms
  )
  
  rm(corpus)

  data.frame(document_term_matrix)
}



