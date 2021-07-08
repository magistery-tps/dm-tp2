library(pacman)
p_load_gh("EmilHvitfeldt/textdata", "juliasilge/tidytext")
p_load(tidyverse, tidytext)

# Cuando se pide get_sentiments ofrece opcion para instalar el paquete nrc que tiene los sentimientos
get_sentiments("nrc")

# Esta es otra libreria numerica
as.data.frame(get_sentiments("afinn"))



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
