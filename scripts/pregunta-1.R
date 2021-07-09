# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, tidyverse)
setwd(this.path::this.dir())

source('./lib/transactions.R')
source('./lib/rules.R')
# ------------------------------------------------------------------------------
#
#
#
#
# ------------------------------------------------------------------------------
# Pregunta 1
# ------------------------------------------------------------------------------
# Para aquellas canciones que alcanzaron el puesto número 1 durante el período 
# 2018-2020, interesa investigar cuáles son los niveles de los atributos 
# musicales que se asocian más a menudo entre sí y/o con los términos del 
# vocabulario frecuentemente utilizado en sus letras.
# ------------------------------------------------------------------------------
#
#
#
#
trans <- load()

#
# Creamos la reglas
#
rules = generate_rules(trans, support=0.1, confidence=0.5)
# plot_rules(rules, interactive=FALSE)


#
# Filtramos la reglas 
#
result <- arules::subset(
  rules, 
  subset = (lhs %pin% "danceability=high")&(rhs %pin% "term_")&lift>1&lift<2
)
show_rules(result, top = 20)
plot_rules(rules, interactive=FALSE)


