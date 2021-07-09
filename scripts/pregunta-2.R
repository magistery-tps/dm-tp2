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
# Pregunta 2
# ------------------------------------------------------------------------------
# Para las canciones que ingresaron al TOP 10 durante el período 2018-2020 
# ¿Cuáles son los niveles de los atributos y los términos del vocabulario 
# frecuente de las letras que más inciden en la permanencia de las canciones
# en el puesto número 1 respecto de aquellas que nunca lo alcanzan?
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

