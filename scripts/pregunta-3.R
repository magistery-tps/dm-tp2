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
# Pregunta 3
# ------------------------------------------------------------------------------
# En las letras de las canciones de los artistas con discos más exitosos 
# editados entre 2018 y 2020 ¿El vocabulario utilizado en la composición es más 
# limitado respecto del utilizado frecuentemente en otras canciones del top10? 
# ¿Los términos del vocabulario utilizado en estas letras permiten 
# caracterizarlas como mayormente positivas o negativas?
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
