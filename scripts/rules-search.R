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
# Main
# ------------------------------------------------------------------------------
trans <- load()

#
# Creamos la reglas
#
rules = generate_rules(trans, support=0.1, confidence=0.5)
plot_rules(rules, interactive=FALSE)


#
# Filtramos la reglas
#
result <- arules::subset(
  rules, 
  subset = (lhs %pin% "danceability=high")&(rhs %pin% "term_")&lift>1&lift<2
)
show_rules(result, top = 20)
plot_rules(rules, interactive=FALSE)



