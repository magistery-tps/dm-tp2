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
# plot_rules(rules, interactive=FALSE)


#
# Filtramos la reglas
#
result <- arules::subset(
  rules, 
  subset=(!rhs %pin% "danceability=high")&lift>1.72
)
show_rules(result, top =10)
plot_rules(rules, interactive=FALSE)



