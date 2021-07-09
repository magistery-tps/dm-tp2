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
transactions <- load()
summary(transactions)

#
# Pregunta 4
#
search_rules(
  transactions,
  parameter = list(support=0.1, confidence=0.2, target = "rules")
)
