# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, tidyverse)

setwd(this.path::this.dir())
source('./lib/transactions.R')
# ------------------------------------------------------------------------------
#
#
#
#
# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------
transactions <- load()
transactions

image(transactions)

summary(transactions)

rules <- arules::apriori(
  transactions,
  parameter = list(support=0.1, confidence=0.8, target = "rules")
)
arules::inspect(head(sort(rules, by="lift", decreasing = T), 10))


plot(rules, measure = c("support", "lift"), shading = "confidence", jitter = 0)
arules::summary(rules)


# Es posible redefinir el orden de las métricas

# El método two-key permite incluir la dimensión de orden (cantidad de items)
plot(rules, method = "two-key plot")

# Es posible generar un gráfico interactivo
sel <- plot(rules, measure = c("support", "lift"), shading = "confidence", interactive = TRUE)

# Muestra las reglas de forma matricial y por índice de los items
plot(subrules2, method = "matrix", shading = "support")
