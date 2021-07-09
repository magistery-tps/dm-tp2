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
# Functions
# ------------------------------------------------------------------------------
graphic_search <- function(rules) {
  plot(rules, measure = c("support", "lift"), shading = "confidence", interactive = TRUE)
}

plot_rules <- function(rules) {
  plot(rules, measure = c("support", "lift"), shading = "confidence", jitter = 0)
}
# ------------------------------------------------------------------------------
#
#
#
#
# ------------------------------------------------------------------------------
# Main
# ------------------------------------------------------------------------------

# Load transactions
transactions <- load()
transactions
summary(transactions)


# Generate rules
rules <- arules::apriori(
  transactions,
  parameter = list(support=0.1, confidence=0.2, target = "rules")
  
 ,appearance = list(lhs="top1=yes")
#  ,appearance = list(lhs="best_album=yes")
)
arules::inspect(head(sort(rules, by="lift", decreasing = T), 10))


plot_rules(rules)
graphic_search(rules)






# El método two-key permite incluir la dimensión de orden (cantidad de items)
plot(rules, method = "two-key plot")

# image(transactions)
