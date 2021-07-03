# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load_gh("EmilHvitfeldt/textdata", "juliasilge/tidytext")
p_load(this::path, tidyverse, tidyverse, tidytext)

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

arules::inspect(head(transactions, 3))

summary(transactions)

rules <- arules::apriori(
  transactions, 
  parameter = list(support=0.09, confidence=0.5, minlen=2, target = "rules")
)
arules::inspect(head(sort(rules, by="lift", decreasing = T), 100))

rules.sub <- apriori(
  transactions, 
  parameter = list(support=0.09, confidence=0.5, minlen=2, target = "rules"), 
  appearance = list(items = c("danceability=very_high"))
)
arules::inspect(head(sort(rules.sub, by="lift", decreasing = T), 30))
