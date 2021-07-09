# ------------------------------------------------------------------------------
# Import dependencies
# ------------------------------------------------------------------------------
library(pacman)
p_load(this.path, tidyverse, arules, arulesViz)
# ------------------------------------------------------------------------------
#
#
#
#
#
# ------------------------------------------------------------------------------
# Functions
# ------------------------------------------------------------------------------
plot_rules <- function(rules, interactive = FALSE) {
  plot(
    rules, 
    measure = c("support", "lift"), 
    shading = "confidence", 
    jitter  = 0,
    engine  = if(interactive) 'interactive' else 'default'
  )
}

search_rules <- function(
  transactions, 
  parameter = NULL, 
  appearance=NULL, 
  top = 10,
  interactive = FALSE
) {
  rules <- arules::apriori(
    transactions,
    parameter  = parameter,
    appearance = appearance
  )
  arules::inspect(head(sort(rules, by="lift", decreasing = T), top))
  plot_rules(rules, interactive)
}

