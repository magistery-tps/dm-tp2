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

limit <- function(rules, top=10) {
  arules::inspect(head(sort(rules, by="lift", decreasing = T), top))
}

generate_rules <- function(
  transactions, 
  support     = 0.1, 
  confidence  = 0.5,
  appearance  = NULL, 
  top         = 10,
  interactive = FALSE
) {
  result <- arules::apriori(
    transactions,
    parameter = list(
      support    = support, 
      confidence = support, 
      target     = "rules"
    ),
    appearance = appearance
  )
  print(result)
  result
}

show_rules <- function(rules, top=10) {
  print(rules)
  limit(rules, top)
  rules
}
