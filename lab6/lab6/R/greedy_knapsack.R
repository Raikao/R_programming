#' Greedy Heuristic approach for the knapsack problem
#' 
#' @param x data frame with two columns \code{w} (weight) and \code{v} (value) of items to place in the knapsack
#' 
#' @param W the maximum weight (numeric) the knapsack can hold
#' 
#' @return theoretical maximum \code{$val} (knapsack value) composed of \code{$ids} (which items)
#'
#' @export
#' 


greedy_knapsack <- function(x, W){
  
  if(!is.data.frame(x)){stop("Not correct object")}
  if(!is.numeric(W) || W < 0){stop("Not correct W")}
  elements <- list()
  elements$value <- 0
  elements$elements <- vector()
  n <- nrow(x)
  v <- x$v
  w <- x$w
  sorting <- data.frame(values = x$v/x$w)
  sorting$ids <- c(1:n)
  sorted <- sorting[order(-sorting$values),]
  remainingW <- W
  i <- 1
  while(w[sorted$ids[i]]<= remainingW && i <= n){
      elements$value <- elements$value + v[sorted$ids[i]]
      elements$elements <- c(elements$elements, sorted$ids[i])
      remainingW <- remainingW - w[sorted$ids[i]]
      i = i+1
  }
  return(elements)
}