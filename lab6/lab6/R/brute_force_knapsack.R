#' Brute Force Algorithm for the Knapsack problem
#' 
#' With this algorithm we can solve the Knapsack problem in the brute force way, this approach is of complexity O(2^n)
#'
#'@param x data frame with two columns \code{w}(weight) and \code{v}(value) of items
#' 
#'@param W maximum weight the knapsack can hold
#'
#'@return theoretical maximum \code{$value} (knapsack value) composed of \code{$elements} (which items)
#'
#'@export

# x <- data frame of knapsack
# W <- knapsack size
brute_force_knapsack<- function(x, W){
  if(!is.data.frame(x)){stop("Not correct object")}
  if(!is.numeric(W) || W < 0){stop("Not correct W")}
  elements <- which.max(x$w <= W)
  weight <- x$w[elements]
  value <- x$v[elements]
  
  res <- list(value = value, elements = elements)
  if(sum(x$w) <= W){
    res$value <- sum(x$v)
    res$elements <- row.names(x)
  }
  
  
  for (i in seq(from = 2, to = nrow(x)-1)) {
    all_combinations <- combn(as.integer(row.names(x)), i)
    all_weights <- combn(x$w, i, sum)
    all_values <- combn(x$v, i, sum)
    
    possible_combination <- which(all_weights <= W)
    max_value <- which.max(all_values[possible_combination])
    
    tmp_weight <- all_weights[possible_combination[max_value]]
    tmp_value <- all_values[possible_combination[max_value]]
    tmp_elements <- all_combinations[, possible_combination[max_value]]
    
    if (any(tmp_value > value, is.na(value))) {
      weight <- tmp_weight
      value <- tmp_value
      elements <- tmp_elements
      
      res$value <- value
      res$elements <- elements
    }
    else{return(res)}
    
  }
  return(res)
}