#' Dynamic algorithm for the Knapstack problem
#' 
#' @param x data frame with two columns \code{w} (weight) and \code{v} (value) of items to place in the knapsack
#' 
#' @param W the maximum weight (numeric) the knapsack can hold
#' 
#' @return theoretical maximum \code{$value} (knapsack value) composed of \code{$num} (which items)
#'
#' @export

knapsack_dynamic <- function(x, W){
  
  if(!is.data.frame(x)){stop("Not correct object")}
  if(!is.numeric(W) || W < 0){stop("Not correct W")}
  
  v <- x$v
  w <- x$w
  n <- length(x$v)
  m <- matrix(NA, nrow = n, ncol = W + 1)
  m[1,] <- 0
  
  for(i in 2:n){
    for(j in 1:W){
      if(w[i] > j){
        m[i,j] = max(m[i-1,j], 0)
      }
      else{
        m[i,j]= max(m[i-1,j], m[i-1, j-w[i]]+v[i])
      }
    }
  }
  c <- W
  elements <- list()
  elements$value <- m[n, W]
  while(m[n,c] > 0){
    if(m[n, c] == m[n-1,c]){
      n <- n - 1
    }
    else{
      elements$elements <- c(n, elements$elements)
      c <- c - w[n]
      n <- n-1
    }
  }
  return(elements)
}