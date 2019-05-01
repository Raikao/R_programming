#' Brute Force Algorithm for the Knapsack problem 
#' 
#' With this algorithm we can solve the Knapsack problem in the brute force way, this approach is of complexity O(2^n)
#'
#'@param x data frame with two columns \code{w}(weight) and \code{v}(value) of items
#' 
#'@param W maximum weight the knapsack can hold
#'
#'@param parallel if true the algorithm will parallelize some parts of it
#'
#'@return theoretical maximum \code{$value} (knapsack value) composed of \code{$elements} (which items)
#'
#'@export

# x <- data frame of knapsack
# W <- knapsack size
brute_force_knapsack_parallel<- function(x, W, parallel = FALSE){
  if(!is.data.frame(x)){stop("Not correct object")}
  if(!is.numeric(W) || W < 0){stop("Not correct W")}
  elements <- which.max(x$w <= W)
  weight <- x$w[elements]
  value <- x$v[elements]
  
  res <- list(value = value, elements = elements)
  cores <- 1
  if(parallel){
    cores <-  parallel::detectCores()
    if(Sys.info()["sysname"] == "Windows"){stop("Parallel is not possible in Windows system")}
  }
    all_combinations <- unlist(parallel::mclapply(1:nrow(x),function(i){
                                                         combinat::combn(rownames(x), 
                                                                         m = i, 
                                                                         simplify = FALSE, 
                                                                         fun = as.numeric)},
                                                       mc.cores = cores),recursive = FALSE)
    
    values <- parallel::mclapply(all_combinations, 
                                     function(all_combinations){ifelse(sum(x[all_combinations,"w"]) <=W,
                                                           sum(x[all_combinations,"v"]),
                                                           0)},
                                     mc.cores =cores)
  
      
    res$value <- values[[which.max(values)]]
    res$elements <- all_combinations[[which.max(values)]]

    
  
  return(res)
}