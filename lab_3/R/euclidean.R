#'Euclidean Algorithm 
#'
#'Algorithm to look for the GCD of two numbers
#'
#'@param a A number
#'@param b A number
#'@return The GCD of \code{a} and \code{b}
#'
#'@references \url{https://en.wikipedia.org/wiki/Euclidean_algorithm}


euclidean <- function(a, b)
{
  if(!is.numeric(a)){stop()}
  if(!is.numeric(b)){stop()}
  rk_1 <- a;
  rk_2 <- b;
  # Recurrence Formula:  r_k =  r_k-1 modulo r_k-2
  # Increment k until r_k-2 == 0 
  while(rk_2 != 0) {
    rk      <- rk_1%%rk_2; # remainder
    rk_1    <- rk_2;       # proceed in recurrence
    rk_2    <- rk;
  }
  return(rk_1)
}
