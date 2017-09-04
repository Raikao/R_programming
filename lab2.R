name <- "Alejandro Garcia"
liuid <- "alega695"

#Sheldon game

sheldon_game <- function(player1, player2){
  vector = list("scissors","lizard", "paper", "rock", "spock")
  if(!(player1 %in% vector && player2 %in% vector)){stop() }
  else if (player1 == player2){ return("Draw!") }
  else if((player1 == "scissors" && (player2 =="paper" || player2 == "lizard")) ||
          (player1 == "paper" && (player2 == "rock" || player2 == "spock")) ||
          (player1 == "rock" && (player2 == "lizard" || player2 == "scissors")) ||
          (player1 == "lizard" && (player2 == "spock" || player2 == "paper")) ||
          (player1 == "spock" && (player2 == "scissors" || player2 == "rock"))){return("Player 1 wins!")}
  
  else{return("Player 2 wins!")}
  
}



my_moving_median <- function(x, n, ...){
  if(!is.vector(x) && !is.numeric(x)){stop()}
  if(!is.numeric(n)){stop()}
  z <- list(...)
  stopifnot(n == floor(n))
  ret = vector()
  for(i in 1:(length(x)-n)){
    if(!is.null(z$na.rm) && z$na.rm){ret[i] <- median(x[i:(i+n)], na.rm = TRUE)}
    else{ret[i] <- median(x[i:(i+n)])}
  }
  return(ret)
}


for_mult_table <- function(from, to){
  stopifnot(is.numeric(from) && is.numeric(to))
  vect <- c(from:to)
  vec <- vector()
  for(i in 1:length(vect)){
    vec <- append(vec, vect[i]*vect)
  }
  return(matrix(vec, nrow=length(vect), ncol = length(vect)))
}



#Cor matrix is not mandatory


find_cumsum <- function(x, find_sum){
  if(!is.vector(x) && !is.numeric(x)){stop()}
  if(!is.numeric(find_sum)){stop()}
  i <- 1
  sum <- 0
  while(sum < find_sum && i <= length(x)){
    sum <- sum + x[i]
    i <- i+1
  }
  return(sum)
}



while_mult_table <- function(from, to){
  stopifnot(is.numeric(from) && is.numeric(to))
  vect <- c(from:to)
  vec <- vector()
  i =1
  while(i <= length(vect)){
    vec <- append(vec, vect[i]*vect)
    i <- i+1
  }
  return(matrix(vec, nrow=length(vect), ncol = length(vect)))
}




#trial division factorization() not mandatory



repeat_find_cumsum <- function(x, find_sum){
  if(!is.vector(x) && !is.numeric(x)){stop()}
  if(!is.numeric(find_sum)){stop()}
  i <- 1
  sum <- 0
  repeat {
  if(sum < find_sum && i <= length(x)){
    sum <- sum + x[i]
    i <- i+1
  }
    else{break}
  }
  return(sum)
}



repeat_my_moving_median <- function(x, n, ...){
  if(!is.vector(x) && !is.numeric(x)){stop()}
  if(!is.numeric(n)){stop()}
  z <- list(...)
  stopifnot(n == floor(n))
  ret = vector()
  i = 0
  repeat{
  if(i <= length(x)-n){
    if(!is.null(z$na.rm) && z$na.rm){ret[i] <- median(x[i:(i+n)], na.rm = TRUE)}
    else{ret[i] <- median(x[i:(i+n)])}
    i <- i+1
  }
  else{break}
  }
  return(ret)
}



in_environment <- function(env){
  return(ls(env))
}




cov <- function(X){
  if(!is.data.frame(X)){stop()}
  ret <- lapply(X, function(X){sd(X)/mean(X)})
  return(unlist(ret))
}



moment <- function(i){
  if(!is.numeric(i)){stop()}
  f <- function(x){
    mean((x-mean(x))^i)
  }
}




