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

my_moving_medians <- function(x, n, na.rm){
  stopifnot(all(x ==floor(x)))
  sotopifnot(n == floor(n))
  
  
  
  
}





