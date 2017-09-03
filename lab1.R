name <- "Alejandro Garcia"
liuid <- "alega695"

lab_path <-
  "https://raw.githubusercontent.com/STIMALiU/AdvRCourse/master/Labs/Tests/lab1.yml"
set_assignment(lab_path)

#Vectors

my_num_vector <- function(){
  ret <- vector()
  ret[1] <- log10(11)
  ret[2] <- cos(pi/5)
  ret[3] <- exp(pi/3)
  ret[4] <- (1173 %% 7)/19
  return(ret)
}
  
filter_my_vector <- function(x, leq){
  x[which(x>=leq)] = NA
  return (x)
}

dot_prod <- function(a,b){
  return(sum(a*b))
}

approx_e <- function(N){
  return(sum(1/factorial(c(0:N))))
  
}


#Matrices

my_magic_matrix <- function(){
  return(matrix(c(4,3,8,9,5,1,2,7,6),3,3))
}



calculate_elements <- function(A){
  return(nrow(A)*ncol(A))
}

row_to_zero <- function(A, i){
  A[i,] = 0
  return(A)
}


add_elements_to_matrix <- function(A,x,i,j){
  A[i,j] = A[i,j]+x
  return(A)
}


#Lists
my_magic_list <- function(){
  a<-list(info="my own list", my_num_vector(), my_magic_matrix())
  return(a)
}


change_info <- function(x, text){
  x$info = text
  return(x)
}


add_note <- function(x, note){
  x$note = note
  return(x)
}



sum_numeric_parts <- function(x){
  return(sum(suppressWarnings(as.numeric(unlist(x))), na.rm=TRUE))
}


#Data frames

my_data.frame <- function(){
  id <- c(1,2,3)
  name <- c("John", "Lisa", "Azra")
  income <- c(7.30, 0.00, 15.21)
  rich <- c(FALSE, FALSE, TRUE)
  return(data.frame(id, name, income, rich))
}


sort_head <- function(df, var.name, n){
  return(df[order(-df[var.name])[1:n],])
}



add_median_variable <- function(df, j){
  media <- median(unlist(df[j]))
  compared_to_median <- vector()
  greater <- c(which(df[j]>media))
  smaller <- c(which(df[j]<media))
  med <- c(which(df[j]==media))
  compared_to_median[greater] <- "Greater"
  compared_to_median[smaller] <- "Smaller"
  compared_to_median[med] <- "Median"
  df$compared_to_median <- compared_to_median
  return(df)
}


analyze_columns <- function(df, j){
  ret <- list()
  media <- median(unlist(df[j[1]]))
  meen  <- mean(unlist(df[j[1]]))
  stand <- sd(unlist(df[j[1]]))
  vec <- c(meen, media, stand)
  names(vec) <- c("mean", "median", "sd")
  ret[[names(df[j[1]])]] <- vec
  media <- median(unlist(df[j[2]]))
  meen  <- mean(unlist(df[j[2]]))
  stand <- sd(unlist(df[j[2]]))
  vec <- c(meen, media, stand)
  names(vec) <- c("mean", "median", "sd")
  ret[[names(df[j[2]])]] <- vec
  ret$correlation_matrix <- cor(df[j])
  return(ret)
  
}



