#'Dijkstra's Algorithm 
#'
#'Shortest path between nodes in a graph
#'
#'@param graph A data frame 
#'@param init_value A number
#'@return A vector with distances
#'
#'@references \url{https://en.wikipedia.org/wiki/Dijkstras_algorithm}

dijkstra <- function(graph, init_value){
  if(!is.data.frame(graph)){stop("graph value is not a data frame")}
  if(!is.numeric(init_value)){stop("init value is not a number")}
  if(length(graph)!= 3){stop("Incorrect graph structure")}
  if(!all(names(graph) == c("v1","v2","w"))){stop("Incorrect graph structure")}
  if(init_value > max(graph$v1)){stop("There is not a node that big")}
  q <- vector()
  distance <- vector()
  prev <- vector()
  for(v in 1:max(graph$v1)){
    distance[v] <- Inf
    q[v] <- v
  }
  q <- q[!q%in%init_value]
  distance[init_value] <- 0
  u <- init_value
  distance[graph$v2[which(graph$v1 == init_value)]] <- graph$w[which(graph$v1 == init_value)]
  while(length(q)>1){
    u1 <- which(graph$w == min(graph$w[which(graph$v1 == u & graph$v2 %in% q) ]))
    u <- graph$v2[u1[which(graph$v1[u1] == u )]]
    g <- graph$v2[which(graph$v1 == u)]
    a <- g[g %in% q]
    for(i in a){
      if(is.infinite(distance[u])){distance[u]<- 0}
      alt <- distance[u] + graph$w[which(graph$v1 == u & graph$v2 == i)]
      if(alt < distance[i]){
        distance[i] <- alt
      }
    }
    q <- q[!q %in% u]
  }
  return(distance)
}














