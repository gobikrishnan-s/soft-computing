library(igraph)
#graph_1 <- graph( edges=c('A','S','A','T','A','Z','S','F','S','R','S','O','T','L','Z','O','F','B','R','P','R','C','L','M','B','G','B','U','P','B','C','P','M','D','U','H','D','C','H','E','H','V','V','I','I','N'), directed=F)
graph_1 <- graph( edges=c('A','B','A','C','B','D','C','D','C','F','C','L','D','E','F','J','F','K','L','M','E','G','J','K','J','I','K','M','G','H','I','H'), directed=F)
#graph_1<-graph(edges = C('A','B','A','C','B','D','C','D','C','F','C','L','D','E','F','J','F','K','L','M','E','G','J','K','J','I','K','M','G','H','I','H'),directed = F)
#E(graph_1)$weight <- c(36,61,31,32,31,80,52,122,112,102,43,36,45,32,20,40)
E(graph_1)$weight <- 1
E(graph_1)$color <- 'black'
V(graph_1)$color <- 'orange'
found=FALSE
initial <- 'A'
goal <- 'H'

ids<-function(c_node,c_depth,max_depth,visited,path){
  if(c_node==goal){
    path<-c(path,c_node)
    print(path)
    return(list(TRUE,visited,path))
  }
  if(c_depth==max_depth){
    return(list(FALSE,visited,path))
  }
  path<-c(path,c_node)
  visited[c_node]=c_depth
  adj<-adjacent_vertices(graph_1,c_node)
  flag<-FALSE
  for(i in 1:length(adj[[1]])){
    next_node<-adj[[1]][i]$name
    next_depth<-c_depth+1
    if(next_node %in% names(visited)){
      if(visited[next_node]<=next_depth)
      next
    }
    values<-ids(next_node,next_depth,max_depth,visited,path)
    flag<-values[[1]][1]
    visited<-values[[2]]
    path<-values[[3]]
    if(flag){
      return(list(TRUE,visited,path))
    }
  }
  return(list(FALSE,visited,path))
}

max_depth<-10
flag<-FALSE
for(i in 1:max_depth){
  values<-ids(initial,1,i,c(),c())
  if(values[[1]][1]){
    print("depth is")
    print(i)
    print(values[[3]])
    break
  }
}

