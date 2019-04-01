library(dplyr)
require(RSQLite)
library(devtools)
library(graphTweets)
library(twinetverse)
library(igraph)
library(zeallot)
library(visNetwork) 

#KEY HASHTAGS
db <- src_sqlite("/Users/wayne/Dropbox/Acer Laptop Sync/Data Science/PH_Election_Tracker/phelectiontracker_hashtags.sqlite", create = FALSE)

OtsoDiretso <- tbl(db, sql("SELECT * FROM tweets WHERE query LIKE '%OtsoDiretso%'"))
OtsoDiretso <- collect(OtsoDiretso)
OtsoDiretso$created_at <- as.Date(OtsoDiretso$created_at)
OtsoDiretso <- OtsoDiretso[!duplicated(OtsoDiretso$tweet_id),]

Imeesolusyon <- tbl(db, sql("SELECT * FROM tweets WHERE query LIKE '%IMEEsolusyon%'"))
Imeesolusyon <- collect(Imeesolusyon)
Imeesolusyon$created_at <- as.Date(Imeesolusyon$created_at)
Imeesolusyon <- Imeesolusyon[!duplicated(Imeesolusyon$tweet_id),]

extract_rt <- function(df){
  df <- df[df$retweeted_status =="THIS IS A RETWEET --> DOUBLE-CHECK JSON",]
  df$retweet_screen_name <- lapply(df$entities_mentions, function(x) unlist(strsplit(x, split=", "))[1])
  
  rt <- df %>% 
    gt_edges(source = from_user_screen_name, target = retweet_screen_name) %>% # get edges
    gt_nodes() %>% # get nodes
    gt_collect() # collect
  
  return(rt)
}

extract_mt <- function(df){

  df <- df[df$entities_mentions !="" & df$retweeted_status !="THIS IS A RETWEET --> DOUBLE-CHECK JSON",]
  df$from_user_screen_name <- as.character(df$from_user_screen_name)
  df$mentions_screen_names <- lapply(df$entities_mentions, function(x) unlist(strsplit(x, split=", ")))
  
  mt <- df %>% 
    gt_edges(source = from_user_screen_name, target = mentions_screen_names) %>% # get edges
    gt_nodes() %>% # get nodes
    gt_collect() # collect
  
  return(mt)
}

nodes <- function(net){
  
  c(edges, nodes) %<-% net
  nodes$id <- as.factor(nodes$nodes) 
  nodes$size <- nodes$n 

  nodes <- nodes2sg(nodes)
  nodes <- nodes[,2:5]
  
  return(nodes)
}

edges <- function(net){
  
  c(edges, nodes) %<-% net
  edges$id <- seq(1, nrow(edges))
  edges <- edges2sg(edges)
  
  return(edges)
}

subnet_in <- function(edgelist, nodes){
  
  edgelist <- edgelist[,c("source","target","size")]
  nodes <- nodes[,c("label","id","size")]
  n1 <- graph_from_data_frame(d=edgelist, vertices=nodes, directed=T) 
  n1 <- set_edge_attr(n1, "weight", value= edgelist$size)
  #n1 <- set_vertex_attr(n1, "size", value= nodes$size)
  
  ceb <- cluster_edge_betweenness(n1,weights = E(n1)$weight) 
  V(n1)$group <- ceb$membership
  V(n1)$bt <- betweenness(n1, directed=T, weights=NA)
  #V(n1)$degree <- degree(n1,mode = "all")
  V(n1)$indegree <- degree(n1,mode = "in")
  V(n1)$size <- degree(n1,mode = "in")
  #V(n1)$coreness <- coreness(n1, mode="all")
  
  #kcore <- coreness(n1, mode="all")
  #bt <- betweenness(n1, directed=T, weights=NA)
  #degree <- degree(n1,mode = "all")
  indegree <- degree(n1,mode = "in")
  #k <- induced_subgraph(n1, kcore>=k)
  in_rank <- sort(degree(n1,mode = "in"),decreasing = TRUE)
  rank <- as.numeric(in_rank[20])
  k <- induced_subgraph(n1, indegree>=rank)
  V(k)$size <- (V(k)$size)/4
  
  return(k)
}

subnet_out <- function(edgelist, nodes){
  
  edgelist <- edgelist[,c("source","target","size")]
  nodes <- nodes[,c("label","id","size")]
  n1 <- graph_from_data_frame(d=edgelist, vertices=nodes, directed=T) 
  n1 <- set_edge_attr(n1, "weight", value= edgelist$size)
  #n1 <- set_vertex_attr(n1, "size", value= nodes$size)
  
  ceb <- cluster_edge_betweenness(n1,weights = E(n1)$weight) 
  V(n1)$group <- ceb$membership
  V(n1)$outdegree <- degree(n1,mode = "out")
  V(n1)$size <- degree(n1,mode = "out")
  
  outdegree <- degree(n1,mode = "out")
  
  #k <- induced_subgraph(n1, kcore>=k)
  out_rank <- sort(degree(n1,mode = "out"),decreasing = TRUE)
  rank <- as.numeric(out_rank[20])
  k <- induced_subgraph(n1, outdegree>=rank)
  V(k)$size <- (V(k)$size)/4
  
  return(k)
}

subnet_kcore <- function(edgelist, nodes,k){
  
  edgelist <- edgelist[,c("source","target","size")]
  nodes <- nodes[,c("label","id","size")]
  n1 <- graph_from_data_frame(d=edgelist, vertices=nodes, directed=T) 
  n1 <- set_edge_attr(n1, "weight", value= edgelist$size)
  #n1 <- set_vertex_attr(n1, "size", value= nodes$size)
  
  ceb <- cluster_edge_betweenness(n1,weights = E(n1)$weight) 
  V(n1)$group <- ceb$membership
 # V(n1)$bt <- betweenness(n1, directed=T, weights=NA)
  V(n1)$coreness <- coreness(n1, mode="all")
  
  kcore <- coreness(n1, mode="all")
  k <- induced_subgraph(n1, kcore>=k)
  #V(k)$size <- (V(k)$size)/2
  
  return(k)
}



OD_rt <- extract_rt(OtsoDiretso)
OD_mt <- extract_mt(OtsoDiretso)
IM_rt <- extract_rt(Imeesolusyon)
IM_mt <- extract_mt(Imeesolusyon)

nodes_OD_rt <- nodes(OD_rt)
edges_OD_rt <- edges(OD_rt)
nodes_OD_mt <- nodes(OD_mt)
edges_OD_mt <- edges(OD_mt)

nodes_IM_rt <- nodes(IM_rt)
edges_IM_rt <- edges(IM_rt)
nodes_IM_mt <- nodes(IM_mt)
edges_IM_mt <- edges(IM_mt)

rt_OD_in <- subnet_in(edges_OD_rt, nodes_OD_rt)
rt_OD_out <- subnet_out(edges_OD_rt, nodes_OD_rt)
mt_OD_in <- subnet_in(edges_OD_mt, nodes_OD_mt)
mt_OD_out <- subnet_out(edges_OD_mt, nodes_OD_mt)

rt_IM_in <- subnet_in(edges_IM_rt, nodes_IM_rt)
rt_IM_out <- subnet_out(edges_IM_rt, nodes_IM_rt)
mt_IM_in <- subnet_in(edges_IM_mt, nodes_IM_mt)
mt_IM_out <- subnet_out(edges_IM_mt, nodes_IM_mt)

visIgraph(rt_IM_in,idToLabel = TRUE,layout = "layout_nicely") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) 

visIgraph(mt_IM_out,idToLabel = TRUE,layout = "layout_nicely") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) 

save(rt_OD_in, file="rt_OD_in.RData")
save(rt_OD_out, file="rt_OD_out.RData")
save(mt_OD_out, file="mt_OD_out.RData")
save(mt_OD_in, file="mt_OD_in.RData")
save(rt_IM_out, file="rt_IM_out.RData")
save(rt_IM_in, file="rt_IM_in.RData")
save(mt_IM_out, file="mt_IM_out.RData")
save(mt_IM_in, file="mt_IM_in.RData")

getwd()
save(rt_OD_in,rt_OD_out, mt_OD_in,mt_OD_out,rt_IM_in,rt_IM_out, mt_IM_in,mt_IM_out, file = "/Users/wayne/Dropbox/Acer Laptop Sync/Data Science/PH_Election_Tracker/PH_tracker/PH_TwNetViz/mydata.rda")

