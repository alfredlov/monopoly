library(DiagrammeR)
library(visNetwork)

from1 <- rep("1", 7)
from2 <- rep("2", 4)
from3 <- rep("3", 8)
from4 <- rep("4", 6)
from5 <- rep("5", 5)
from6 <- rep("6", 1)
from7 <- rep("7", 0)
from8 <- rep("8", 1)
from9 <- rep("9", 4)
from10 <- rep("10", 6)
from11 <- rep("11", 3)

from <- c(from1, from2, from3, from4, from5, from6, from7, from8, from9, from10, from11)

to1<-c("2","4","6","7","8","10","11")
to2<-c("6","7","8","11")
to3<-c("2","4","5","6","7","8","9","11")
to4<-c("2", "11","5","6","7","8")
to5<-c("6","7","8","9","11")
to6<-c("7")
to7<-c()
to8<-c("6")
to9<-c("6","7","8","11")
to10<-c("2","6","7","8","9","11")
to11<-c("6","7","8")
to<-c(to1, to2, to3, to4, to5, to6, to7, to8, to9, to10, to11)


nodesd <- unique(c(to, from))
nodes <- create_node_df(n=length(nodesd), label=nodesd,  width=7.8) 
edges <- create_edge_df(from = factor(from, levels=nodesd), to = factor(to, levels=nodesd), rel = "leading_to")   
graph <- create_graph(nodes_df = nodes, edges_df = edges, attr_theme = "theme_dark")
render_graph(graph)
# 
# 
# 
# # Create a simple edge data frame (edf) and
# # view the results
# edf <-
#   create_edge_df(
#     from = c(1, 2, 3),
#     to = c(4, 3, 1),
#     rel = "a")
# edf <- create_graph(nodes_df = nodes, edges_df = edf)
# 
# render_graph(edf, "visNetwork")
# 
