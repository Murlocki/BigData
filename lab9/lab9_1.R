rm(list=ls())
install.packages("igraph")
install.packages("network")
install.packages("sna")
install.packages("ndtv")

library(igraph)
g1<-graph(c(1,2,1,3,1,4,2,3,2,4, 3,4, 4,5, 5,6, 2,6),n=6)
g1

E(g1, P=NULL, path=NULL, directed=TRUE)

g<-graph.full(n=10, directed = FALSE, loops = FALSE)
g

edges <- c(1,2, 3,2, 2,4)
g2<-graph(edges, n=max(edges), directed=TRUE)
plot(g2)
#Вывод матрицы смежности
g2[]
#Проверка связности вершин
are_adjacent(g1,5,6)

#Матрица достижимости
distMatrix <- distances(g1, v=V(g1), to=V(g1), weights=NA)
distMatrix

g<-graph.empty()+vertices(letters[1:10],color="red") #Команда 1
g<-g+vertices(letters[11:20],color="blue") #Команда 2
g<-g+edges(sample(V(g),30,replace=TRUE),color="green") #Команда 3
plot(g, edge.arrow.size=.2,vertex.size=20) #Команда 4


edges <- c(1,2, 3,2, 2,4)
g<-graph(edges, n=max(edges), directed=TRUE)
vcount(g)
ecount(g)

neighbors(g, V(g)[1], mode = 1)

incident(g,V(g)[2], mode=c("all", "out", "in", "total"))

is.directed(g)
are.connected(g, V(g)[1], V(g)[3])
get.edgelist(g)


advice_data_frame <- read.table('http://sna.stanford.edu/sna_R_labs/data/Krack-High-Tec-edgelist-Advice.txt')
g <- graph_from_data_frame(advice_data_frame)
write.graph(g, file='my_graph.txt', format="edgelist")

er_graph<-erdos.renyi.game(100,2/100)
plot(er_graph,vertex.label=NA,vertex.size=3)
ws_graph<-watts.strogatz.game(1,100,4,0.05)
plot(ws_graph,layout=layout.circle,vertex.label=NA,vertex.size=3)
ba_graph<-barabasi.game(100)
plot(ba_graph,vertex.label=NA,vertex.size=3)

g <- make_ring(10) + make_full_graph(5)
coords <- layout_(g, as_star())
coords <- layout_(g, in_circle())
coords <- layout_(g, as_tree())
plot(g, layout = coords)

g<-graph.lattice(length=10,dim=1,nei=2, circular = TRUE)
plot(g,vertex.size=2,vertex.label=NA,layout=layout.kamada.kawai)

diameter(g1)
g1.all_simple<-all_simple_paths(g1, 2, 6)
g1.all_simple

all_shortest_paths(g1, 1, to = V(g1), mode = c("out", "all", "in"),weights = NULL)
