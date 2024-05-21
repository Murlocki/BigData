install.packages("igraph")
install.packages("network")
install.packages("sna")
install.packages("ndtv")

library(igraph)
#1
vertext_number = sample(18:(8/10+5)**2+5*8,1)
vertext_number
g<-graph.star(n=vertext_number, mode="in")
plot(g)

vcount(g)
ecount(g)
options(max.print = 100000,width=50)
g[]

#2
g1<-graph.empty()+vertices(1:vertext_number,color="yellow")
g1<-g1+edges(sample(V(g1),18,replace=TRUE),color="red")
g1<-g1+edges(sample(V(g1),80,replace=TRUE),color="blue")
plot(g1)
g1[]
as_adjacency_matrix(g1)

#3
vert_to_check = c(2*8+23,2*8+20,2*8+12,8+15,2*8-1,8+8,2*8,2*8+1,8+7,8+13)
vert_to_check %in% V(g1)
g1 <- g1+edges(vert_to_check,color="black")
plot(g1)
E(g1)

#Все соседи
neighbors(g1,8)
#Ребра инциндентные вершине
incident(g1,8, mode=c("all", "out", "in", "total"))
#Проверка соединения
are_adjacent(g1, 18,20)| are_adjacent(g1, 20,18)
#Матрица смежности
g1[]


#4
deg <- degree(g1, mode="all")
max_degree_vertex = V(g1)[which.max(deg)]
max_degree_vertex

g1<-g1+vertices(vertext_number+1,color="green")
g1<-g1+edges(c(vertext_number+1,max_degree_vertex),color="green")
plot(g1)
vertext_number = vertext_number+1

#Присваиваем имена
labels = paste(rep("m",vertext_number),1:vertext_number,sep="")
V(g1)$name = labels
plot(g1)
g1[]

selected_vertices <- V(g1)[degree(g1) > 2 & degree(g1) < 5]
selected_vertices

#5
coords <- layout_(g1, as_star())
plot(g1, layout = coords)

coords <- layout_(g1, in_circle())
plot(g1, layout = coords)


coords <- layout_(g1, as_tree())
plot(g1, layout = coords)

install.packages("igraph")
library(igraph)
plot(g1, layout = layout.fruchterman.reingold(g1))


#6
diameter(g1)

f = function(vert){all_shortest_paths(g1, vert, V(g1),weights = NULL)}
lapply(V(g1), f)

plot(g1,vertex.size=degree(g1, mode="all"))
