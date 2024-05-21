library(igraph)

g <- graph(c(1, 2, 1, 3, 1, 4, 2, 5, 2, 6, 3, 5, 3, 6, 4, 5, 4, 6))
g <- graph(c(1, 2, 1, 3, 2, 3),directed=FALSE)

plot(g)

colors <- greedy_vertex_coloring(g)

# Если граф может быть покрашен в два цвета
if(max(colors) == 2) {
  # Создаем фактор с группами
  groups <- factor(colors)
  
  # Создаем фрейм с группами
  frame <- data.frame(Group1 = which(groups == 1), Group2 = which(groups == 2))
  
  # Выводим результат
  print(frame)
  print(c("Group 1" = sum(groups == 1), "Group 2" = sum(groups == 2)))
} else {
  print("Graph cannot be colored with 2 colors")
}