# bidouilles
library(igraph)
  
structuration(synthetic, 100, 10) -> babu
babug <- make_graph(as.vector(rbind(1:length(babu), babu)))
plot(babug, vertex.size = 3, edge.arrow.size = 0.1, vertex.label = "")
components(babug)$no
mean(components(babug)$csize)
mean(unlist(lapply(decompose(babug), diameter)))
mean(unlist(lapply(decompose(babug), average.path.length)))

kapo <- get.knn(scale(synthetic), k = 1)
kapo <- as.vector(kapo$nn.index)
kapog <- make_graph(as.vector(rbind(1:length(kapo), kapo)))
plot(kapog, vertex.size = 3, edge.arrow.size = 0.1, vertex.label = "")
components(kapog)$no
mean(components(kapog)$csize)
mean(unlist(lapply(decompose(kapog), diameter)))
mean(unlist(lapply(decompose(kapog), average.path.length)))


print("# joris housing")
gjh <- make_graph(as.vector(rbind(1:372, results_joris)))
print("# nb composantes")
components(gjh)$no
print("# taille moyenne composantes")
mean(components(gjh)$csize)
print("# diametre moyen composantes")
mean(unlist(lapply(decompose(gjh), diameter)))
print("# longueur chemin moyen composantes")
mean(unlist(lapply(decompose(gjh), average.path.length)))

print("# scaled housing")
gsh <- make_graph(as.vector(rbind(1:372, results_scaled)))
print("# nb composantes")
components(gsh)$no
print("# taille moyenne composantes")
mean(components(gsh)$csize)
print("# diametre moyen composantes")
mean(unlist(lapply(decompose(gsh), diameter)))
print("# longueur chemin moyen composantes")
mean(unlist(lapply(decompose(gsh), average.path.length)))

# create the plot linking elements to their exemplar
library(ggpubr)
library(ggplot2)
data(iris)
structuration(iris[,1:4], 50, 10) -> repk
size <- 150

d <- data.frame(x = rep(0, size), y = rep(0, size), ex = rep(0, size), ey = rep(0, size))
d$x <- iris$Sepal.Length
d$y <- iris$Sepal.Width

for(j in 1:size){
  d$ex[j] <- d$x[repk[j]]
}

for(j in 1:size){
  d$ey[j] <- d$y[repk[j]]
}

op <- ggplot() + geom_segment(d, mapping = aes(x = x, y = y, xend = ex, yend = ey)) + geom_point(iris, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + labs(x = "Sepal Length", y = "Sepal Width")
op + theme_pubr()
op + theme_pubr() + grids(linetype = "dashed")
op + theme_pubr() + grids(linetype = "solid")

# generate extracted connected component
l1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
l2 <- c(3, 3, 9, 6, 6, 9, 9, 9, 3)

tmpg <- make_graph(as.vector(rbind(l1, l2)))
plot(tmpg, vertex.size = 15, edge.arrow.size = 0.8, edge.arrow.length = 0.1, label.cex = 30)

# compute similarities between an element and its exemplar
detailing(data, 124, 10, 309, 300) -> test
sum(test)
sum(test != 0)

res <- numeric(372)
for (i in 1:372) {
  res[i] <- sum(detailing(data, 124, 10, 261, i) != 0)
}