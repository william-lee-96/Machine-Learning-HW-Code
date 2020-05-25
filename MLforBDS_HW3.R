# William Lee
# Code for ML HW #3

bdir = "~/Desktop/machine_learning_code"
setwd(bdir)

# 2a)
problem2_data = "assignment3_problem2_data.txt"
problem2_df = read.table(header=F, quote = "", sep="\t", fill=T, file = problem2_data, stringsAsFactors=FALSE)

colnames(problem2_df) <- c("A","B")

combo = c(problem2_df$A, problem2_df$B)
length(unique(combo))

# ANSWER: There are 130 unique nodes.

# 2b)
library(igraph)

gph_edge = graph.edgelist(as.matrix(problem2_df), directed=FALSE)
adj_igph = as_adjacency_matrix(gph_edge)
adj_matx = as.matrix(adj_igph)

heatmap(adj_matx, Rowv=NA, Colv=NA, col = c("white","black"), scale="none")

# ANSWER: This heatmap represents a concentration (precision) matrix.

# 2c.i)
adj_df = as.data.frame(adj_matx)

deg_vec = c()
for (row in adj_df) {
  deg_vec <- append(deg_vec, sum(row))
}

hist(deg_vec,
     main="Degree Distribution",
     xlab="Node degree", ylab = "Node count",
     col = "dodgerblue4",
     breaks = seq(0,11,l=12)
     )


# ANSWER: The degree distribution is very right-skewed. Most nodes have low degrees (81/130 nodes have a degree of 1),
# but a few nodes (hubs) have high degrees.

# 2c.ii) 
nodes = rownames(adj_df)
temp = as.data.frame(cbind(nodes, deg_vec))

top5 = (temp[order(-deg_vec),])[1:5,]
colnames(top5) <- c("node","degree")

install.packages("kableExtra")
library(kableExtra)

table2cii = kable(top5,"html",align="l") %>% 
  kable_styling("striped",full_width = FALSE)

table2cii

# 2c.iii)
temp$deg_vec = as.numeric(as.character(temp$deg_vec))
sum(temp$deg_vec)

# ANSWER: The sum of degrees of all the nodes is 274, and there are 130 nodes. From this, we can see that 
# for each node in this network, there are ~2 edges.

# 3a)
A <- c(1,2,1,3,4,5,6,6,5,7)
B <- c(2,4,3,4,5,6,7,8,7,8)

AtoB = cbind(A,B)
AtoB_df = as.data.frame(AtoB)

gph_obj = graph.edgelist(as.matrix(AtoB_df), directed=FALSE)
adj_obj = as_adjacency_matrix(gph_obj)
adjacency = as.matrix(adj_obj); adjacency 

# 3b)
adjacency_df = as.data.frame(adjacency)

deg_vec = c()
for (row in adjacency_df) {
  deg_vec <- append(deg_vec, sum(row))
}

install.packages("optimbase")
library("optimbase")

deg_mat = zeros(8,8)

i = 0; j = 0
for (element in deg_vec) {
  i = i + 1
  j = j + 1
  
  deg_mat[i,j] = element
  
}

laplacian = deg_mat - adjacency
laplacian

# 3c) 
(edge_betweenness(graph.edgelist(as.matrix(AtoB_df)), directed = FALSE, weights = NULL))[5]

# 3d)
(centr_degree(graph.edgelist(as.matrix(AtoB_df)))$res)[4:6]

# 3e)
(centr_betw(graph.edgelist(as.matrix(AtoB_df)), directed = FALSE)$res)[4:6]

# 4a)
mods = cluster_edge_betweenness(graph.edgelist(as.matrix(AtoB_df), directed=FALSE)) 
mods

# 4b)
bicolor = c("red", "green")
mod_color = bicolor[mods$membership]
plot.igraph(graph.edgelist(as.matrix(AtoB_df), directed=FALSE),
            vertex.color = mod_color,
            main = "Topology of Problem 3 Undirected Graph")

# 5a)
# vector variable used to rename columns 
class_vect = c("bw_float","bw_non_float","vw_float",
               "containers","tableware","headlamps")

# k = 2
c1 = c(4,20,1,13,9,28)
c2 = c(66,56,16,0,0,1)

k2_em_df = rbind(c1,c2)
colnames(k2_em_df) = class_vect

k2_em_df = as.data.frame(k2_em_df)

k2_em_df$entropy = NA
k2_em_df$purity = NA

k2_em_df = entropyPurity_calc(k2_em_df)

# k = 4
c1 = c(4,15,0,8,1,2)
c2 = c(0,0,0,4,4,26)
c3 = c(23,25,5,0,0,0)
c4 = c(43,36,12,1,4,1)

k4_em_df = rbind(c1,c2,c3,c4)
colnames(k4_em_df) = class_vect

k4_em_df = as.data.frame(k4_em_df)

k4_em_df$entropy = NA
k4_em_df$purity = NA

k4_em_df = entropyPurity_calc(k4_em_df)

# k = 6
c1 = c(0,0,0,3,1,21)
c2 = c(4,8,1,0,0,1)
c3 = c(0,11,0,9,3,0)
c4 = c(0,4,0,1,1,7)
c5 = c(20,2,5,0,4,0)
c6 = c(46,51,11,0,0,0)

k6_em_df = rbind(c1,c2,c3,c4,c5,c6)
colnames(k6_em_df) = class_vect

k6_em_df = as.data.frame(k6_em_df)

k6_em_df$entropy = NA
k6_em_df$purity = NA

k6_em_df = entropyPurity_calc(k6_em_df)

# 6a)
install.packages("dbscan")
library("dbscan")
data(DS3)
plot(DS3, pch=20, cex=0.25)

clst1 <- dbscan(DS3, eps=0.1, minPts=1); clst1
clst2 <- dbscan(DS3, eps=0.1, minPts=2); clst2
clst3 <- dbscan(DS3, eps=0.1, minPts=4); clst3
clst4 <- dbscan(DS3, eps=0.25, minPts=1); clst4
clst5 <- dbscan(DS3, eps=0.25, minPts=2); clst5
clst6 <- dbscan(DS3, eps=0.5, minPts=2); clst6

# 6b)
plot(DS3, pch=20, cex=0.25, col = clst1$cluster + 1)
plot(DS3, pch=20, cex=0.25, col = clst2$cluster + 1)
plot(DS3, pch=20, cex=0.25, col = clst3$cluster + 1)
plot(DS3, pch=20, cex=0.25, col = clst4$cluster + 1)
plot(DS3, pch=20, cex=0.25, col = clst5$cluster + 1)
plot(DS3, pch=20, cex=0.25, col = clst6$cluster + 1)
