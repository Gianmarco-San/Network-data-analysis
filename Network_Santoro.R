
# Network Data Analysis Project - Gianmarco Santoro ----

library(igraph)
library(igraphdata)
library(sbm)
library(ggplot2)


# let's load some network data
data(UKfaculty)

# Dataset References:
# "Nepusz T., Petroczi A., Negyessy L., Bazso F.: Fuzzy communities and the concept of 
# bridgeness in complex networks. 
# Physical Review E, 77:016107, 2008."

UKfaculty
# nodal attribute, Group (v/n): schools in which the member belong to
# edge attribute, weight (e/n)


# 1) Visualize Network ----

set.seed(23)  # to have fix nodes positions in the visualization

#layout <- layout_with_fr(UKfaculty, niter = 100000, area = 100000)  # to see better clusters
layout <- layout_with_kk(UKfaculty)  # to have low overlap between vertices

# Plot ----
plot(UKfaculty, 
     layout = layout, 
     vertex.size = 10, 
     vertex.label = NA,
     edge.size=20, 
     edge.width = 1, 
     edge.arrow.size = 0.3,
     main = "UK Faculty Network")

# With labels
plot(UKfaculty, 
     layout = layout, 
     vertex.size = 10, 
     edge.size=20, 
     edge.width = 1, 
     edge.arrow.size = 0.3,
     main = "UK Faculty Network")

# Changing color
plot(UKfaculty, 
     layout = layout, 
     vertex.size = 10, 
     edge.size=20, 
     edge.width = 1, 
     vertex.color = "lightblue", 
     edge.arrow.size = 0.3,
     main = "UK Faculty Network")

# With weighted edges ----
plot(UKfaculty, 
     layout = layout, 
     vertex.size = 10,
     edge.size=20, 
     edge.width = E(UKfaculty)$weight*0.2,
     vertex.color = "lightblue", 
     edge.arrow.size = 0.3,
     main = "UK Faculty Network",
     vertex.label.cex = 1.2, 
     asp = 0)


# let us represent the Group (school) where the node (student) belongs to
V(UKfaculty)$Group

# finding unique values of classes
unique_values_Group <- unique(V(UKfaculty)$Group)
num_unique_values_Group <- length(unique_values_Group)

# let us modify the color of the nodes based on an attribute
# define a discrete attribute with levels
Group_d = cut(V(UKfaculty)$Group, num_unique_values_Group)

# add this info to the igraph object
V(UKfaculty)$Group_d = Group_d
UKfaculty  # UKfaculty object in igraph has the new attribute

# most informative graph based on the available data
plot(UKfaculty, 
     layout = layout, 
     vertex.color=V(UKfaculty)$Group_d, 
     vertex.size = 10, 
     edge.size = 1, 
     edge.width = E(UKfaculty)$weight*0.2, 
     edge.arrow.size = 0.3,
     main = "UK Faculty Network",
     vertex.label.cex = 1.2, 
     asp = 0)


# 2) Network Properties ----

V(UKfaculty)
E(UKfaculty)

# list of vertices ----
vcount(UKfaculty)
# list of edges
ecount(UKfaculty)
# ----

is.directed(UKfaculty)

components(UKfaculty) # no = 1 since all nodes has at least one edge


# moving from directed to undirected network ----
UKfaculty.und1 = as.undirected(UKfaculty, mode = "each")
UKfaculty.und2 = as.undirected(UKfaculty, mode = "collapse")
UKfaculty.und3 = as.undirected(UKfaculty, mode = "mutual")

par(mfrow = c(2,2))

plot(UKfaculty, 
     layout = layout, 
     vertex.color=V(UKfaculty)$Group_d, 
     vertex.size = 10, 
     edge.size= 20,
     edge.width = 1,
     edge.arrow.size = 0.1,
     main = "Directed")

plot(UKfaculty.und1, 
     layout = layout, 
     vertex.color=V(UKfaculty)$Group_d, 
     vertex.size = 10, 
     edge.size= 20, 
     edge.width = 1,
     main = "Each")

plot(UKfaculty.und2, 
     layout = layout, 
     vertex.color=V(UKfaculty)$Group_d, 
     vertex.size = 10, 
     edge.size= 20,
     edge.width = 1,
     main = "Collapse")

plot(UKfaculty.und3, 
     layout = layout, 
     vertex.color=V(UKfaculty)$Group_d, 
     vertex.size = 10, 
     edge.size= 20,
     edge.width = 1,
     main = "Mutual")


# 3) Network Descriptive Analysis ----

# network density
graph.density(UKfaculty)  # it's a relative sparse graph
# the prob of observing a tie is quite low. 0 = empty graph

# dyads (does it make sense?)
dyad.census(UKfaculty)
# what can we say?  240 mutual, 337 single e 2663 null

# reciprocity (does it make sense?)
reciprocity(UKfaculty)
# R = 1 --  perfect reciprocity
# R = 0 --  no reciprocity
# what can we say?  There's a low tendency to reciprocity 240·2/817

# transitivity, clustering
transitivity(UKfaculty)  # not considering the directions of edges
# C = 1 --  perfect transitivity
# C = 0 --  no transitivity
# what can we say?  For about 50% dei casi, friend's friend are friend


# homophily and assortative coefficient
undirected_UKfaculty <- as.undirected(UKfaculty)  # collapse way
#plot(undirected_UKfaculty)

assortativity(undirected_UKfaculty, V(undirected_UKfaculty)$Group)# over the attribute Group
# r = 1 -- perfect homophily
# r = -1 -- perfect eterophily
# what can we say? There's a quite high tendency to homophily, so students of the same school
# tend to have friendship more
# It's a measure of correlation


# 4) Community Detection ----

# Perform community detection using the Louvain method
communities <- cluster_louvain(undirected_UKfaculty)

# Get the communities identified
memb <- membership(communities)

# Show the identified communities
memb

# Plot the graph, dividing it by communities
par(mfrow = c(1,2))
plot(undirected_UKfaculty, 
     layout = layout, 
     vertex.color = memb,
     vertex.size = 10, 
     edge.size= 20,  
     edge.width = 1, 
     edge.arrow.size = 0.3,
     main = "Communities",
     vertex.label.cex = 0.9, 
     asp = 0)

plot(UKfaculty, 
     layout = layout, 
     vertex.color=V(UKfaculty)$Group_d, 
     vertex.size = 10, 
     edge.size= 20,  
     edge.width = 1, 
     edge.arrow.size = 0.3,
     main = "Groups",
     vertex.label.cex = 0.9, 
     asp = 0)


# Print values as pairs ----
for (i in 1:min(length(memb), length(V(UKfaculty)$Group))) {
  cat("memb -", memb[i], ",", V(UKfaculty)$Group[i], "- Group\n")}


# 5a) Centrality - Directed ----
# what if the network is directed?

# 1) in- and out-degree centrality
# a node is central if it receives many relations
ideg = degree(UKfaculty, mode = "in")
ideg
# normalized version
ideg_norm = degree(UKfaculty, normalized = T, mode = "in")
ideg_norm

# which are the most central nodes? 
ord_ideg = order(ideg_norm, decreasing = T)
df_ideg = data.frame(Node =ord_ideg, 
                     Degree = ideg_norm[ord_ideg])


# a node is central if it sends many relations
odeg = degree(UKfaculty, mode = "out")
odeg
# normalized version
odeg_norm = degree(UKfaculty, normalized = T, mode = "out")
odeg_norm

# which are the most central nodes? 
ord_odeg = order(odeg_norm, decreasing = T)
df_odeg = data.frame(Node = ord_odeg, 
           Degree = odeg_norm[ord_odeg])


# 2) closeness centrality
# a node is central if it is close to many others - many paths pointing at it
iclo = closeness(UKfaculty, mode = "in")
iclo
# normalized version
iclo_norm = closeness(UKfaculty, normalized = T,  mode = "in")
iclo_norm

# a node is central if it is close to many others - many paths start from it
oclo = closeness(UKfaculty, mode = "out")
oclo
# normalized version
oclo_norm = closeness(UKfaculty, normalized = T,  mode = "out")
oclo_norm

# there is one isolated node, so geodesic distance is infinity --> node 11 has NaN
summary(oclo_norm)  # since it has no exiting edges, so it has no path trough it


# 3) betweeness centrality
# a node is central if it lies in between many others
bet = betweenness(UKfaculty)
bet
# normalized version
bet_norm = betweenness(UKfaculty, normalized = T)
bet_norm


# Visual
par(mfrow = c(1,2), mar = c(0,0,3,0))

plot(UKfaculty, 
     layout = layout, 
     vertex.color=V(UKfaculty)$Group_d, 
     vertex.size = ideg_norm*50,
     edge.size= 20,  
     edge.width = 1, 
     edge.arrow.size = 0.3,
     main = "In-Degree", 
     vertex.label.cex = 1.2, 
     asp = 0)

plot(UKfaculty, 
     layout = layout, 
     vertex.color=V(UKfaculty)$Group_d, 
     vertex.size = odeg_norm*50,
     edge.size= 20,  
     edge.width = 1, 
     edge.arrow.size = 0.3,
     main = "Out-Degree", 
     vertex.label.cex = 1.2, 
     asp = 0)


plot(UKfaculty,
     layout = layout,
     vertex.color=V(UKfaculty)$Group_d,
     vertex.size = iclo_norm*50, 
     edge.size= 20,  
     edge.width = 1, 
     edge.arrow.size = 0.3,
     main = "In-Closeness", 
     vertex.label.cex = 1.2, 
     asp = 0)

oclo_norm[is.nan(oclo_norm)] = 0
plot(UKfaculty,
     layout = layout,
     vertex.color=V(UKfaculty)$Group_d,
     vertex.size = oclo_norm*50, 
     edge.size= 20,  
     edge.width = 1, 
     edge.arrow.size = 0.3,
     main = "Out-Closeness", 
     vertex.label.cex = 1.2, 
     asp = 0)


par(mfrow = c(1,1), mar = c(0,0,3,0))
plot(UKfaculty, 
     layout = layout,
     vertex.color=V(UKfaculty)$Group_d,
     vertex.size = bet_norm*100, 
     edge.size= 20,  
     edge.width = 1, 
     edge.arrow.size = 0.3,
     main = "Betweenees", 
     vertex.label.cex = 1.2, 
     asp = 0)


# 5b) Centrality - Undirected ----

# list of vertices 
# V(undirected_UKfaculty) the same
# list of edges
E(undirected_UKfaculty)

# centrality analysis
# 1) degree centrality
# a node is central if it is connected to many others
deg = degree(undirected_UKfaculty)
deg  # vector

# normalized version
deg_norm = degree(undirected_UKfaculty, normalized = T) # normalized over the max possible: n-1
deg_norm

# which are the most central nodes?
ord_deg = order(deg_norm, decreasing = T)  # V(UKfaculty)[ord]

df_deg = data.frame(Node = ord_deg, 
          Degree_Norm = deg_norm[ord_deg])  # ordered df


# 2) closeness centrality
# a node is central if it is close to many others
clo = closeness(undirected_UKfaculty)
clo
# normalized version
clo_norm = closeness(undirected_UKfaculty, normalized = T)
clo_norm

# which are the most central nodes? 
ord_clo = order(clo_norm, decreasing = T)
df_clo = data.frame(Node = ord_clo, 
          Closeness = clo_norm[ord_clo])


# 3) betweeness centrality
# a node is central if it lies in between many others
bet = betweenness(undirected_UKfaculty)
bet
# normalized version
bet_norm = betweenness(undirected_UKfaculty, normalized = T)
bet_norm

# which are the most central nodes? 
ord_bet = order(bet_norm, decreasing = T)
df_bet = data.frame(Node = ord_bet, 
           Betweenness = bet_norm[ord_bet])
# if the node is deleted from the net, how it is impacted? 0 if no impact at all

# let us represent graphically centrality
par(mfrow = c(1,3), mar = c(0,0,3,0))

plot(UKfaculty, 
     layout = layout, 
     vertex.color=V(UKfaculty)$Group_d, 
     vertex.size = deg_norm*50,  # dimensions prop to deg
     edge.size= 20,  
     edge.width = 1, 
     edge.arrow.size = 0.3,
     main = "Degree", 
     vertex.label.cex = 1.2, 
     asp = 0)

# actor 11 has not a valid closeness centrality
# lets us fix it to 0
clo_norm[is.nan(clo_norm)] = 0

plot(UKfaculty,
     layout = layout,
     vertex.color=V(UKfaculty)$Group_d,
     vertex.size = clo_norm*50, 
     edge.size= 20,  
     edge.width = 1, 
     edge.arrow.size = 0.3,
     main = "Closeness", 
     vertex.label.cex = 1.2, 
     asp = 0)

plot(UKfaculty, 
     layout = layout,
     vertex.color=V(UKfaculty)$Group_d,
     vertex.size = bet_norm*200, 
     edge.size= 20,  
     edge.width = 1, 
     edge.arrow.size = 0.3,
     main = "Betweenees", 
     vertex.label.cex = 1.2, 
     asp = 0)

# what can we say?
# 37, 62, 52, 77, 38, 5, 58 are some of the most central (important) nodes in the network 
# besides the centrality measure we consider


# 6) Model ----

# move from graph to matrix representation
Yw = as.matrix(get.adjacency(UKfaculty, attr = "weight", sparse = FALSE))
Yw
Y = get.adjacency(UKfaculty, sparse = F)  # sparse = T to delete null terms
Y
# Or by binarizing the matrix
Y = (Yw != 0) * 1

# let us derive the graph
graph_u = graph_from_adjacency_matrix(Y, mode = "undirected")
graph_d = graph_from_adjacency_matrix(Y, mode = "directed")
graph_d_w = graph_from_adjacency_matrix(Yw, mode = "directed", weighted = TRUE, diag = FALSE)

plot(graph_u, 
     layout = layout, 
     vertex.size = 10, 
     edge.size= 20,
     vertex.color = "lightblue", 
     edge.arrow.size = 0.2,
     main = "Undirected", 
     vertex.label.cex = 1.2, 
     asp = 0)

plot(graph_d, 
     layout = layout, 
     vertex.size = 10, 
     edge.size= 20,
     vertex.color = "lightblue", 
     edge.arrow.size = 0.2,
     main = "Directed", 
     vertex.label.cex = 1.2, 
     asp = 0)

plot(graph_d_w, 
     layout = layout, 
     vertex.size = 10, 
     edge.size= 20,
     edge.width = E(graph_d_w)$weight*0.2, 
     vertex.color = "lightblue", 
     edge.arrow.size = 0.2,
     main = "Directed & Weighted", 
     vertex.label.cex = 1.2, 
     asp = 0)

# but let also represent the adjacency matrix graphically
plotMyMatrix(Y, dimLabels = list(row = 'Student', col = 'Student'))

plotMyMatrix(Yw, 
             dimLabels = list(row = 'Student', col = 'Student'))

# let us estimate a sbm 
#dev.off()
par(mfrow = c(1,1), mar = c(3,3,3,3))

sbm1 = estimateSimpleSBM(Y, 
                         "bernoulli", 
                         directed = TRUE,
                         dimLabels = 'Student', 
                         estimOptions = list(verbosity = 1))
# the plot reports the icl criterion for each choice of Q 
# and the different random starting solutions
# in red the optimal one for each Q

# let us look at the results
sbm1

## selected number of blocks
sbm1$nbBlocks

# prior block probabilities
sbm1$blockProp

## where nodes belong
sbm1$memberships

# connectivity parameters, shows the connection probabilities between blocks
round(sbm1$connectParam$mean,3)

## a clearer representation
plot(sbm1, type = "data")
# nodes are ordered wrt to the block they belong to and blocks are highlighted

plot(sbm1, type = "expected")
# fitted connection probabilities

plot(sbm1, type = "meso")
# fitted connection probabilities 

# info on all estimated model is given in 
sbm1$storedModels


# extension to valued relations
# ******************************** 
# Instead of considering the binary network student-student 
# we may consider the weighted network where the link between 
# two students is the weight of friendship.

# let us consider poisson distributed variables Y_ij
sbm2 = estimateSimpleSBM(Yw, 
                         "poisson",
                         directed = TRUE,
                         dimLabels = 'Student', 
                         estimOptions = list(verbosity = 1))

sbm2


## selected number of blocks
sbm2$nbBlocks

# prior block probabilities
sbm2$blockProp

# connectivity parameters
round(sbm2$connectParam$mean,3)

## where nodes belong
sbm2$memberships

# Let us graphically represent the data matrix 
## reordering rows and cols according to the estimated block in the SBM
plot(sbm2, type = "data", dimLabels = list(row = 'Student', col= 'Student'))

# or the average number of connections between students
plot(sbm2, type = "expected", dimLabels = list(row = 'Student', col= 'Student'))

# or 
plot(sbm2, type = "meso", dimLabels = list(row = 'Student', col= 'Student'))
