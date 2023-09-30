source("Packages.R")
source("Functions.R")

links <- read_csv("links.csv")
servicesNet <- graph.edgelist(as.matrix(links), directed = T)

V(servicesNet)$degree <- degree(servicesNet, mode = "all")
V(servicesNet)$inDegree <- degree(servicesNet, mode = "in")
V(servicesNet)$outDegree <- degree(servicesNet, mode = "out")
V(servicesNet)$betweenness <- betweenness(servicesNet, directed = T)
V(servicesNet)$closeness <- closeness(servicesNet)
V(servicesNet)$eigenvector <- evcent(servicesNet)$vector
V(servicesNet)$power <- power_centrality(servicesNet)
a <- power_centrality(servicesNet)
nodes <- as.data.frame(
  list(
    Id=names(V(servicesNet)), 
    degree = V(servicesNet)$degree,
    inDegree = V(servicesNet)$inDegree,
    outDegree = V(servicesNet)$outDegree,
    betweenness = V(servicesNet)$betweenness,
    closeness = V(servicesNet)$closeness,
    eigenvector = V(servicesNet)$eigenvector,
    power = V(servicesNet)$power
    ),
  stringsAsFactors=FALSE
)

servicesNetF <- graph.edgelist(as.matrix(links), directed = F)
V(servicesNetF)$cluster <- membership(cluster_louvain(servicesNetF))
nodes$cluster <- V(servicesNetF)$cluster

write.csv(nodes, "./artifacts/nodes.csv", row.names = T)
