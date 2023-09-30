source("Packages.R")
source("Functions.R")

links <- read_csv("links.csv")
servicesNet <- graph.edgelist(as.matrix(links), directed = F)

V(servicesNet)$degree <- degree(servicesNet, mode = "all")
V(servicesNet)$inDegree <- degree(servicesNet, mode = "in")
V(servicesNet)$outDegree <- degree(servicesNet, mode = "out")
V(servicesNet)$betweenness <- betweenness(servicesNet, directed = F)
V(servicesNet)$closeness <- closeness(servicesNet)
V(servicesNet)$eigenvector <- evcent(servicesNet)$vector
V(servicesNet)$power <- power_centrality(servicesNet)
# V(servicesNet)$strength <- strength(servicesNet, mode = "all")
# V(servicesNet)$strong <- getStrong(servicesNet)
V(servicesNet)$cluster <- membership(cluster_louvain(servicesNet))

e <- evcent(servicesNet)
e$value

nodes <- as.data.frame(
  list(
    Id=names(V(servicesNet)), 
    degree = V(servicesNet)$degree,
    inDegree = V(servicesNet)$inDegree,
    outDegree = V(servicesNet)$outDegree,
    betweenness = V(servicesNet)$betweenness,
    closeness = V(servicesNet)$closeness,
    eigenvector = V(servicesNet)$eigenvector,
    power = V(servicesNet)$power,
    #    strength = V(servicesNet)$strength,
    #    strong = V(servicesNet)$strong,
    cluster = as.matrix(V(servicesNet)$cluster)
  ),
  stringsAsFactors=FALSE
)

write.csv(nodes, "./artifacts/nodes_f.csv", row.names = T)
