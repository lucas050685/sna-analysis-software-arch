## Analisys

# Loading Packages
source("Packages.R")
source("Functions.R")

# Loading Data
links <- read_csv("links.csv")

services <- read_csv("services.csv")
services <- services %>% select(2:9, 11:12)

devResponses <- read_csv("dev_responses.csv")
criticalityData <- devResponses %>% select(44:85, )
colnames(criticalityData) <- gsub("\\..*", "", colnames(criticalityData))

criticalityPivoted <- criticalityData %>%
  pivot_longer(colnames(criticalityData))

criticalityNorm <- normObs(criticalityData, 1:42)
criticalityNorm <- criticalityNorm %>% 
  pivot_longer(colnames(criticalityNorm))

cNames <- c("serviceName", "criticality")
colnames(criticalityNorm) <- cNames
colnames(criticalityPivoted) <- cNames
rm(cNames)

criticalityReduced <- criticalityNorm %>%
  group_by(serviceName) %>%
  summarise(
    criticalityMean = mean(criticality),
    criticalityMedian = median(criticality),
    criticalitySd = sd(criticality)
  )

criticalityReduced <- merge(
  x = criticalityReduced, 
  y = services[1:9], by = "serviceName"
)

criticalityReduced$type <- as.factor(criticalityReduced$type)
criticalityReduced$domain <- as.factor(criticalityReduced$domain)
criticalityReduced$sub_domain <- as.factor(criticalityReduced$sub_domain)
criticalityReduced$language <- ifelse(is.na(criticalityReduced$language), "na", criticalityReduced$language)
criticalityReduced$language <- as.factor(criticalityReduced$language)
criticalityReduced$framework <- as.factor(criticalityReduced$framework)
criticalityReduced$architecture <- as.factor(criticalityReduced$architecture)

# SNA analisys
servicesNet <- graph.edgelist(as.matrix(links), directed = T)

V(servicesNet)$degree <- degree(servicesNet, mode = "all")
V(servicesNet)$inDegree <- degree(servicesNet, mode = "in")
V(servicesNet)$outDegree <- degree(servicesNet, mode = "out")
V(servicesNet)$betweenness <- betweenness(servicesNet, directed = T)
V(servicesNet)$closeness <- closeness(servicesNet)
V(servicesNet)$eigenvector <- evcent(servicesNet)$vector
V(servicesNet)$power <- power_centrality(servicesNet)

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

servicesNetUndirected <- graph.edgelist(as.matrix(links), directed = F)
V(servicesNetUndirected)$cluster <- membership(cluster_louvain(servicesNetUndirected))
nodes$cluster <- V(servicesNetUndirected)$cluster

# Final data frame for analysis and regretion models
criticalityReduced <- merge(
  x = criticalityReduced, 
  y = nodes, by = "Id"
)

# Dummy process
criticalityDummies <- criticalityReduced %>% 
  select(c(
    "Id", 
    "criticalityMean", 
    "degree",
    "betweenness",
    "closeness",
    "eigenvector", 
    "power", 
    "language", 
  ))

criticalityDummies <- dummy_columns(.data = criticalityDummies, 
                                            select_columns = "language",
                                            remove_selected_columns = T)
# Removing missing values
criticalityDummies$closeness <- NULL

# Full model with all main variables
fullModel <- lm(formula = criticalityMean ~ . 
                            -Id
                            -language_na 
                            -language_typescript
                            -language_postgres
                            -language_mySQL,
                            data = criticalityDummies)
summary(fullModel)

# Step model with a ste-wise process
k <- qchisq(p = 0.05, df = 1, lower.tail = F)
stepModel <- step(fullModel, k = k)
summary(stepModel)

# Datasets from final model
modelData <- criticalityDummies %>%
  select(Id, criticalityMean, eigenvector,  language_dynamo, language_javascript)
modelData$fitted <- stepModel$fitted.values
modelData$residuals <- stepModel$residuals

nodePredict <- full_join(
  x = nodes, 
  y = modelData, by = "Id"
)
nodePredict$fitted <- ifelse(is.na(nodePredict$fitted), 0, nodePredict$fitted)
nodePredict$residuals <- ifelse(is.na(nodePredict$residuals), 0, nodePredict$residuals)
nodePredict$criticalityMean <- ifelse(is.na(nodePredict$criticalityMean), 0, nodePredict$criticalityMean)

# Adding criticity to net
criticalityMeanVector <- as.vector(
  nodePredict %>%
    select(criticalityMean)
)
criticalityMeanVector <- as.numeric(criticalityMeanVector$criticalityMean)
names(criticalityMeanVector) <- nodePredict$Id
V(servicesNet)$criticalityMean <- criticalityMeanVector
rm(criticalityMeanVector)

# adding fitted values to net
criticalityFittedVector <- as.vector(
  nodePredict %>%
    select(fitted)
)
criticalityFittedVector <- as.numeric(criticalityFittedVector$fitted)
names(criticalityFittedVector) <- nodePredict$Id
V(servicesNet)$fitted <- criticalityFittedVector
rm(criticalityFittedVector)
