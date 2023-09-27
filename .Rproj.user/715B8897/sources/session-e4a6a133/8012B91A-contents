source("Packages.R")
source("Functions.R")
source("SNA.R")

data <- read_csv("dev_responses.csv")
services <- read_csv("services.csv")

############################ Data selection ####################################
familiarityDataSet <- data %>% select(2:43)
businessCriticality <- data %>% select(44:85, )

services <- services %>% select(2:9, 11:12)

########################## Fixing columns names ################################
colnames(businessCriticality) <- gsub("\\..*", "", colnames(businessCriticality))

############################# Data normalization ###############################
############################ Min-Max normalization #############################
businessCriticalityNorm <- normObs(businessCriticality, 1:42)

businessCriticalityNorm <- businessCriticalityNorm %>% 
  pivot_longer(colnames(businessCriticalityNorm))

businessCriticalityPivoted <- businessCriticality %>%
  pivot_longer(colnames(businessCriticality))

cNames <- c("serviceName", "criticality")
colnames(businessCriticalityNorm) <- cNames
colnames(businessCriticalityPivoted) <- cNames
rm(cNames)

############ Data reducing to get mean, median, standart desviation ############
businessCriticalityReduced <- businessCriticalityNorm %>%
  group_by(serviceName) %>%
  summarise(
    criticalityMean = mean(criticality),
    criticalityMedian = median(criticality),
    criticalitySd = sd(criticality)
  )

############################ Merge services information ########################
businessCriticalityReduced <- merge(
  x = businessCriticalityReduced, 
  y = services[1:9], by = "serviceName"
)

businessCriticalityReduced <- merge(
  x = businessCriticalityReduced, 
  y = nodes, by = "Id"
)

businessCriticalityReduced$type <- as.factor(businessCriticalityReduced$type)
businessCriticalityReduced$domain <- as.factor(businessCriticalityReduced$domain)
businessCriticalityReduced$sub_domain <- as.factor(businessCriticalityReduced$sub_domain)
businessCriticalityReduced$language <- ifelse(is.na(businessCriticalityReduced$language), "na", businessCriticalityReduced$language)
businessCriticalityReduced$language <- as.factor(businessCriticalityReduced$language)
businessCriticalityReduced$framework <- as.factor(businessCriticalityReduced$framework)
businessCriticalityReduced$architecture <- as.factor(businessCriticalityReduced$architecture)


businessCriticalityDummies <- businessCriticalityReduced %>% 
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

businessCriticalityDummies <- dummy_columns(.data = businessCriticalityDummies, 
                                            select_columns = "language",
                                            remove_selected_columns = T)
