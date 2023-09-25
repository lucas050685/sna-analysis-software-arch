source("Packages.R")
source("Functions.R")
source("SNA.R")

data <- read_csv("dev_responses.csv")
services <- read_csv("services.csv")

############################ Data selection ####################################
familiarityDataSet <- data %>% select(2:43)
businessCriticality <- data %>% select(44:85, )
workCriticality <- data %>% select(86:127)

services <- services %>% select(2:9, 11:12)

########################## Fixing columns names ################################
colnames(businessCriticality) <- gsub("\\..*", "", colnames(businessCriticality))
colnames(workCriticality) <- gsub("\\..*", "", colnames(workCriticality))

############################# Data normalization ###############################
############################ Min-Max normalization #############################
businessCriticalityNorm <- normObs(businessCriticality, 1:42)
workCriticalityNorm <- normObs(workCriticality, 1:42)

workCriticalityNorm <- workCriticalityNorm %>% 
  pivot_longer(colnames(workCriticalityNorm))

businessCriticalityNorm <- businessCriticalityNorm %>% 
  pivot_longer(colnames(businessCriticalityNorm))

cNames <- c("serviceName", "criticality")
colnames(workCriticalityNorm) <- c("serviceName", "criticality")
colnames(businessCriticalityNorm) <- c("serviceName", "criticality")
rm(cNames)

############ Data reducing to get mean, median, standart desviation ############
workCriticalityReduced <- workCriticalityNorm %>% 
  group_by(serviceName) %>% 
  summarise(
    criticalityMean = mean(criticality),
    criticalityMedian = median(criticality),
    criticalitySd = sd(criticality)
  )

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
businessCriticalityReduced$language <- as.factor(businessCriticalityReduced$language)
businessCriticalityReduced$framework <- as.factor(businessCriticalityReduced$framework)
businessCriticalityReduced$architecture <- as.factor(businessCriticalityReduced$architecture)
businessCriticalityReduced$status <- as.factor(businessCriticalityReduced$status)





workCriticalityReduced <- merge(
  x = workCriticalityReduced, 
  y = services[1:9], by = "serviceName"
)

workCriticalityReduced <- merge(
  x = workCriticalityReduced,
  y = nodes, by = "Id"
)

workCriticalityReduced$type <- as.factor(workCriticalityReduced$type)
workCriticalityReduced$domain <- as.factor(workCriticalityReduced$domain)
workCriticalityReduced$sub_domain <- as.factor(workCriticalityReduced$sub_domain)
workCriticalityReduced$language <- as.factor(workCriticalityReduced$language)
workCriticalityReduced$framework <- as.factor(workCriticalityReduced$framework)
workCriticalityReduced$architecture <- as.factor(workCriticalityReduced$architecture)
workCriticalityReduced$status <- as.factor(workCriticalityReduced$status)
