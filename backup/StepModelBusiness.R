businessCriticalityDummies$closeness <- NULL
businessModelComplete <- lm(formula = criticalityMean ~ . 
                            -Id
                            -language_na 
                            -language_typescript
                            -language_postgres
                            -language_mySQL,
     data = businessCriticalityDummies)
summary(businessModelComplete)
export_summs(businessModelComplete, scale= F, digits = 4)

k <- qchisq(p = 0.05, df = 1, lower.tail = F)
businessStepModel <- step(businessModelComplete, k = k)
summary(businessStepModel)
export_summs(businessStepModel, scale= F, digits = 4)

shapiro.test(businessStepModel$residuals) # Amostras pequenas
sf.test(businessStepModel$residuals) # Qualquer amostra

businessModelData <- businessCriticalityDummies %>%
  select(Id, criticalityMean, eigenvector,  language_dynamo, language_javascript)

businessModelData$fitted <- businessStepModel$fitted.values
businessModelData$residuals <- businessStepModel$residuals
full_join
businessNodePredict <- full_join(
  x = nodes, 
  y = businessModelData, by = "Id"
)
businessNodePredict$fitted <- ifelse(is.na(businessNodePredict$fitted), 0, businessNodePredict$fitted)
businessNodePredict$residuals <- ifelse(is.na(businessNodePredict$residuals), 0, businessNodePredict$residuals)
businessNodePredict$criticalityMean <- ifelse(is.na(businessNodePredict$criticalityMean), 0, businessNodePredict$criticalityMean)

businessCriticalityMeanVector <- as.vector(
  businessNodePredict %>%
    select(criticalityMean)
)
businessCriticalityMeanVector <- as.numeric(businessCriticalityMeanVector$criticalityMean)
names(businessCriticalityMeanVector) <- businessNodePredict$Id
V(servicesNet)$criticalityMean <- businessCriticalityMeanVector
V(servicesNet)$criticalityMean
rm(businessCriticalityMeanVector)

businessCriticalityFittedVector <- as.vector(
  businessNodePredict %>%
    select(fitted)
)
businessCriticalityFittedVector <- as.numeric(businessCriticalityFittedVector$fitted)
names(businessCriticalityFittedVector) <- businessNodePredict$Id
V(servicesNet)$fitted <- businessCriticalityFittedVector
V(servicesNet)$fitted
rm(businessCriticalityFittedVector)


businessModelData <- businessNodePredict %>%
  select(Id, criticalityMean, fitted, eigenvector.x, language_dynamo, language_javascript) %>%
  filter(!is.na(language_dynamo))

businessModelData$eigenvector <- businessModelData$eigenvector.x
businessModelData$eigenvector.x <- NULL

sjPlot::plot_model(businessStepModel, type = "slope")

sjPlot::tab_model(businessStepModel)
