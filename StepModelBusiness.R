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
