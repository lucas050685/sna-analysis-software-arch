#################### Analise de correlação entre variáveis #####################

# Variáveis quantitativas de alta correlação
# degree
# betweenness
# closeness
# eigenvector
# cluster

# Cluster não será incluido aqui por ser uma váriavel resultado de uma clusterização

workModel1 <- workCriticalityReduced %>% 
  lm(formula = criticalityMean ~ degree + betweenness + closeness + eigenvector + strong)
summary(workModel1)
anova(workModel1)
aov(workModel1)
export_summs(workModel1, scale= F, digits = 4)

workModel2 <- workCriticalityReduced %>% 
  lm(formula = criticalityMean ~ degree)
summary(workModel2)
anova(workModel2)
aov(workModel2)
export_summs(workModel2, scale= F, digits = 4)

# Comparação entre o modelo 1 e 2
export_summs(workModel1, workModel2, scale= F, digits = 4, model.names = c("Business 1", "..2"))

workModel3 <- workCriticalityReduced %>% 
  lm(formula = criticalityMean ~ betweenness)
summary(workModel3)
anova(workModel3)
aov(workModel3)
export_summs(workModel3, scale= F, digits = 4)

# Comparação entre o modelo 2 e 3
export_summs(workModel2, workModel3, scale= F, digits = 4, model.names = c("Business 2", "..3"))

workModel4 <- workCriticalityReduced %>% 
  lm(formula = criticalityMean ~ closeness)
summary(workModel4)
anova(workModel4)
aov(workModel4)
export_summs(workModel4, scale= F, digits = 4)

# Comparação entre o modelo 2 e 4
export_summs(workModel2, workModel4, scale= F, digits = 4, model.names = c("Business 2", "..4"))

workModel5 <- workCriticalityReduced %>% 
  lm(formula = criticalityMean ~ eigenvector)
summary(workModel5)
anova(workModel5)
aov(workModel5)
export_summs(workModel5, scale= F, digits = 4)

# Comparação entre o modelo 2 e 5
export_summs(workModel2, workModel5, scale= F, digits = 4, model.names = c("Business 2", "..5"))


workModel6 <- workCriticalityReduced %>% 
  lm(formula = criticalityMean ~ strong)
summary(workModel6)
anova(workModel6)
aov(workModel6)
export_summs(workModel6, scale= F, digits = 4)

workModel7 <- workCriticalityReduced %>% 
  lm(formula = criticalityMean ~ degree + eigenvector)
summary(workModel7)
anova(workModel7)
aov(workModel7)
export_summs(workModel7, scale= F, digits = 4)

workModel8 <- workCriticalityReduced %>% 
  lm(formula = criticalityMean ~ degree + strong)
summary(workModel8)
anova(workModel8)
aov(workModel8)
export_summs(workModel8, scale= F, digits = 4)

# Comparação entre o modelo 2 e 8
export_summs(
  workModel2, workModel8, 
  model.names = c("2", "8"),
  scale= F, digits = 4)

# Comparação entre todos os modelos
export_summs(
  workModel1, workModel2, workModel3, workModel4, workModel5, 
  workModel6, workModel7, workModel8,
  model.names = c("1", "2", "3", "4", "5", "6", "7", "8"),
  scale= F, digits = 4)

# Melhor modelo 2

workModelData <- workCriticalityReduced %>%
  select(c("Id", "criticalityMean", "degree"))

shapiro.test(workModel2$residuals) # Amostras pequenas
sf.test(workModel2$residuals) # Qualquer amostra
# Infelizmente a distribuição dos resíduos do melhor modelo não é aderente a normalidade, 
# ou seja, o modelo não possui capacidade preditiva e por isso será descartado.

############### Transformação de Box Cox #######################################
workCriticalityReduced <- createBoxCoxColumn(workCriticalityReduced, "criticalityMean", "criticalityMeanTransformed")
workModel9 <- workCriticalityReduced %>% 
  lm(formula = criticalityMeanTransformed ~ degree)
summary(workModel9)
anova(workModel9)
aov(workModel9)
export_summs(workModel9, scale= F, digits = 4)

# Comparação entre o modelo 5 e o modelo 10 (transformação de box cox)
export_summs(
  workModel2, workModel9,
  model.names = c("2", "9"),
  scale= F, digits = 4)

shapiro.test(workModel9$residuals) # Amostras pequenas
sf.test(workModel9$residuals) # Qualquer amostra

################## Modelo final para a criticidade de negócio ##################
# Não há ganho de sginificancia após a transformação, deste modo não foi possível 
# gerar modelo que apontasse a criticidade de trabalho

