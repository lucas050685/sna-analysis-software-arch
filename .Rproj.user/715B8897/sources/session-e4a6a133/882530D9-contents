#################### Analise de correlação entre variáveis #####################

# Variáveis quantitativas de alta correlação
# degree
# betweenness
# closeness
# eigenvector
# strong
# cluster

# Cluster não será incluido aqui por ser uma váriavel resultado de uma clusterização

businessModel1 <- businessCriticalityReduced %>% 
  lm(formula = criticalityMean ~ degree + betweenness + closeness + eigenvector + strong)
summary(businessModel1)
anova(businessModel1)
aov(businessModel1)
export_summs(businessModel1, scale= F, digits = 4)

businessModel2 <- businessCriticalityReduced %>% 
  lm(formula = criticalityMean ~ degree)
summary(businessModel2)
anova(businessModel2)
aov(businessModel2)
export_summs(businessModel2, scale= F, digits = 4)

# Comparação entre o modelo 1 e 2
export_summs(businessModel1, businessModel2, scale= F, digits = 4, model.names = c("Business 1", "..2"))

businessModel3 <- businessCriticalityReduced %>% 
  lm(formula = criticalityMean ~ betweenness)
summary(businessModel3)
anova(businessModel3)
aov(businessModel3)
export_summs(businessModel3, scale= F, digits = 4)

# Comparação entre o modelo 2 e 3
export_summs(businessModel2, businessModel3, scale= F, digits = 4, model.names = c("Business 2", "..3"))

businessModel4 <- businessCriticalityReduced %>% 
  lm(formula = criticalityMean ~ closeness)
summary(businessModel4)
anova(businessModel4)
aov(businessModel4)
export_summs(businessModel4, scale= F, digits = 4)

# Comparação entre o modelo 2 e 4
export_summs(businessModel2, businessModel4, scale= F, digits = 4, model.names = c("Business 2", "..4"))

businessModel5 <- businessCriticalityReduced %>% 
  lm(formula = criticalityMean ~ eigenvector)
summary(businessModel5)
anova(businessModel5)
aov(businessModel5)
export_summs(businessModel5, scale= F, digits = 4)

# Comparação entre o modelo 2 e 5
export_summs(businessModel2, businessModel5, scale= F, digits = 4, model.names = c("Business 2", "..5"))


businessModel6 <- businessCriticalityReduced %>% 
  lm(formula = criticalityMean ~ strong)
summary(businessModel6)
anova(businessModel6)
aov(businessModel6)
export_summs(businessModel6, scale= F, digits = 4)

# Comparação entre o modelo 2, 5 e 6
export_summs(
  businessModel2, businessModel5, businessModel6, 
  scale= F, digits = 4, model.names = c("Business 2", "..5", "..6"))

businessModel7 <- businessCriticalityReduced %>% 
  lm(formula = criticalityMean ~ degree + eigenvector)
summary(businessModel7)
anova(businessModel7)
aov(businessModel7)
export_summs(businessModel7, scale= F, digits = 4)

businessModel8 <- businessCriticalityReduced %>% 
  lm(formula = criticalityMean ~ degree + strong)
summary(businessModel8)
anova(businessModel8)
aov(businessModel8)
export_summs(businessModel8, scale= F, digits = 4)

businessModel9 <- businessCriticalityReduced %>% 
  lm(formula = criticalityMean ~ eigenvector + strong)
summary(businessModel9)
anova(businessModel9)
aov(businessModel9)
export_summs(businessModel9, scale= F, digits = 4)

# Comparação entre todos os modelos
export_summs(
  businessModel1, businessModel2, businessModel3, businessModel4, businessModel5, 
  businessModel6, businessModel7, businessModel8,  businessModel9,
  model.names = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
  scale= F, digits = 4)

# Melhor modelo 5

businessModelData <- businessCriticalityReduced %>%
  select(c("Id", "criticalityMean", "eigenvector"))

shapiro.test(businessModel5$residuals) # Amostras pequenas
sf.test(businessModel5$residuals) # Qualquer amostra

############### Transformação de Box Cox #######################################
businessCriticalityReduced <- createBoxCoxColumn(businessCriticalityReduced, "criticalityMean", "criticalityMeanTransformed")
businessModel10 <- businessCriticalityReduced %>% 
  lm(formula = criticalityMeanTransformed ~ eigenvector)
summary(businessModel10)
anova(businessModel10)
aov(businessModel10)
export_summs(businessModel10, scale= F, digits = 4)

# Comparação entre o modelo 5 e o modelo 10 (transformação de box cox)
export_summs(
  businessModel5, businessModel10,
  model.names = c("5", "10"),
  scale= F, digits = 4)

shapiro.test(businessModel10$residuals) # Amostras pequenas
sf.test(businessModel10$residuals) # Qualquer amostra

# O gráfico de disperssão apresenta um formato cônico indicando um possível multinível
# Há duas possibilidade aqui, dummyzar as variaveis "type -> isDatabase" e "language -> isTypescript"
businessCriticalityDummies <- businessCriticalityReduced %>% 
  select(c("Id", "criticalityMean", "eigenvector", "degree", "type", "language", "criticalityMeanTransformed"))
businessCriticalityDummies$isDatabase <- ifelse(!is.na(businessCriticalityDummies$type) & businessCriticalityDummies$type == "database", 1, 0)
businessCriticalityDummies$isTypescript <- ifelse(!is.na(businessCriticalityDummies$language) & businessCriticalityDummies$language == "typescript", 1, 0)

# Foi encontrada uma boa correlação entre a linguagem ser ou não Typescript e a criticidade de negócio percebida
businessModel11 <- businessCriticalityDummies %>% 
  lm(formula = criticalityMean ~ eigenvector + isTypescript)
summary(businessModel11)
anova(businessModel11)
aov(businessModel11)
export_summs(businessModel11, scale= F, digits = 4)

shapiro.test(businessModel11$residuals) # Amostras pequenas
sf.test(businessModel11$residuals) # Qualquer amostra

# Embora o modelo 11 tenha mostrado bons indicadores de correlação de variáveis e R2
# O distribuição dos resíduos não se aderiu a normalidade, ou seja, o modelo continua inviável
businessModel12 <- businessCriticalityDummies %>% 
  lm(formula = criticalityMeanTransformed ~ eigenvector + isTypescript)
summary(businessModel12)
anova(businessModel12)
aov(businessModel12)
export_summs(businessModel12, scale= F, digits = 4)

shapiro.test(businessModel12$residuals) # Amostras pequenas
sf.test(businessModel12$residuals) # Qualquer amostra

# Os modelos 11 e 12 passaram no teste dos residuais

############ Teste para modelo multinível ######################################
businessCriticalityTypescript <- businessCriticalityDummies %>%
  filter(isTypescript == 1)

businessCriticalityNoTypescript <- businessCriticalityDummies %>%
  filter(isTypescript == 0)

businessModel13.1 <- businessCriticalityTypescript %>% 
  lm(formula = criticalityMean ~ eigenvector)
summary(businessModel13.1)
anova(businessModel13.1)
aov(businessModel13.1)
export_summs(businessModel13.1, scale= F, digits = 4)

shapiro.test(businessModel13.1$residuals) # Amostras pequenas
sf.test(businessModel13.1$residuals) # Qualquer amostra

businessModel13.2 <- businessCriticalityTypescript %>% 
  lm(formula = criticalityMeanTransformed ~ eigenvector)
summary(businessModel13.2)
anova(businessModel13.2)
aov(businessModel13.2)
export_summs(businessModel13.2, scale= F, digits = 4)

shapiro.test(businessModel13.2$residuals) # Amostras pequenas
sf.test(businessModel13.2$residuals) # Qualquer amostra

# Comparando modelos 13.1 e 13.2
export_summs(
  businessModel13.1, businessModel13.2,
  model.names = c("13.1", "13.2"),
  scale= F, digits = 4)


businessModel13.3 <- businessCriticalityNoTypescript %>% 
  lm(formula = criticalityMean ~ eigenvector)
summary(businessModel13.3)
anova(businessModel13.3)
aov(businessModel13.3)
export_summs(businessModel13.3, scale= F, digits = 4)

shapiro.test(businessModel13.3$residuals) # Amostras pequenas
sf.test(businessModel13.3$residuals) # Qualquer amostra

businessModel13.4 <- businessCriticalityNoTypescript %>% 
  lm(formula = criticalityMeanTransformed ~ eigenvector)
summary(businessModel13.4)
anova(businessModel13.4)
aov(businessModel13.4)
export_summs(businessModel13.4, scale= F, digits = 4)

shapiro.test(businessModel13.4$residuals) # Amostras pequenas
sf.test(businessModel13.4$residuals) # Qualquer amostra

export_summs(
  businessModel11, businessModel12,
  businessModel13.1, businessModel13.2,
  businessModel13.3, businessModel13.4,
  model.names = c("11", "12", "13.1", "13.2", "13.3", "13,4"),
  scale= F, digits = 4)

################## Modelo final para a criticidade de negócio ##################
# Não há ganho de sginificancia após a transformação deste modo vamos permanecer
# com o modelo 11 que parece explicar melhor o comportamento da variável dependente
summary(businessModel11)
export_summs(businessModel11, scale= F, digits = 4, model.names = "Modelo 5")

# ŷ = 0.62882 + 0.34249 * eigenvector - 0.12893 * isTypescript

# É possível notar uma correlação entre o cluster e a criticidade de negócio 
# percebida. Isso seignifica dizer que, serviços mais críticos estão agrupados, ou
# mesmo que, um serviço crítico pode aumentar a criticidade de seus vizinhos.
# O degree matem uma corelação significativa com a criticidade, mas não leva em 
# conta a influência de seus vizinhos. Já a eigenvector (centralidade de autovetor)
# contempla o degree e a influência dos nós vizinhos, por isso parece ser a variável
# que melhor explica o comportamento da criticidade de negócio percebida.
# O tipo de linguagem também possui uma correlação significativa quando convertemos
# a liguagem typescript para uma dummy, ou seja, se aquele nó é ou não typescript.
# É importante observar que neste caso o mais provavel não é que a linguagem em si
# tenha uma influência direta sobre a criticidade do serviço. Mas, dentro do sistema
# observado e dentro do contexto da empresa, a adoção de typescript é algo recente: menos de 2 anos.
# Significa dizer que serviços que não são desenvolvidos em typescript são provavelmente mais antigos.
# Uma vez que a inclinação desta variável é negativa, podemos inferir que há uma relação
# inversa entre a presença do typescript e a criticidade. Se, baseados no contexto,
# adotarmos a hipotese de que o typescript esteja sugerindo serviços mais recentes, então 
# poderemos afimar que o tempo passado desde a primeira release do serviço deve ser
# significativo para explicar o comportamento da criticidade observada. Infelizmente
# este é um dado que não estava explicitamente disponível no momento da coleta de dados.

businessModelData$yhat <- businessModel11$fitted.values
businessModelData$residuals <- businessModel11$residuals
