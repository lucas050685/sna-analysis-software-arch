pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","correlation","see",
             "ggraph","psych","nortest","rgl","car","ggside","tidyquant","olsrr",
             "jtools","ggstance","magick","cowplot","emojifont","beepr","Rcpp", "dplry")

options(rgl.debug = TRUE)

# pacotes <- c("equatiomatic")
# pacotes <- c("huxtable")
# pacotes <- c("dplry")
pacotes <- c("mgcv")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

data <- read.csv("data.csv")

# Converter dados$crit_<...>_z_score para numericos
data$crit_negocio <- as.numeric(data$crit_negocio)
data$crit_negocio_z_score <- as.numeric(sub(",", ".", data$crit_negocio_z_score, fixed = TRUE))
data$crit_trabalho <- as.numeric(data$crit_trabalho)
data$crit_trabalho_z_score <- as.numeric(sub(",", ".", data$crit_trabalho_z_score, fixed = TRUE))
names(data)[5] <- "servico"


# Imprimir o sumário
summary(data)

# Imprimir histograma para as variáveis dependentes
hist(
  data$crit_negocio, 
  main = "Distribuição da criticidade percebida para o negócio", 
  xlab = "criticidade percebida para o negócio em escala de 1 à 10",
  ylab = "Frequência")

hist(
  data$crit_trabalho,
  main = "Distribuição da criticidade percebida para o trabalho", 
  xlab = "criticidade percebida para o negócio em escala de 1 à 10",
  ylab = "Frequência")

hist(
  data$crit_negocio_z_score,
  main = "Distribuição da criticadade percebida para o negócio (z-score)",
  xlab = "criticidade percebida para o negócio em escala de 0 à 1",
  ylab = "Frequência")

hist(
  data$crit_trabalho_z_score,
  main = "Distribuição da criticadade percebida para o trabalho (z-score)",
  xlab = "criticidade percebida para o negócio em escala de 0 à 1",
  ylab = "Frequência")

# Qual o melhor modelo para estes tipos de distribuição

# Grau re correlação entre os dados
cor_business <- correlation(data[4], data[8:28])
cor_business <- as.data.frame(cor_business)
cor_business <- cor_business %>% filter(., p < 0.05)

cor_work <- correlation(data[7], data[8:28])
cor_work <- as.data.frame(cor_work)
cor_work <- cor_work %>% filter(., p < 0.05)

summary(cor_business)
summary(cor_work)


# Adicionando um label para os modularity_class
data <- data %>% mutate(modularity_class_label = case_when(
  modularity_class == 0 ~ "Tokenizer",
  modularity_class == 1 ~ "Payment processor",
  modularity_class == 2 ~ "Subscription connectors",
  modularity_class == 3 ~ "Recurrence",
  modularity_class == 4 ~ "Payment link",
  modularity_class == 5 ~ "Invoice processor",
  modularity_class == 6 ~ "Acquirer connectors and installments manager",
))

# Capturar o sumário de criticidade de negócio por módulo
summ_business_group <- data %>% 
  group_by(modularity_class_label) %>%
  summarize(mean = mean(crit_negocio_z_score),
            sum = sum(crit_negocio_z_score),
            sd = sd(crit_negocio_z_score)) %>%
  order_by(arrange(., desc(mean)))

# Capturar o sumário de criticidade de trabalho por módulo
summ_work_group <- data %>% 
  group_by(modularity_class_label) %>%
  summarize(mean = mean(crit_trabalho_z_score),
            sum = sum(crit_trabalho_z_score),
            sd = sd(crit_trabalho_z_score)) %>%
  order_by(arrange(., desc(mean)))


# Distribuição para criticidade de negócio com indegree e degree
ggplotly(
  ggplot(data, aes(x = indegree, y = crit_negocio_z_score)) +
  geom_point(color = "#39568CFF", size = 2.5)
)

ggplotly(
  ggplot(data, aes(x = degree, y = crit_negocio_z_score)) +
    geom_point(color = "#39568CFF", size = 2.5)
)

# Nota1: embora o indegree possua um P valor menor, é o degree que parece explicar melhor o comportamento da criticidade
# Nota2: Quando o degree é pequeno parece haver mais fatores influenciando na criticidade


# Distribuição para criticidade de trabalho com indegree e degree
ggplotly(
  ggplot(data, aes(x = degree, y = crit_trabalho_z_score)) +
    geom_point(color = "#39568CFF", size = 2.5)
)

# Nota3: quando falamos em criticidade de trabalho o indegree nem sequer aparece 
# com relação de significância, mas sim o degree. E assim como na criticidade de
# negócio é esta variável que parece explicar melhor o comportamento da criticidade


############################ Conclusões preliminares ##########################
# #1: Serviços com maior grau de criticidade tendem a se agrupar. Isso implica
#     dizer que quando a equipe consegue apontar com precisão, durante o processo
#     de desenho da arquitetura, um ou mais serviços críticos é possível inferir
#     que os serviços orbitantes também possuirão elevado grau de criticidade
#
# #2: O degree parece explicar de algum modo a criticidade observada pela equipe.
#     Essa explicação parece ser mais significativa quanto maior for o degree.
#     E de mesmo modo quanto maior o degree maior parece ser a criticidade observada
#     Todavia o degree pequeno não parece ter poder de explicação significativo
#
# #3: Uma hipotese: Um serviço altamente crítico pode estar sendo protegido por outro
#     neste caso o primeiro serviço materia conxão apenas com seu protetor. O protetor,
#     por sua vez faria o intercambio com os diversos outro serviços.
#
# #4: Existem ao menos 8 observações para um mesmo serviço, deste modo as correlações
#     preliminares são entre a criticidade presumida e os demais parâmetros. Mas
#     seria possível que a relação entre os parâmetros e a média da criticidade desse
#     resultados diferentes. Ou seja, a média seria uma observação única para cada
#     serviço.
#

################## Redução de dados a partir da média de criticidade ##############

data_reduced <- data %>% 
  group_by(servico) %>% 
  summarise(
    crit_negocio_media = mean(crit_negocio_z_score),
    crit_trabalho_media = mean(crit_trabalho_z_score),
    degree = mean(degree),
    strongcompnum = mean(strongcompnum)
    )

cor_business_by_mean <- correlation(data_reduced[2], data_reduced[4])
cor_business_by_mean <- as.data.frame(cor_business_by_mean)
cor_business_by_mean <- cor_business_by_mean %>% filter(., p < 0.05)

cor_work_by_mean <- correlation(data_reduced[3], data_reduced[4])
cor_work_by_mean <- as.data.frame(cor_work_by_mean)
cor_work_by_mean <- cor_business_by_mean %>% filter(., p < 0.05)

ggplotly(
  ggplot(data_reduced, aes(x = degree, y = crit_negocio_media)) +
    geom_point(color = "#39568CFF", size = 2.5)
)

ggplotly(
  ggplot(data_reduced, aes(x = degree, y = crit_trabalho_media)) +
    geom_point(color = "#39568CFF", size = 2.5)
)

############################ Conclusões preliminares 2 #########################
#
# #1 A significancia da correlação entre o degree e a criticidade aumenta muito
#    quando consideramos a média da criticidade para cada serviço, e não a observação
#    individual da criticidade presumida. Isso pode indicar que embora não haja uma
#    concordância entre os desenvolvedores sobre serviço com baixo degree a média 
#    pode acabar se tornando um melhor indicador
#
# #2 hipotese: O grafico de dispersão apresenta um formato cônico, isso pode indicar que a 
#    abordagem multinível pode ser uma possibilidade. Um dos fatores que a ser
#    levado em conta pode ser o agrupamento de classe (clusterização) do serviço,
#    ou seja, serviços mesmo que com um degree baixo podem estar sendo classificados
#    como críticos por estarem muito próximos de outros serviços com um degree alto
#
#
#

cor_business_by_mean_strong <- correlation(data_reduced[2], data_reduced[5])
cor_business_by_mean_strong <- as.data.frame(cor_business_by_mean_strong)

ggplotly(
  ggplot(data_reduced, aes(x = strongcompnum, y = crit_trabalho_media)) +
    geom_point(color = "#39568CFF", size = 2.5)
)

########################## Primeiros modelos ###################################

# Modelo linear para explicar a criticidade percebida pelo degree
modelo_test <- lm(formula = data$crit_negocio_z_score ~ data$degree, data = data)
summary(modelo_test)
anova(modelo_test)
export_summs(modelo_test, scale = F, digits = 4)

# Modelo linear para explicar a criticidade média pelo degree
modelo_test1 <- lm(formula = data_reduced$crit_negocio_media ~ data_reduced$degree, data = data_reduced)
summary(modelo_test1)
anova(modelo_test1)
export_summs(modelo_test1, scale= F, digits = 4)

# Modelo linear para explicar a criticidade média pelo degree e power (força dor vizinhos)
modelo_test2 <- lm(formula = data_reduced$crit_negocio_media ~ data_reduced$degree + data_reduced$strongcompnum, data = data_reduced)
summary(modelo_test2)
anova(modelo_test2)
aov(modelo_test2)
export_summs(modelo_test2, scale= F, digits = 4)

# Por alguma razão o comando abaixo não está plotando o gráfico
scatter3d(
  crit_negocio_media ~ degree + strongcompnum,
  data = data_reduced,
  surface = F,
  point.col = "#440154FF",
  axios.col = rep(x = "black", times =3),
)

data_reduced[2:5] %>%
  correlation(method = "pearson") %>%
  plot()

chart.Correlation(data_reduced[2:5], histogram = T)

############################ Conclusões preliminares 3 #########################
# 
# #1 Embora a correlação direta entre o power e a criticidade não tenha demonstrado
#    um p-valor menor que 5%. Ela se mostra significativa quando adicionado ao 
#    modelo linear. Principalmente se estamos falando da criticidade média.
#
# #2 O modelo de regressão linear simples parece ainda não ser o mais adequado, 
#    visto que o R2 ainda está abaixo de 30%, porém é um granho muito significativo
#    se comparado com o primeiro modelo


modelo_test4 <- lm(formula = data_reduced$crit_negocio_media ~ data_reduced$strongcompnum, data = data_reduced)
summary(modelo_test4)
anova(modelo_test4)
aov(modelo_test4)
export_summs(modelo_test4, scale= F, digits = 4)

############ Teste de aderência dos termos de erro a normalidade ###############
shapiro.test(modelo_test2$residuals) # Amostras pequenas
sf.test(modelo_test2$residuals) # Qualquer amostra

data_reduced %>% 
  mutate(residuos = modelo_test2$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(
    aes(y = ..density..),
    color = "grey50", 
    fill = "grey90", 
    bins = 30,
    alpha = 0.6
  ) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_test2$residuals),
                            sd = sd(modelo_test2$residuals)),
                aes(color = "Curva Normal Teórica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")

############################ Conclusões preliminares 4 #########################
# #1 Ambos os testes apresentam um p-value > 0.05, que mostra que a curva dos 
#    erros tem aderência a normalidade

############################ Transformação Box Cox #############################
# ime.usp.br/~abe/lista/pdfQWaCMboK68.pdf

lambda_crit_negocio <- powerTransform(data_reduced$crit_negocio_media)
lambda_crit_negocio
data_reduced$bc_crit_negocio_media <- (((data_reduced$crit_negocio_media ^ lambda_crit_negocio$lambda) - 1) / lambda_crit_negocio$lambda)

modelo_test3 <- lm(formula = bc_crit_negocio_media ~ degree + strongcompnum, data = data_reduced)
summary(modelo_test3)
anova(modelo_test3)
aov(modelo_test3)
export_summs(modelo_test3, scale= F, digits = 4)

shapiro.test(modelo_test3$residuals) # Amostras pequenas
sf.test(modelo_test3$residuals) # Qualquer amostra

############################ Conclusões preliminares 5 #########################
# #1 Houve perda no R2 após a transformação de box cox, portanto acho que esse não
#    deve ser o caminho mais adequado, além disso o teste shapiro-Francia apresentou
#    maior aderência a normalidade no modelo linear (em comparação com o transformado)
#
# #2 Embora o Shapiro Francia tenha caido o Teste Shapiro-Wilk teve um aumento
#    de 60% para 65%.

######## Tentar rodar um modelo com stepwise utilizando todo o dataset #########
# criar modelo reduzido 2 com todas as colunas importantes
modelo_test5 <- "lm(data = data, formula = crit_negocio_z_score ~ )"
modelo_test5_step <- step(modelo_test5, k = 3.841459)




###################################### Drafts ##################################

# Este metodo precisa do pacote equatiomatic, que está indisponível para esta versão do R
#
# extract_eq(modelo_test, use_coefs = T) %>%
#   kable() %>%
#   kable_styling(bootstrap_options = "striped", full_width = F, font_size = 28)



