### Plots
# Loading analysis
source("Analysis.R")

seedNum <- 125
transparent <- "#00000000"

hist(criticalityPivoted$criticality,
     main = "Distribuição da criticidade percebida bruta", 
     xlab = "criticidade percebida para o negócio em escala de 1 à 10",
     ylab = "Frequência")

hist(criticalityNorm$criticality,
     main = "Distribuição da criticidade percebida normalizada", 
     xlab = "criticidade percebida para o negócio em escala de 0 à 1",
     ylab = "Frequência")

hist(criticalityReduced$criticalityMean,
     main = "Distribuição da criticidade média percebida", 
     xlab = "criticidade percebida para o negócio em escala de 0 à 1",
     ylab = "Frequência")

# Net plot
set.seed(seedNum)
plot(
  servicesNet, 
  vertex.size = V(servicesNet)$degree, 
  vertex.label.cex = .4, 
  vertex.label.color = transparent, 
  vertex.color = "tomato", 
  edge.arrow.size = 0.01
)

set.seed(seedNum)
plot(
  servicesNet, 
  vertex.size = tPlot(V(servicesNet)$criticalityMean), 
  vertex.label.cex = .4, 
  vertex.label.color = transparent, 
  vertex.color = "tomato", 
  edge.arrow.size = 0.01
)

set.seed(seedNum)
plot(
  servicesNet, 
  vertex.size = tPlot(V(servicesNet)$fitted), 
  vertex.label.cex = .4,
  vertex.label.color = transparent, 
  vertex.color = "purple", 
  edge.arrow.size = 0.01
)

# Correlation charts
{
  chart.Correlation(
    criticalityReduced %>% select(
      criticalityMean,
      degree,
      inDegree,
      outDegree,
      betweenness,
      closeness,
      eigenvector,
      power,
      cluster
    ),
    histogram = T
  )
  mtext("Correlação entre criticidade média e dados da rede", side=3, line=3)
}


{
  chart.Correlation(
    criticalityDummies %>% select(
      criticalityMean,
      language_dynamo,
      language_javascript,
      language_mySQL,
      language_postgres,
      language_typescript,
      language_na
    ),
    histogram = T
  )
  mtext("Correlação entre criticidade média e linguagens", side=3, line=3)
}

sjPlot::plot_model(stepModel, type = "slope")
sjPlot::tab_model(stepModel)

ggplotly(
  modelData %>%
    ggplot() +
    geom_smooth(
      aes(x = criticalityMean, y = fitted, color = "Modelo final"), 
      method = "lm",
      color = "#440154FF", 
      se = F, 
      size = .25,
      formula = y ~ splines::bs(x, df = 5)) +
    geom_smooth(
      aes(x = criticalityMean, y = criticalityMean), 
      method = "lm", 
      color = "gray70", 
      linetype = "longdash", 
      size = 0.25) +
    scale_color_manual("modelos:", values = c("440154FF")) +
    labs(x = "Criticidade média percebida", y = "Fitted Values") +
    theme(panel.background = element_rect("white"),
          panel.grid = element_line("grey95"),
          panel.border = element_rect(NA),
          legend.position = "bottom")
)
