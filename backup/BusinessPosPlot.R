######################## BusinessPosPlot #######################################
businessModelData %>% 
  select(criticalityMean, eigenvector, yhat, residuals) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 22)

ggplotly(
  ggplot(businessModelData, aes(x = eigenvector , y = criticalityMean)) +
    geom_point(color = "#39568CFF", size =.5) +
    geom_smooth(
      aes(color = "Fitted Values"),
      method = "lm", formula = y ~ x, se = F, size = 1
    ) +
    labs(x = "Centralidade de autovetor", y = "Criticidade de negócio") +
    scale_color_manual("Legenda:", values = c("#55C667FF", "grey50", "#440154FF")) +
    theme_classic()
)

{
  chart.Correlation(
    select(businessCriticalityDummies, c("criticalityMean", "criticalityMeanTransformed", "eigenvector", "degree", "isDatabase", "isTypescript")),
    histogram = T
  )
  mtext("Correlação entre criticidade de negócio, autovetor e tipo de nó", side=3, line=3)
}

{
  chart.Correlation(
    select(businessCriticalityTypescript, c("criticalityMean", "criticalityMeanTransformed", "eigenvector", "degree", "isDatabase")),
    histogram = T
  )
  mtext("Correlação entre criticidade de negócio, autovetor e tipo de nó (isTypescript)", side=3, line=3)
}

ggplotly(
  businessModelData %>%
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
    labs(x = "Criticidade de negócio", y = "Fitted Values") +
    theme(panel.background = element_rect("white"),
          panel.grid = element_line("grey95"),
          panel.border = element_rect(NA),
          legend.position = "bottom")
)


seedNum <- 125
############################## grafo pelo grau
set.seed(seedNum)
plot(
  servicesNet, 
  vertex.size = V(servicesNet)$degree, 
  vertex.label.cex = .4, 
  vertex.label.color = "black", 
  vertex.color = "tomato", 
  edge.arrow.size = 0.01
)


############################# grafo pela criticidade
transparent <- "#00000000"
set.seed(seedNum)
plot(
  servicesNet, 
  vertex.size = V(servicesNet)$degree, 
  vertex.label.cex = .4, 
  vertex.label.color = transparent, 
  vertex.color = "tomato", 
  edge.arrow.size = 0.01
)


tPlot <- function(x) {
  lambdaPlot <- 2  
  factor <- 3
  return ( (x*factor) ^lambdaPlot)
}



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
