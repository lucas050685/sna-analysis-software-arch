console.log <- function(...) {
  cat(paste(..., '\n'))  
}

################### Normalize observations by Min-Max scale ####################
normValue <- function(value, minValue, maxValue) {
  r <- (maxValue - minValue)
  if (r == 0){
    return (1)
  }
  v <- (value - minValue) / r
  return(v)
}

normObs <- function(dataSet, columnRange) {
  console.log("Nomalizing...")
  
  data <- dataSet[columnRange]
  n = nrow(data)
  
  nDataFrame = data.frame(matrix(nrow = 0, ncol = ncol(data)))
  colnames(nDataFrame) <- colnames(data)
  
  for(i in 1:n){
    line <- data %>% slice(i)
    
    line <- unname(unlist(line[1,]))
    minValue <- min(line)
    maxValue <- max(line)
    
    line <- unlist(lapply(line, FUN = normValue,minValue=minValue,maxValue=maxValue))
    line <- data.frame(t(line))
    colnames(line) <- colnames(nDataFrame)
    nDataFrame <- rbind(nDataFrame, line)
  }
  return (nDataFrame)
}


############################# get Neighbors ids ################################
getNeighbors <- function (g, vIndex) {
  n <- as.matrix(neighbors(g, vIndex, mode = "all"))
  return (
    n[!duplicated(n)]
  )
}

getStrongByIndex <- function (g, vIndex, mode = "all") {
  strong <- 0
  n <- getNeighbors(g, vIndex)
  for(i in 1:length(n)) {
    s <- as.numeric(degree(g, v = n[i], mode = mode))
    strong <- s + strong
  }
  return (strong)
}

getStrong <- function(g, mode = "all") {
  len <- vcount(g)
  vec <- 1:len
  for(i in 1:len) {
    vec[i] <- getStrongByIndex(g, vIndex = i, mode = mode)
  }
  return (vec)
}

createBoxCoxColumn <- function(data, column, newColumnName = "boxCoxTransformed") {
  original <- data[column]
  lambdaObj <- powerTransform(original)
  l <- lambdaObj$lambda
  transformed <- ((original ^ l) - 1) / l
  data[newColumnName] <- transformed[column]
  return(data)
}

tPlot <- function(x) {
  lambdaPlot <- 2  
  factor <- 3
  return ( (x*factor) ^lambdaPlot)
}
