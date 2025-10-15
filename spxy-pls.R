#SPXY-based hyperparameter selection in partial least squares

spxy <- function(data, Ncal) {
  
  X <- as.matrix(data[, 1:701]) 
  y <- as.matrix(data[, 702])    
  
  dminmax <- numeric(Ncal)      
  M <- nrow(X)                  
  samples <- 1:M               
  
  
  Dx <- as.matrix(dist(X))
 
  Dy <- as.matrix(dist(y))
  
  Dxmax <- max(Dx)
  Dymax <- max(Dy)
  
  D <- Dx / Dxmax + Dy / Dymax
  
  
  maxD <- apply(D, 2, max)             
  index_row <- apply(D, 2, which.max)  
  index_column <- which.max(maxD)     
  
  
  m <- numeric(Ncal)
  m[1] <- index_row[index_column]    
  m[2] <- index_column                 
  
  
  for (i in 3:Ncal) {
    pool <- setdiff(samples, m[1:(i-1)])  
    
    dmin <- numeric(length(pool))         
    
    
    for (j in seq_along(pool)) {
      indexa <- pool[j]
      d <- D[indexa, m[1:(i-1)]]      
      dmin[j] <- min(d)                
    }
    
   
    index <- which.max(dmin)
    m[i] <- pool[index]
  }
  
 
  return(m)
}


results_list <- list()

ratios <- seq(0.4, 0.8, by = 0.1)


for (ratio in ratios) {
  
  
  selected_indices <- spxy(data, nrow(data))
  
  train_size <- round(ratio * nrow(data))
  
  train_indices <- selected_indices[1:train_size]
  
  dataTrain <- data[train_indices, ]     
  dataTest <- data[-train_indices, ]     
  
  trainControl <- trainControl(method = "LOOCV")
  
  pls_model <- train(y ~ ., data = dataTrain, method = "pls", 
                     trControl = trainControl, 
                     tuneGrid = expand.grid(ncomp = 1:k))
  
  print(pls_model)
  
  result <- pls_function(dataTrain, dataTest, pls_model$bestTune$ncomp)
  
  results_list[[as.character(ratio)]] <- result
  
}


