#SPlit-based hyperparameter selection in partial least squares (SS-PLS)

library(SPlit)     
library(caret)      
library(pls)        


results_list_100 <- list()

split_counts <- list(100)

run_cross_validation <- function(data, nSplits) {
  results <- list()  
  for (i in 1:nSplits) {
    SPlitIndices <- SPlit(data, tolerance = 1e-6, nThreads = 2)  
    dataTest <- data[SPlitIndices, ]  
    dataTrain <- data[-SPlitIndices, ] 
    
    trainControl <- trainControl(method = "LOOCV")  
    
    pls_model <- train(
      y ~ ., data = dataTrain,
      method = "pls",                    
      trControl = trainControl,          
      tuneGrid = expand.grid(ncomp = 1:k)  
    )
    
    
    best_ncomp <- pls_model$bestTune$ncomp
    
    result <- pls_function(dataTrain, dataTest, best_ncomp)
    
    results[[i]] <- result
  }
  
  return(results)
}
results_list_10 <- run_cross_validation(data, split_counts[[1]])


