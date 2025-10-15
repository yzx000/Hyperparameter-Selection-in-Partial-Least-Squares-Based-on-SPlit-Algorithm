#Bootstrap-based hyperparameter selection in partial least squares

set.seed(123)  

results_list <- list()  

n_sampling <- 100  


for (i in 1:n_sampling) {
  cat("正在进行第", i, "次抽样...\n") 
  
 
  train_indices <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
  
  train_data <- data[train_indices, ]        
  test_data <- data[-train_indices, ]        
  
  trainControl <- trainControl(method = "LOOCV")
  
  pls_model <- train(y ~ ., data = train_data, method = "pls",
                     trControl = trainControl,
                     tuneGrid = expand.grid(ncomp = 1:k))
  
  best_ncomp <- pls_model$bestTune$ncomp
  
  result <- pls_function(train_data, test_data, best_ncomp)
  
  results_list[[i]] <- result
}

