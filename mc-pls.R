#Monte Carlo-based hyperparameter selection in partial least squares

n_iterations <- 100  
train_ratio <- 0.8  
results_list <- list()  
final_results <- data.frame()  

for (n in 1:n_iterations) {  
  message("开始第 ", n, " 次迭代...")  
  
  indices <- sample(1:nrow(data), size = round(train_ratio * nrow(data)))
  train_set <- data[indices, ]  
  test_set <- data[-indices, ]  
  
  trainControl <- trainControl(method = "LOOCV")
  pls_model <- train(y ~ ., data = train_set, method = "pls",
                     trControl = trainControl, tuneGrid = expand.grid(ncomp = 1:k))
  
  best_ncomp <- pls_model$bestTune$ncomp  
  
  result <- pls_function(train_set, test_set, best_ncomp)
  
  result_df <- as.data.frame(result)
  final_results <- rbind(final_results, result_df)
  
  results_list[[as.character(n)]] <- result
}


