#Kennard–Stone-based hyperparameter selection in partial least squares


ks <- function(X) {
  Mx <- nrow(X)  
  Nx <- ncol(X)  
  
  Rank <- numeric(Mx)  
  out <- 1:Mx           
  D <- distli(X)        
  ind <- which(D == max(D), arr.ind = TRUE)
  i <- ind[1, 1]  
  j <- ind[1, 2]  
  
  Rank[1] <- i    
  Rank[2] <- j    
  out <- out[!out %in% c(i, j)]  
  
  iter <- 3
  while (iter <= Mx) {
    inRank <- Rank[Rank > 0]  
    
    Dsub <- D[inRank, out, drop = FALSE]
    
    minD <- apply(Dsub, 2, min)
    
    maxD <- which.max(minD)
    Vadd <- out[maxD]
    
    Rank[iter] <- Vadd
    out <- out[out != Vadd]
    
    iter <- iter + 1
  }
  
  return(Rank)
}




results_list <- list()


set.seed(123)
for (proportion in seq(0.2, 0.8, by = 0.1)) {
  
  train_size <- floor(nrow(data) * proportion)
  test_size <- nrow(data) - train_size  
  
  ksresult <- ks(as.matrix(data[, -which(names(data) == "y")])) 
  
  train_indices <- ksresult[1:train_size]  
  test_indices <- ksresult[(train_size + 1):(train_size + test_size)]  
  
  train_data <- data[train_indices, ]
  test_data <- data[test_indices, ]
  
  trainControl <- trainControl(method = "LOOCV")
  
  pls_model <- train(
    y ~ ., data = train_data,
    method = "pls",
    trControl = trainControl,
    tuneGrid = expand.grid(ncomp = 1:k)
  )
  
 
  m <- pls_model$bestTune$ncomp
  
  result <- pls_function(train_data, test_data, m)
  
  results_list[[as.character(proportion)]] <- result
}

print(results_list)


