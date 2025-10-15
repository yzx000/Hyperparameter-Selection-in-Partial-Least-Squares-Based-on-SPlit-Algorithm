#5、Monte Carlo-based hyperparameter selection in partial least squares

n_iterations <- 100  # 总共迭代的次数，即重复随机划分100次
train_ratio <- 0.8  # 训练集所占比例，80%的数据用于训练
results_list <- list()  # 用于保存每次迭代的单次结果，方便后续细致分析
final_results <- data.frame()  # 用于汇总所有迭代结果，便于统计分析和绘图

for (n in 1:n_iterations) {  # 循环进行100次实验，保证结果稳定可靠
  message("开始第 ", n, " 次迭代...")  # 输出当前迭代信息，便于跟踪程序进度
  
  # --- Step1：随机划分训练集和测试集 ---
  indices <- sample(1:nrow(data), size = round(train_ratio * nrow(data)))
  train_set <- data[indices, ]  # 取样训练集
  test_set <- data[-indices, ]  # 取样测试集
  
  # --- Step2：在训练集上使用留一交叉验证(LOOCV)选择PLS模型的最优主成分数 ---
  trainControl <- trainControl(method = "LOOCV")
  pls_model <- train(y ~ ., data = train_set, method = "pls",
                     trControl = trainControl, tuneGrid = expand.grid(ncomp = 1:k))
  
  best_ncomp <- pls_model$bestTune$ncomp  # 获取交叉验证选出的最佳主成分数
  
  # --- Step3：使用自定义pls_function在测试集上进行预测和性能评估 ---
  result <- pls_function(train_set, test_set, best_ncomp)
  
  # --- Step4：将本次结果转换成数据框，合并到最终结果中 ---
  result_df <- as.data.frame(result)
  final_results <- rbind(final_results, result_df)
  
  # 同时将结果存入列表，方便后续细粒度分析
  results_list[[as.character(n)]] <- result
}
