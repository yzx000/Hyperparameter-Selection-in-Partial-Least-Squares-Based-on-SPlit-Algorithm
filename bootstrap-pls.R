#4、Bootstrap-based hyperparameter selection in partial least squares

set.seed(123)  # 固定随机种子，保证实验可重复性

results_list <- list()  # 初始化结果存储列表

n_sampling <- 100  # 设置Bootstrap抽样次数为100次

# 进行Bootstrap采样循环
for (i in 1:n_sampling) {
  cat("正在进行第", i, "次抽样...\n")  # 打印当前采样进度
  
  # Bootstrap采样：有放回随机抽样生成训练集索引
  train_indices <- sample(1:nrow(data), size = nrow(data), replace = TRUE)
  
  # 构造训练集和测试集
  train_data <- data[train_indices, ]        # Bootstrap训练集
  test_data <- data[-train_indices, ]        # 剩余样本作为测试集
  
  # 交叉验证参数设置：采用留一交叉验证（LOOCV）
  trainControl <- trainControl(method = "LOOCV")
  
  # 使用caret包中的train函数训练PLS模型，并通过LOOCV确定最佳成分数（ncomp）
  pls_model <- train(y ~ ., data = train_data, method = "pls",
                     trControl = trainControl,
                     tuneGrid = expand.grid(ncomp = 1:k))
  
  # 提取最优成分数
  best_ncomp <- pls_model$bestTune$ncomp
  
  # 调用自定义pls_function进行训练集拟合及测试集预测，并计算模型评估指标
  result <- pls_function(train_data, test_data, best_ncomp)
  
  # 将当前Bootstrap采样的结果保存到列表中
  results_list[[i]] <- result
}
