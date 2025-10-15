#2、Kennard–Stone-based hyperparameter selection in partial least squares

# 定义 K-S 样本重排序函数
# 输入：X 为一个 n × p 的数据矩阵（n 为样本数，p 为变量数）
# 输出：返回一个长度为 n 的索引向量，表示样本按“分布最大差异性”排序的顺序
ks <- function(X) {
  Mx <- nrow(X)  # 样本数量
  Nx <- ncol(X)  # 变量数量
  
  # 初始化样本排序向量和输出索引
  Rank <- numeric(Mx)  # 最终样本排序索引（顺序记录）
  out <- 1:Mx           # 当前未被选入排序的样本索引集合
  
  # 计算样本之间的两两欧式距离矩阵
  D <- distli(X)        # 这里假设 distli 是一个自定义函数，返回完整的距离矩阵（n × n）
  
  # 找到距离最远的两个样本作为起始点（种子点）
  ind <- which(D == max(D), arr.ind = TRUE)
  i <- ind[1, 1]  # 第一个点
  j <- ind[1, 2]  # 第二个点
  
  Rank[1] <- i    # 将第一个点加入排序序列
  Rank[2] <- j    # 将第二个点加入排序序列
  out <- out[!out %in% c(i, j)]  # 从未选样本集合中移除这两个点
  
  # 开始迭代添加样本：每次选择离已排序样本集合“最远”的样本
  iter <- 3
  while (iter <= Mx) {
    inRank <- Rank[Rank > 0]  # 当前已选中的样本索引
    
    # 提取未排序样本与已排序样本之间的子距离矩阵
    Dsub <- D[inRank, out, drop = FALSE]
    
    # 计算每个待选样本与当前排序集中样本的最小距离
    minD <- apply(Dsub, 2, min)
    
    # 找到最小距离最大的样本（即与当前排序样本最远的样本）
    maxD <- which.max(minD)
    Vadd <- out[maxD]
    
    # 将该样本加入排序中，并从未选集中移除
    Rank[iter] <- Vadd
    out <- out[out != Vadd]
    
    iter <- iter + 1
  }
  
  # 返回最终排序结果
  return(Rank)
}




# 初始化结果列表，用于存储每种比例下的建模评估结果
results_list <- list()

# 设置随机种子以确保实验具有可重复性
set.seed(123)

# 遍历一系列训练集比例（从0.2到0.8，步长为0.1）
for (proportion in seq(0.2, 0.8, by = 0.1)) {
  
  # 根据比例计算训练集的样本数量
  train_size <- floor(nrow(data) * proportion)
  test_size <- nrow(data) - train_size  # 剩下为测试集大小
  
  # 应用 Kennard-Stone 算法对数据排序，确保数据划分考虑分布差异（非随机划分）
  # ks 函数假设输入为特征矩阵，因此排除响应变量 y
  ksresult <- ks(as.matrix(data[, -which(names(data) == "y")]))  # 返回按分布排序的索引
  
  # 依据排序索引选择训练集与测试集
  train_indices <- ksresult[1:train_size]  # 前部分数据作为训练集
  test_indices <- ksresult[(train_size + 1):(train_size + test_size)]  # 后部分数据为测试集
  
  # 从原始数据中提取训练集与测试集
  train_data <- data[train_indices, ]
  test_data <- data[test_indices, ]
  
  # 设定交叉验证策略为 Leave-One-Out Cross-Validation（LOOCV）
  trainControl <- trainControl(method = "LOOCV")
  
  # 使用 caret 包中的 train 函数构建 PLS 模型
  # 自动寻找最佳的成分数（ncomp ∈ 1:k）以最小化预测误差
  pls_model <- train(
    y ~ ., data = train_data,
    method = "pls",
    trControl = trainControl,
    tuneGrid = expand.grid(ncomp = 1:k)
  )
  
  # 提取最佳成分数（对应最低CV误差的 ncomp 值）
  m <- pls_model$bestTune$ncomp
  
  # 使用自定义的 pls_function 进行模型评估
  result <- pls_function(train_data, test_data, m)
  
  # 将当前训练比例对应的结果存入结果列表
  results_list[[as.character(proportion)]] <- result
}

# 输出所有训练比例下的建模评估结果（可进一步保存或可视化）
print(results_list)
