#3、SPXY-based hyperparameter selection in partial least squares

# spxy 函数：基于X（自变量）和y（目标变量）的距离组合，选择具有最大代表性差异性的样本子集
# 输入：
#   data - 包含自变量和目标变量的数据框，假设前701列为自变量，702列为目标变量y
#   Ncal - 需要选择的样本数量
# 输出：
#   m - 长度为Ncal的索引向量，表示选中样本的行号（代表子集）

spxy <- function(data, Ncal) {
  # 提取自变量矩阵 X 和目标变量向量 y
  X <- as.matrix(data[, 1:701])  # 自变量部分，假设701个特征
  y <- as.matrix(data[, 702])    # 目标变量
  
  dminmax <- numeric(Ncal)       # 初始化存储每一步最小距离的向量
  M <- nrow(X)                   # 样本总数
  samples <- 1:M                 # 样本索引全集
  
  # 计算样本间自变量的距离矩阵（欧氏距离）
  Dx <- as.matrix(dist(X))
  # 计算样本间目标变量的距离矩阵（欧氏距离）
  Dy <- as.matrix(dist(y))
  
  # 归一化距离，分别除以各自最大距离，防止尺度差异影响距离综合
  Dxmax <- max(Dx)
  Dymax <- max(Dy)
  
  # 结合自变量和目标变量的归一化距离，形成综合距离矩阵D
  # 综合距离 = 归一化自变量距离 + 归一化目标变量距离
  D <- Dx / Dxmax + Dy / Dymax
  
  # 寻找初始选择的两个样本对：使得两个样本之间的综合距离最大
  maxD <- apply(D, 2, max)             # 每列最大距离值
  index_row <- apply(D, 2, which.max)  # 每列最大距离对应的行索引
  index_column <- which.max(maxD)      # 最大的最大距离所在的列索引
  
  # 初始化选择的样本索引向量 m
  m <- numeric(Ncal)
  m[1] <- index_row[index_column]     # 选中第一样本
  m[2] <- index_column                 # 选中第二样本
  
  # 迭代选择剩余样本
  for (i in 3:Ncal) {
    pool <- setdiff(samples, m[1:(i-1)])  # 当前未选样本池
    
    dmin <- numeric(length(pool))          # 用于存储每个待选样本与已选样本的最小距离
    
    # 遍历待选样本，计算其与已选样本中最接近的距离
    for (j in seq_along(pool)) {
      indexa <- pool[j]
      d <- D[indexa, m[1:(i-1)]]       # 该样本与已选样本的距离向量
      dmin[j] <- min(d)                 # 取最小距离（表示与已选样本中最接近的那个）
    }
    
    # 选取具有最大最小距离的样本，即最远离当前已选样本集的样本，保证多样性
    index <- which.max(dmin)
    m[i] <- pool[index]
  }
  
  # 返回选中样本索引序列
  return(m)
}




# 初始化一个空列表，用于存储每个训练集比例下模型的结果
results_list <- list()

# 设置训练集比例的范围，从0.4到0.8，步长为0.1
ratios <- seq(0.4, 0.8, by = 0.1)

# 循环遍历每一个训练集比例
for (ratio in ratios) {
  
  # 使用 spxy 函数从整个数据集 wheat 中选择100个代表性样本的索引
  # spxy 通过综合考虑特征和目标变量的距离来选择具有代表性的样本
  selected_indices <- spxy(data, nrow(data))
  
  # 根据当前比例计算训练集大小（向上或向下取整）
  train_size <- round(ratio * nrow(data))
  
  # 从选中的样本索引中取出前 train_size 个作为训练集索引
  train_indices <- selected_indices[1:train_size]
  
  # 根据索引划分训练集和测试集
  dataTrain <- data[train_indices, ]     # 训练集数据
  dataTest <- data[-train_indices, ]     # 测试集数据（剩余样本）
  
  # 设置训练过程中的交叉验证方法，这里采用留一交叉验证（LOOCV）
  trainControl <- trainControl(method = "LOOCV")
  
  # 使用 caret 包中的 train 函数训练 PLS 模型
  # 通过交叉验证选择最优的主成分数（ncomp）
  pls_model <- train(y ~ ., data = dataTrain, method = "pls", 
                     trControl = trainControl, 
                     tuneGrid = expand.grid(ncomp = 1:k))
  
  # 打印交叉验证训练结果，便于观察模型调参情况
  print(pls_model)
  
  # 使用选出的最优成分数，调用自定义 pls_function 对训练集和测试集进行建模与预测
  result <- pls_function(dataTrain, dataTest, pls_model$bestTune$ncomp)
  
  # 将该比例对应的模型结果存储到列表中，键为当前比例的字符串形式
  results_list[[as.character(ratio)]] <- result
  
}
