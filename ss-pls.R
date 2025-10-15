#1、SPlit-based hyperparameter selection in partial least squares (SS-PLS)
# 加载必要的 R 包
library(SPlit)      # 用于数据集划分的 SPlit 方法
library(caret)      # 用于构建机器学习流程，包括模型训练与交叉验证
library(pls)        # 用于部分最小二乘法（Partial Least Squares, PLS）建模

# 初始化一个列表用于存储多次划分实验的结果（此处为100次划分的实验结果容器）
results_list_100 <- list()

# 设置要进行的数据划分次数（可扩展为多次，如10次、50次、100次）
split_counts <- list(100)

# 自定义函数：进行数据划分、模型训练与交叉验证
run_cross_validation <- function(data, nSplits) {
  results <- list()  # 用于存储每次划分的结果
  
  for (i in 1:nSplits) {
    # 使用SPlit方法进行稳定性驱动的数据划分，确保划分具备代表性与稳定性
    SPlitIndices <- SPlit(data, tolerance = 1e-6, nThreads = 2)  # 设置容差阈值和线程数
    dataTest <- data[SPlitIndices, ]   # 划分出的测试集（SPlit推荐的子集）
    dataTrain <- data[-SPlitIndices, ] # 剩余数据作为训练集
    
    # 设置训练控制参数：采用留一交叉验证（LOOCV）用于选择最优的 PLS 成分数
    trainControl <- trainControl(method = "LOOCV")  # 精细评估小样本学习性能
    
    # 利用 caret::train 函数训练 PLS 模型，遍历成分数从1到k，进行超参数调优
    pls_model <- train(
      y ~ ., data = dataTrain,
      method = "pls",                     # 指定建模方法为部分最小二乘回归
      trControl = trainControl,          # 设置交叉验证控制参数
      tuneGrid = expand.grid(ncomp = 1:k)  # 指定待搜索的成分数范围
    )
    
    # 提取训练过程中选择的最优成分数（最小交叉验证误差对应的ncomp）
    best_ncomp <- pls_model$bestTune$ncomp
    
    # 使用自定义的 PLS 评估函数对训练集和测试集进行评估（需提前定义 pls_function）
    result <- pls_function(dataTrain, dataTest, best_ncomp)
    
    # 将当前划分结果存入结果列表中
    results[[i]] <- result
  }
  
  # 返回所有划分下的模型评估结果
  return(results)
}

# 调用函数：执行100次 SPlit 划分 + 留一交叉验证评估过程
# 注：data为原始数据框，需包含预测变量与响应变量y
results_list_10 <- run_cross_validation(data, split_counts[[1]])
