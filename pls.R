pls_function=function(dataTrain, dataTest, m){
  pls.fit <- pls::mvr(y ~ ., ncomp = m, data = dataTrain, method = "simpls", center = TRUE)
  # 对训练数据做出预测
  fit_y <- predict(pls.fit, ncomp = m, newdata = dataTrain) # 此处不需选取x
  
  # 对测试数据做出预测
  pred_y <- predict(pls.fit, ncomp = m, newdata = dataTest) # 此处不需选取x
  
  # 计算并返回模型评估指标
  Output_pls <- list(
    K = pls.fit$ncomp,
    nvar = length(which(pls.fit[["coefficients"]][,,pls.fit$ncomp] != 0)),
    RMSEC = sqrt(mean((fit_y - dataTrain$y)^2, na.rm = TRUE)),
    RMSEP = sqrt(mean((pred_y - dataTest$y)^2, na.rm = TRUE)),
    MAE = mean(abs(pred_y - dataTest$y), na.rm = TRUE),
    Q2.train = 1 - sum((fit_y - dataTrain$y)^2, na.rm = TRUE) / sum((dataTrain$y - mean(dataTrain$y))^2, na.rm = TRUE),
    Q2.test = 1 - (sum((pred_y - dataTest$y)^2, na.rm = TRUE) / sum((dataTest$y - mean(dataTest$y))^2))
  )
  
  return(Output_pls)
}
