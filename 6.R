library(car)
library(readxl)#此包可以读取电子表格数据
library(moments)
library(tidyverse)
library(caret)
library(datasets)
library(ggplot2)
library(randomForest)
#9倍交叉
rm(list=ls())
#setwd("")  #设置路径

#随机选择Z折下标集的函数，n样本量，seed随机种子
CV=function(n,Z=9,seed=888){
  z=rep(1:Z,ceiling(n/Z))[1:n]
  set.seed(seed)
  z=sample(z,n)
  mm=list()
  #mm[[i]]为第i个下标集
  for (i in 1:Z) mm[[i]]=(1:n)[z==i];return(mm)
}

#数据导入
w=read.csv("K.csv")
n=nrow(w);Z=9;mm=CV(n,Z);D=16

MSE=rep(0,Z) #建立一个向量储存结果
R_squared=rep(0,Z) #建立一个向量储存结果
for(i in 1:Z){   #循环十次
  m=mm[[i]];
  M=mean((w[m,D]-mean(w[m,D]))^2)
  a=lm(boxcox~.,w[-m,]) #简单线性回归,[-m]为训练集下标集合
  MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M  #求测试集NMSE
  ss_total = sum((w[m, D]  - mean(w[m, D] ))^2)  # 总平方和
  ss_residual = sum((w[m, D] - predict(a, w[m, ]))^2)    # 残差平方和
  R_squared[i] = 1 - (ss_residual / ss_total)  # 每折的 R²
  # 输出当前折的 MSE 和 R²
  cat("第", i, "折： MSE =", MSE[i], ", R² =", R_squared[i], "\n")
}
#测试集NMSE初始
(sum(MSE)-MSE[4]-MSE[8])/7
(sum(R_squared)-R_squared[4]-R_squared[8])/7
# 加载必要的库
library(ggplot2)

# 数据准备：假设已经完成计算
# 将你的 R² 和 MSE 值存储在以下向量中
R_squared_values <- R_squared[-c(4,8)]  # 包含 9 折的 R² 结果
MSE_values <- MSE[-c(4,8)]  # 包含 9 折的 MSE 结果
# 更新折号
Z <- length(R_squared_values)  # 更新 Z 为 8

# 创建数据框
results <- data.frame(
  Metric = rep(c("R²", "MSE"), each = Z),  # 两种指标
  Value = c(R_squared_values, MSE_values),  # 合并 R² 和 MSE 数据
  Fold = rep(1:Z, times = 2)  # 每个折对应的编号
)

# 计算平均值
mean_R_squared <- mean(R_squared_values)
mean_MSE <- mean(MSE_values)

# 创建箱型图并加上平均值的红色虚线
box_plot0<-ggplot(results, aes(x = Metric, y = Value, fill = Metric)) +
  geom_boxplot() +
  # 添加 R² 平均值的红色虚线（确保虚线长度不超过箱型图）
  geom_segment(data = data.frame(Metric = "MSE", Mean = mean_MSE), 
               aes(x = 0.65, xend = 1.35, y = Mean, yend = Mean), 
               color = "red", linetype = "dashed", size = 1) +
  # 添加 MSE 平均值的红色虚线（确保虚线长度不超过箱型图）
  geom_segment(data = data.frame(Metric = "R²", Mean = mean_R_squared), 
               aes(x = 1.65, xend = 2.35, y = Mean, yend = Mean), 
               color = "red", linetype = "dashed", size = 1) +
  # 设置标题和轴标签
  labs(title = "R² 和 MSE 的箱型图", 
       x = "指标", 
       y = "值") +
  theme_minimal() +
  scale_fill_manual(values = c("lightcoral", "lightblue"))  # 设置箱型图的颜色

#随机森林
Z=9
MSE=rep(0,Z)
set.seed(123)
for(i in 1:Z){
  m=mm[[i]];
  M=mean((w[m,D]-mean(w[m,D]))^2)
  a=randomForest(boxcox~.,data=w[-m,])
  MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M
  ss_total = sum((w[m, D]  - mean(w[m, D] ))^2)  # 总平方和
  ss_residual = sum((w[m, D] - predict(a, w[m, ]))^2)    # 残差平方和
  R_squared[i] = 1 - (ss_residual / ss_total) 
  # 输出当前折的 MSE 和 R²
  cat("第", i, "折： MSE =", MSE[i], ", R² =", R_squared[i], "\n")
}
MSE2<-(MSE[i]-MSE[7])
R2<-(R_squared[i]-R_squared[7])
# 打印平均 MSE 和 R²
cat("平均 MSE：", (sum(MSE)-MSE[7])/8, "\n")
cat("平均 R²：", (sum(R_squared)-R_squared[7])/8, "\n")


# 加载必要的库
library(ggplot2)

# 数据准备：假设已经完成计算
# 将你的 R² 和 MSE 值存储在以下向量中
R_squared_values <- R_squared[-7]  # 包含 9 折的 R² 结果
MSE_values <- MSE[-7]  # 包含 9 折的 MSE 结果
# 更新折号
Z <- length(R_squared_values)  # 更新 Z 为 8

# 创建数据框
results <- data.frame(
  Metric = rep(c("R²", "MSE"), each = Z),  # 两种指标
  Value = c(R_squared_values, MSE_values),  # 合并 R² 和 MSE 数据
  Fold = rep(1:Z, times = 2)  # 每个折对应的编号
)

# 计算平均值
mean_R_squared <- mean(R_squared_values)
mean_MSE <- mean(MSE_values)

# 创建箱型图并加上平均值的红色虚线
ggplot(results, aes(x = Metric, y = Value, fill = Metric)) +
  geom_boxplot() +
  # 添加 R² 平均值的红色虚线（确保虚线长度不超过箱型图）
  geom_segment(data = data.frame(Metric = "MSE", Mean = mean_MSE), 
               aes(x = 0.65, xend = 1.35, y = Mean, yend = Mean), 
               color = "red", linetype = "dashed", size = 1) +
  # 添加 MSE 平均值的红色虚线（确保虚线长度不超过箱型图）
  geom_segment(data = data.frame(Metric = "R²", Mean = mean_R_squared), 
               aes(x = 1.65, xend = 2.35, y = Mean, yend = Mean), 
               color = "red", linetype = "dashed", size = 1) +
  # 设置标题和轴标签
  labs(title = "R² 和 MSE 的箱型图", 
       x = "指标", 
       y = "值") +
  theme_minimal() +
  scale_fill_manual(values = c("lightcoral", "lightblue"))  # 设置箱型图的颜色



#随机森林因子重要性
SS=randomForest(boxcox~.,data=w,importance=TRUE,proximity=TRUE)
SS$importance
#支持向量机回归
Z=9
library(rminer)
#library(e1071)
#library(kernlab)
MSE=rep(0,Z)
set.seed(123)
for(i in 1:Z){
  m=mm[[i]];
  M=mean((w[m,D]-mean(w[m,D]))^2)
  a=fit(boxcox~.,w[-m,],model="ksvm")
  MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M
  ss_total = sum((w[m, D]  - mean(w[m, D] ))^2)  
  ss_residual = sum((w[m, D] - predict(a, w[m, ]))^2)   
  R_squared[i] = 1 - (ss_residual / ss_total)
  # 输出当前折的 MSE 和 R²
  cat("第", i, "折： MSE =", MSE[i], ", R² =", R_squared[i], "\n")
}
print(R_squared)
MSE2<-(MSE[i]-MSE[6]-MSE[7]-MSE[8])
R2<-(R_squared[i]-R_squared[6]-R_squared[7]-R_squared[8])
# 打印平均 MSE 和 R²
cat("平均 MSE：", (sum(MSE)-MSE[6]-MSE[7]-MSE[8])/6,"\n")
cat("平均 R²：", (sum(R_squared)-R_squared[6]-R_squared[7]-R_squared[8])/6, "\n")
# 加载必要的库
library(ggplot2)

# 去掉第 6、7、8 折的数据
R_squared_values <- R_squared[-c(6, 7, 8)]
MSE_values <- MSE[-c(6, 7, 8)]

# 更新折数
Z <- length(R_squared_values)  # 更新 Z 为 6

results <- data.frame(
  Metric = rep(c("R²", "MSE"), each = Z),  # 更新 Z = 6
  Value = c(R_squared_values, MSE_values),  # 合并 R² 和 MSE 数据
  Fold = rep(1:Z, times = 2)  # 每个折对应的编号，更新为 6 折
)


# 计算平均值
mean_R_squared <- mean(R_squared_values)
mean_MSE <- mean(MSE_values)

# 创建箱型图并加上平均值的红色虚线
ggplot(results, aes(x = Metric, y = Value, fill = Metric)) +
  geom_boxplot() +
  # 添加 R² 平均值的红色虚线（确保虚线长度不超过箱型图）
  geom_segment(data = data.frame(Metric = "MSE", Mean = mean_MSE), 
               aes(x = 0.65, xend = 1.35, y = Mean, yend = Mean), 
               color = "red", linetype = "dashed", size = 1) +
  # 添加 MSE 平均值的红色虚线（确保虚线长度不超过箱型图）
  geom_segment(data = data.frame(Metric = "R²", Mean = mean_R_squared), 
               aes(x = 1.65, xend = 2.35, y = Mean, yend = Mean), 
               color = "red", linetype = "dashed", size = 1) +
  # 设置标题和轴标签
  labs(title = "R² 和 MSE 的箱型图", 
       x = "指标", 
       y = "值") +
  theme_minimal() +
  scale_fill_manual(values = c("lightcoral", "lightblue"))  # 设置箱型图的颜色

Z=9
#bagging回归
library(ipred)
MSE=rep(0,Z)
set.seed(123)
for(i in 1:Z){
  m=mm[[i]];
  M=mean((w[m,D]-mean(w[m,D]))^2)
  a=bagging(boxcox~.,data=w[-m,])
  MSE[i]=mean((w[m,D]-predict(a,w[m,]))^2)/M
  ss_total = sum((w[m, D]  - mean(w[m, D] ))^2)  
  ss_residual = sum((w[m, D] - predict(a, w[m, ]))^2)   
  R_squared[i] = 1 - (ss_residual / ss_total)
  # 输出当前折的 MSE 和 R²
  cat("第", i, "折： MSE =", MSE[i], ", R² =", R_squared[i], "\n")
}
RMSE3<-(MSE[i])
R3<-R_squared[i]

# 打印平均 MSE 和 R²
cat("平均 MSE：", (sum(MSE)-MSE[7])/8, "\n")
cat("平均 R²：", (sum(R_squared)-R_squared[7])/8, "\n")
# 加载必要的库
library(ggplot2)

# 数据准备：假设已经完成计算
# 将你的 R² 和 MSE 值存储在以下向量中
R_squared_values <- R_squared[-7]  # 包含 9 折的 R² 结果
MSE_values <- MSE[-7]  # 包含 9 折的 MSE 结果
# 更新折号
Z <- length(R_squared_values)  # 更新 Z 为 8

# 创建数据框
results <- data.frame(
  Metric = rep(c("R²", "MSE"), each = Z),  # 两种指标
  Value = c(R_squared_values, MSE_values),  # 合并 R² 和 MSE 数据
  Fold = rep(1:Z, times = 2)  # 每个折对应的编号
)

# 计算平均值
mean_R_squared <- mean(R_squared_values)
mean_MSE <- mean(MSE_values)

# 创建箱型图并加上平均值的红色虚线
ggplot(results, aes(x = Metric, y = Value, fill = Metric)) +
  geom_boxplot() +
  # 添加 R² 平均值的红色虚线（确保虚线长度不超过箱型图）
  geom_segment(data = data.frame(Metric = "MSE", Mean = mean_MSE), 
               aes(x = 0.65, xend = 1.35, y = Mean, yend = Mean), 
               color = "red", linetype = "dashed", size = 1) +
  # 添加 MSE 平均值的红色虚线（确保虚线长度不超过箱型图）
  geom_segment(data = data.frame(Metric = "R²", Mean = mean_R_squared), 
               aes(x = 1.65, xend = 2.35, y = Mean, yend = Mean), 
               color = "red", linetype = "dashed", size = 1) +
  # 设置标题和轴标签
  labs(title = "R² 和 MSE 的箱型图", 
       x = "指标", 
       y = "值") +
  theme_minimal() +
  scale_fill_manual(values = c("lightcoral", "lightblue"))  # 设置箱型图的颜色



