# 加载所需的库
library(car)
library(readxl)  # 读取电子表格数据
library(moments)  # 用于计算偏度和峰度
library(tidyverse)
library(caret)
library(datasets)
library(randomForest)
library(MASS)

# 读取数据
soil_data <- read.csv("pro.csv")

# 创建数据
y <- soil_data[3]  # 假设第3列为响应变量
x <- soil_data[19] # 假设第19列为自变量

# 进行线性回归拟合模型
model <- lm(unlist(y) ~ unlist(x))

# 使用Box-Cox变换找到最优lambda
bc <- boxcox(unlist(y) ~ unlist(x))
lambda <- bc$x[which.max(bc$y)]

# 应用Box-Cox变换后的新模型
new_y <- ((unlist(y)^lambda - 1) / lambda)

# 设置图形布局为1行2列
par(mfrow = c(1, 2))  

# 原始数据的直方图
hist(unlist(y), 
     main = "", 
     xlab = "SOM g/kg", 
     col = "gray", 
     border = "black", 
     freq = TRUE, 
     breaks = 9)
# col = "red" 设置颜色，lty = 2 设置虚线，lwd = 2 设置线宽
# 计算统计信息：均值、中位数、标准差、偏度、峰度
mean_y <- mean(unlist(y))
median_y <- median(unlist(y))
sd_y <- sd(unlist(y))
skew_y <- skewness(unlist(y))
kurt_y <- kurtosis(unlist(y))
# 添加红色虚线表示样本的平均值
abline(v = mean_y, col = "red", lty = 2, lwd = 2)  
# 图例部分，放在右侧
#legend("topright", 
 #      legend = c(paste("Mean:", round(mean_y, 2)), 
 #                 paste("Median:", round(median_y, 2)),
 #                 paste("SD:", round(sd_y, 2)),
 #                 paste("Skewness:", round(skew_y, 2)),
#                  paste("Kurtosis:", round(kurt_y, 2))),
#       cex = 0.8,  # 调整字体大小为 0.8（默认是1）
#       text.width = 3,  # 控制每行的宽度，增加或减少数值可调整行距
 #      box.lty = 0,  # 去掉图例边框
#       y.intersp = 0.2,  # 调整行距，数值小会使行距更紧凑
#       inset = c(0.03, 0), 
#       bg = "transparent",
  #     text.font = 2
#)
# 变换后数据的直方图
hist(new_y, 
     main = "", 
     xlab = "BoxCox (λ=0.75) transformed SOM", 
     col = "gray", 
     border = "black", 
     freq = TRUE, 
     breaks = 9)

# 计算统计信息：均值、中位数、标准差、偏度、峰度
mean_new_y <- mean(new_y)
median_new_y <- median(new_y)
sd_new_y <- sd(new_y)
skew_new_y <- skewness(new_y)
kurt_new_y <- kurtosis(new_y)
# 添加红色虚线表示样本的平均值
abline(v = mean_new_y, col = "red", lty = 2, lwd = 2) 
# 图例部分，放在右侧
#legend("topright", 
#       legend = c(paste("Mean:", round(mean_new_y, 2)), 
#                  paste("Median:", round(median_new_y, 2)),
 #                 paste("SD:", round(sd_new_y, 2)),
 #                 paste("Skewness:", round(skew_new_y, 2)),
#                  paste("Kurtosis:", round(kurt_new_y, 2))),
#       cex = 0.80,  # 调整字体大小为 0.8（默认是1）
 #      text.width =3,  # 控制每行的宽度，增加或减少数值可调整行距
 #      box.lty = 0,  # 去掉图例边框
 #      y.intersp = 0.2,  # 调整行距，数值小会使行距更紧凑
  #     inset = c(-0.12, 0), 
 #      bg = "transparent",
 #      text.font = 2
#)

# 恢复默认的图像布局
par(mfrow = c(1, 1))  



# 设置图形布局为1行2列
par(mfrow = c(1, 2))  

# 原始数据的直方图
hist(unlist(y), 
     main = "Original Data", 
     xlab = "SOM g/kg", 
     ylab = "Frequency", 
     col = "gray", 
     border = "black", 
     freq = TRUE, 
     breaks = 9,
     cex.axis = 1.4,  # 调整x、y轴刻度的大小
     cex.lab = 1.6,   # 调整x、y轴标签的大小
     cex.main = 1.8)  # 调整标题的大小

# 添加红色虚线表示样本的平均值
mean_y <- mean(unlist(y))
abline(v = mean_y, col = "red", lty = 2, lwd = 2)  

# 变换后数据的直方图
hist(new_y, 
     main = "Box-Cox Transformed Data", 
     xlab = "BoxCox (λ=0.75) transformed SOM", 
     ylab = "Frequency", 
     col = "gray", 
     border = "black", 
     freq = TRUE, 
     breaks = 9,
     cex.axis = 1.4,  # 调整x、y轴刻度的大小
     cex.lab = 1.6,   # 调整x、y轴标签的大小
     cex.main = 1.8)  # 调整标题的大小

# 添加红色虚线表示样本的平均值
mean_new_y <- mean(new_y)
abline(v = mean_new_y, col = "red", lty = 2, lwd = 2) 

# 恢复默认的图像布局
par(mfrow = c(1, 1)) 



















