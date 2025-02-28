#重要性，特征效应和ALE图
library(gridExtra)
library(randomForest)
library(iml)
library(ggplot2)
library(dplyr)

# 读取数据
data <- read.csv("K.csv")  # 请根据实际路径调整文件位置
# 训练一个随机森林回归模型
model_rf <- randomForest(X, y)
head(data)
set.seed(42)
rf = randomForest(boxcox ~ ., data = data, ntree = 50)
X <- data[, 1:15]  # 自变量：前15列
y <- data[, 16]    # 目标变量：第16列
predictor = Predictor$new(rf, data = X, y = y)
imp = FeatureImp$new(predictor, loss = "mse")
plot(imp)+theme(
  axis.title.x = element_text(size = 30),  # 调整x轴标题大小
  #axis.title.y = element_text(size = 14),  # 调整y轴标题大小
  axis.text.x = element_text(size = 20),   # 调整x轴刻度文本大小
  axis.text.y = element_text(size = 20)# 调整y轴刻度文本大小
)
print(imp$results)

#交互作用
#interact = Interaction$new(predictor)
#plot(interact)
#博弈论解释单个预测值
#shapley = Shapley$new(predictor, x.interest = X[1,])
#shapley$plot()
#特征效应
# 创建预测器对象（iml包中的 Predictor 类）
predictor <- Predictor$new(model_rf, data = X, y = y)
# 计算并绘制累积局部效应
# 对所有自变量进行类似的操作
# 生成多个特征的ALE图
features <- colnames(X)
# 使用循环自动生成并绘制所有特征的ALE图
for (feature in features) {
  cle <- FeatureEffect$new(predictor, feature = feature, method = "ale")
  print(cle$plot())  # 打印每个特征的累积局部效应图
}

# 使用累积局部效应 (ALE) 方法计算每个特征的图形
cle_plots <- list()  # 存储每个特征的图形

# 生成每个特征的 ALE 图
for (feature in features) {
  cle <- FeatureEffect$new(predictor, feature = feature, method = "ale")
  cle_plots[[feature]] <- cle$plot() + 
    theme(
      axis.title.x = element_text(size = 20),  # 调整x轴标题大小
      axis.title.y = element_text(size = 15),  # 调整y轴标题大小
      axis.text.x = element_text(size = 15, face = "bold"),   # 调整x轴刻度文本大小
      axis.text.y = element_text(size = 12, face = "bold")# 调整y轴刻度文本大小
      
    )
}

# 使用 grid.arrange() 将所有 ALE 图放在同一视图中
grid.arrange(grobs = cle_plots, ncol = 3)  # ncol = 3 表示每行放3个图，您可以根据需要调整







