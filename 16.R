# 加载包
library(corrplot)
library(ggplot2)
library(reshape2)

# 读取数据
VIF <- read.csv("iris2.csv")


# 计算相关性矩阵
cor_matrix <- cor(VIF)

# 使用 corrplot 包绘制相关性图并显示数值
corrplot(cor_matrix, 
         method = "circle",       # 使用圆形表示相关性
         type = "lower",          # 只显示下三角
         tl.col = "black",        # 标签字体颜色
         tl.cex = 1.5,            # 横纵坐标标签的字体大小
         tl.srt = 360,              # 标签旋转角度
         title = "Correlation Plot_basin",  # 设置标题
         cex.main = 2,            # 调整标题字体大小
         addCoef.col = "black",   # 显示相关系数的数值，设置字体颜色
         number.cex = 1.0,        # 调整显示数值的字体大
         mar=c(0,0,3,0)
)


# 加载包
library(corrplot)
library(ggplot2)
library(reshape2)

# 读取数据
VIF <- read.csv("iris3.csv")

# 计算相关性矩阵
cor_matrix <- cor(VIF)

# 使用 corrplot 包绘制相关性图并显示数值
corrplot(cor_matrix, 
         method = "circle",       # 使用圆形表示相关性
         type = "lower",          # 只显示下三角
         tl.col = "black",        # 标签字体颜色
         tl.cex = 1.5,            # 横纵坐标标签的字体大小
         tl.srt = 360,              # 标签旋转角度
         title = "Correlation Plot_Low Mountain Area",  # 设置标题
         cex.main = 1.6,            # 调整标题字体大小
         addCoef.col = "black",   # 显示相关系数的数值，设置字体颜色
         number.cex = 0.8,        # 调整显示数值的字体大
         mar=c(0,0,3,0)
)


# 加载包
library(corrplot)
library(ggplot2)
library(reshape2)

# 读取数据
VIF <- read.csv("iris4.csv")

# 计算相关性矩阵
cor_matrix <- cor(VIF)

# 使用 corrplot 包绘制相关性图并显示数值
corrplot(cor_matrix, 
         method = "circle",       # 使用圆形表示相关性
         type = "lower",          # 只显示下三角
         tl.col = "black",        # 标签字体颜色
         tl.cex = 1.5,            # 横纵坐标标签的字体大小
         tl.srt = 360,              # 标签旋转角度
         title = "Correlation Plot_Mid Mountain Area",  # 设置标题
         cex.main = 1.6,            # 调整标题字体大小
         addCoef.col = "black",   # 显示相关系数的数值，设置字体颜色
         number.cex = 0.8,        # 调整显示数值的字体大
         mar=c(0,0,3,0)
)

