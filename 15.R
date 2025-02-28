# 加载必要的包
library(ggplot2)
library(patchwork)
R_squared_values_A<-c(0.6239385,0.303008, 0.3887625,0.540118 ,0.1983773,0.3150074,0.7460348, 0.3150074)
R_squared_values_B<-c(0.595446,0.5244068,0.5740129,0.4347208,0.4892935, 0.4155638 ,0.41075,0.5434948)
R_squared_values_C<-c(0.4656606,0.3330461,0.5506390,0.2628406,0.3274803,0.4451158,0.3974637,0.3974637)
R_squared_values_D<-c(0.6330904,0.5688666,0.5200674,0.3417398,0.5369640,0.4659306 ,0.3126823,0.5688198)
R_squared_values_E<-c( 0.6326440 ,0.5735776 ,0.6169116, 0.4924763 ,0.5619411, 0.3999785 ,0.3278801 ,0.5879575)
R_squared_values_F<-c(0.5478439 ,0.4165268, 0.6251480 ,0.3833640, 0.4929269,0.5742297,0.5066732,0.5066732)
R_squared_values_G<-c(0.6697352, 0.5633797 ,0.5396686 ,0.4145369, 0.6195601 ,0.3949627 ,0.2877539 ,0.6129432)
MSE_values_A<-c(0.3760615,0.696992,0.6112375,0.459882,0.8016227,0.4984611,0.2539652,0.6849927)
MSE_values_B<-c(0.404554,0.4755932,0.4259871,0.5652792,0.5107065,0.5844362,0.58925,0.4565052)
MSE_values_C<-c( 0.4045540,0.4755932 ,0.4259871 ,0.5652792 ,0.5107065 ,0.5844362 ,0.5892500, 0.4565052)
MSE_values_D<-c(0.5343394, 0.6669539 ,0.4493610 ,0.7371594 ,0.6725197 ,0.6025363,0.6025363,0.5548842)
MSE_values_E<-c(0.3673560, 0.4264224 ,0.3830884, 0.5075237 ,0.4380589, 0.6000215, 0.6721199 ,0.4120425)
MSE_values_F<-c(0.4521561, 0.5834732 ,0.3748520 ,0.6166360, 0.5070731 ,0.6721199 ,0.4257703,0.4933268 ,0.4933268 )
MSE_values_G<-c(0.3302648, 0.4366203, 0.4603314 ,0.5854631, 0.6050373 ,0.7122461 ,0.3870568)
# 数据准备：构造包含两组箱型图数据的完整数据框

results <- data.frame(
  Model = rep(c("Linear", "RF","KSVM","Bagging","RFK","KK","BK"), each = 8),  # 模型名称
  R_squared = c(R_squared_values_A,R_squared_values_B,R_squared_values_C,R_squared_values_D,R_squared_values_E,R_squared_values_F,R_squared_values_G),  # R² 值
  MSE = c(MSE_values_A,MSE_values_B,MSE_values_C,MSE_values_D,MSE_values_E,MSE_values_F,MSE_values_G)  # MSE 值
)

# 计算平均值
mean_R_squared_A <- mean(R_squared_values_A)
mean_R_squared_A
mean_R_squared_B <- mean(R_squared_values_B)
mean_R_squared_B
mean_R_squared_C <- mean(R_squared_values_C)
mean_R_squared_C
mean_R_squared_D <- mean(R_squared_values_D)
mean_R_squared_D
mean_R_squared_E <- mean(R_squared_values_E)
mean_R_squared_E
mean_R_squared_F <- mean(R_squared_values_F)
mean_R_squared_F
mean_R_squared_G <- mean(R_squared_values_G)
mean_R_squared_G
mean_MSE_A <- mean(MSE_values_A)
mean_MSE_A
mean_MSE_B <- mean(MSE_values_B)
mean_MSE_B
mean_MSE_C <- mean(MSE_values_C)
mean_MSE_C
mean_MSE_D <- mean(MSE_values_D)
mean_MSE_D
mean_MSE_E <- mean(MSE_values_E)
mean_MSE_E
mean_MSE_F <- mean(MSE_values_F)
mean_MSE_F
mean_MSE_G <- mean(MSE_values_G)
mean_MSE_G
# 确保模型顺序一致
results$Model <- factor(results$Model, levels = c("Linear", "RF", "KSVM", "Bagging", "RFK", "KK", "BK"))
# R² 的箱型图
R_squared_plot <- ggplot(results, aes(x = Model, y = R_squared, fill = Model)) +
  geom_boxplot() +
  # 添加红色虚线表示均值
  geom_segment(aes(x = 0.63, xend = 1.38, y = mean_R_squared_A), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 1.63, xend = 2.38, y = mean_R_squared_B), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 2.63, xend = 3.38, y = mean_R_squared_C), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 3.63, xend = 4.38, y = mean_R_squared_D), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 4.63, xend = 5.38, y = mean_R_squared_E), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 5.63, xend = 6.38, y = mean_R_squared_F), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 6.63, xend = 7.38, y = mean_R_squared_G), color = "red", linetype = "dashed", size = 1) +
  labs(title = "", x = "Model", y = "(R²)Adj") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 30),           # 调整坐标刻度文字大小
    axis.ticks = element_line(color = "gray"),    # 添加刻度线
    axis.title.x = element_text(size = 35),          # 设置横纵坐标标题字体大小
    axis.title.y = element_text(size = 35), 
    axis.line = element_line(color = "black"),     # 添加横纵坐标轴
    plot.title = element_text(hjust = 0.5),        # 标题居中
    panel.grid.major = element_line(color = "white"), # 保留默认网格线
    #panel.background = element_rect(fill = "grey90", color = NA) ,# 设置灰色背景
    legend.position = "none"              
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +  # 设置纵坐标刻度范围
  scale_fill_manual(values = c("Linear" = "#004F98", "RF" = "#004F98", 
                               "KSVM" = "#004F98", "Bagging" = "#004F98", 
                               "RFK" = "#004F98", "KK" = "#004F98", "BK" = "#004F98"))
R_squared_plot

# MSE 的箱型图
MSE_plot <- ggplot(results, aes(x = Model, y = MSE, fill = Model)) +
  geom_boxplot() +
  # 添加红色虚线表示均值
  geom_segment(aes(x = 0.63, xend = 1.38, y = mean_MSE_A), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 1.63, xend = 2.38, y = mean_MSE_B), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 2.63, xend = 3.38, y = mean_MSE_C), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 3.63, xend = 4.38, y = mean_MSE_D), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 4.63, xend = 5.38, y = mean_MSE_E), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 5.63, xend = 6.38, y = mean_MSE_F), color = "red", linetype = "dashed", size = 1) +
  geom_segment(aes(x = 6.63, xend = 7.38, y = mean_MSE_G), color = "red", linetype = "dashed", size = 1) +
  labs(title = "", x = "Model", y = "MSE") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 30),           # 调整坐标刻度文字大小
    axis.ticks = element_line(color = "gray"),    # 添加刻度线
    axis.title.x = element_text(size = 35),          # 设置横纵坐标标题字体大小
    axis.title.y = element_text(size = 35), 
    axis.line = element_line(color = "black"),     # 添加横纵坐标轴
    plot.title = element_text(hjust = 0.5),        # 标题居中
    panel.grid.major = element_line(color = "white"), # 保留默认网格线
   # panel.background = element_rect(fill = "grey90", color = NA) ,# 设置灰色背景
    legend.position = "none"       
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.1)  # 适当扩展纵坐标范围，避免图表边界重叠
  ) +
  scale_fill_manual(values = c("Linear" = "lightcoral", "RF" = "lightcoral", 
                               "KSVM" = "lightcoral", "Bagging" = "lightcoral", 
                               "RFK" = "lightcoral", "KK" = "lightcoral", "BK" = "lightcoral"))
MSE_plot

# 合并两张图表，垂直排列
#combined_plot <- R_squared_plot | MSE_plot

# 显示图表
#combined_plot

