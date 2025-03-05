install.packages("readxl")
install.packages("tidyverse")
install.packages("cluster")
install.packages("openxlsx")


# Load the required libraries
library(readxl)      # For reading Excel files
library(tidyverse)   # For data manipulation and visualisation
library(cluster)     # For clustering methods
library(openxlsx)    # For exporting data to Excel

watch_data <- read_excel(file.choose())

# 查看数据结构
str(watch_data)
summary(watch_data)

df <- watch_data %>% 
  select(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style)
dfz <- scale(df)
view(dfz)
# calculate euclidean distance
distance <- dist(dfz, method = 'euclidean')
#cluster dendrogram
hc.w <- hclust(distance, method = 'ward.D')
x <- 1:10 
sort_height <- sort(hc.w$height, decreasing = TRUE)
y <- sort_height[1:10]
plot(x, y, type = "b", main = "Elbow Plot for SmartWatch Clustering", 
     xlab = "Number of Clusters", 
     ylab = "Merged Height (Top 10)")
lines(x, y, col = "blue")4


plot(hc.w, main="Dendrogram of Watch Data", xlab="Observations", ylab="Height")

# 可观察hclust对象的merge高度或其它指标
# Rect.hclust可在树状图上画框
rect.hclust(hc_result, k = 4, border = 2:5)
watch_data$Cluster <- cutree(hc_result, k = 4)

# 假设我们已经导入并标准化了数据，存储在变量 survey_scaled 中
# survey_scaled <- scale(原始变量数据集)


library(dplyr)
cluster_summary <- watch_data %>%
  group_by(Cluster) %>%
  summarise(across(c(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style),
                   mean, .names="mean_{col}"))
cluster_summary

# 将AmznP列转换为因子类型(若尚未转换)
watch_data$AmznP <- factor(watch_data$AmznP, levels = c(0,1), labels = c("NonPrime","Prime"))

# 进行独立样本T检验
t_prime <- t.test(Wellness ~ AmznP, data = watch_data)
t_prime

# 转换Income为因子（假设1~5对应5个等级）
watch_data$IncomeGroup <- factor(watch_data$Income, 
                                 levels = c(1,2,3,4,5), 
                                 labels = c("Low","LowMid","Mid","MidHigh","High"))

anova_athlete <- aov(Athlete ~ IncomeGroup, data = watch_data)
summary(anova_athlete)

# 若ANOVA结果显著，可作Tukey事后检验
TukeyHSD(anova_athlete)

