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


rect.hclust(hc_result, k = 4, border = 2:5)
watch_data$Cluster <- cutree(hc_result, k = 4)




library(dplyr)
cluster_summary <- watch_data %>%
  group_by(Cluster) %>%
  summarise(across(c(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style),
                   mean, .names="mean_{col}"))
cluster_summary


watch_data$AmznP <- factor(watch_data$AmznP, levels = c(0,1), labels = c("NonPrime","Prime"))


t_prime <- t.test(Wellness ~ AmznP, data = watch_data)
t_prime


watch_data$IncomeGroup <- factor(watch_data$Income, 
                                 levels = c(1,2,3,4,5), 
                                 labels = c("Low","LowMid","Mid","MidHigh","High"))

anova_athlete <- aov(Athlete ~ IncomeGroup, data = watch_data)
summary(anova_athlete)


TukeyHSD(anova_athlete)

