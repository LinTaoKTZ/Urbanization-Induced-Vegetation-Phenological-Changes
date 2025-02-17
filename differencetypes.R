#置工作空间
setwd("H:/paper2-vegetation type/00-data/002-table/5-Rtable/2-不同类型物候间的差异")#设置工作路径
data <- read_excel("difference_types.xls, sheet1") 
data

library(FSA)#用于Dunn's Test事后比较
library(ggplot2)

# 将土地利用类型转换为因子变量
data$land_use<- as.factor(data$land_use)
data$SOS <- as.numeric(data$SOS)

# 进行单因素方差分析（ANOVA）
anova_result1 <- aov(SOS ~ land_use, data = data)
summary(anova_result1)
anova_result2 <- aov(EOS ~ land_use, data = data)
summary(anova_result2)
anova_result3 <- aov(GSL ~ land_use, data = data)
summary(anova_result3)

#正态检验
shapiro.test(data$SOS)

#方差齐性检验,P > 0.05 接受原假设认为方差整齐
bartlett.test(SOS ~ land_use, data = data)

# 不满足正态性和方差齐性等ANOVA的前提条件,进行非参数检验，如Kruskal-Wallis检验
  kruskal.test(SOS ~ land_use, data = data)
  kruskal.test(EOS ~ land_use, data = data)
  kruskal.test(GSL ~ land_use, data = data)
  # 然后进行Dunn's Test事后比较
  dun1 <- dunnTest(SOS ~ land_use,data=data,method="bh") 
  dun1
  dun2 <- dunnTest(EOS ~ land_use,data=data,method="bh") 
  dun2
  dun3 <- dunnTest(GSL ~ land_use,data=data,method="bh") 
  dun3
  
  dun1_significance <- dun1$p.value
  dun1_significance_df <- as.data.frame(cbind(unique(data$land_use), rep("", length(unique(data$land_use))^2)))
  colnames(dun1_significance_df) <- c("Group1","p_value")
  
  # 创建一个矩阵，计算每一对组合的p值并标记显著性
for (i in 1:(length(unique(data$land_use))^2)) {
    group1 <- dun1_significance_df$Group1[i %% length(unique(data$land_use))+1]
    p_val <- dun1_significance[i]
    
    dun1_significance_df[i, "p_value"] <- p_val
    dun1_significance_df[i, "significance_star"] <- ifelse(p_val < 0.05, "*", "")
  }
print(dun1_significance_df)


sos<-ggplot(data, aes(x = land_use, y = SOS)) +
  geom_boxplot(outlier.shape = NA) +  # 箱线图，不显示异常点
  labs(title = "SOS Distribution by Land Use",
       x = "Land Use Type",
       y = "SOS") +
  theme_minimal() +
  
  # 标注显著性差异
  geom_text(data = dun1_significance[dun1_significance$significance_star != "", ],
            aes(x = as.numeric(Group1) + as.numeric(Group2)/2 - 0.25,  # 调整文本位置
                y = Inf,
                label = significance_star),
            vjust = -1, size = 4, color = "red")  # 文本垂直对齐、大小和颜色设置
sos

Eos<-ggplot(data, aes(x = land_use, y = EOS)) +
  geom_boxplot(outlier.shape = NA) +  # 箱线图，不显示异常点
  labs(title = "EOS Distribution by Land Use",
       x = "Land Use Type",
       y = "EOS") +
  theme_minimal() +
  
  # 标注显著性差异
  geom_text(data = dun1_significance[dun1_significance$significance_star != "", ],
            aes(x = as.numeric(Group1) + as.numeric(Group2)/2 - 0.25,  # 调整文本位置
                y = Inf,
                label = significance_star),
            vjust = -1, size = 4, color = "red")  # 文本垂直对齐、大小和颜色设置
Eos

gsl<-ggplot(data, aes(x = land_use, y = GSL)) +
  geom_boxplot(outlier.shape = NA) +  # 箱线图，不显示异常点
  labs(title = "GSL Distribution by Land Use",
       x = "Land Use Type",
       y = "GSL") +
  theme_minimal() +
  
  # 标注显著性差异
  geom_text(data = dun1_significance[dun1_significance$significance_star != "", ],
            aes(x = as.numeric(Group1) + as.numeric(Group2)/2 - 0.25,  # 调整文本位置
                y = Inf,
                label = significance_star),
            vjust = -1, size = 4, color = "red")  # 文本垂直对齐、大小和颜色设置
GSL