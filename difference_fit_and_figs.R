#forests_difference
#设置工作空间
setwd("H:/paper2-vegetation type/00-data/002-table/5-Rtable/1-different type")#设置工作路径
data <- read_excel("forests_difference.xls", sheet = "Sheet1") 
data

library(ggpubr)
library(ggplot2)
library(ggsci)
library(cowplot)
library(ggpmisc)
library(egg)#图表主题theme_article()


forestsGSL<-ggviolin(data, x="ZU", y="GSL", fill="ZU",color="ZU",size=0.4,width = 0.8,alpha = 0.7,
                     add="boxplot",add.params = list(fill = "white"),
                     palette=c("#0E3461","#c1cbd7"),#"#8696a7","#c1cbd7"
                     xlab=" ", 
                     ylab="",
                     legend.title="GSL") + 
  # 差异性检验
  stat_compare_means(method="wilcox.test", paired = T, comparisons=list(c("urban", "nonurban")),label = "p.signif",size=8) +
  #添加均值点
  stat_summary(fun = "mean",geom = "point",pch = 24,size = 3,color = "black",fill="yellow")+ #pch点形状
  
  theme_article() + 
  # 设置主次网格线
  #theme(panel.grid.major = element_line(colour = "gray90"), 
  #panel.grid.minor = element_line(colour = "gray90")) + 
  # 设置轴标题和图标标题
  theme(axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 14, 
                                  face = "bold"), legend.title = element_text(face = "bold"))+
  
  theme(plot.title = element_text(hjust =0,colour="black",size=15), # 将图标题居中
        axis.text.x=element_text(hjust = 0.5,colour="black",face="bold",size=12), #设置x轴刻度标签的字体显示倾斜角度为45度，并向下调整1(hjust = 1)，字体大小为14
        axis.text.y=element_text(hjust=0, colour="black",face="bold",size=12), #设置y轴刻度标签的字体簇，字体大小，字体样式为 bold （加粗）
        axis.title.x=element_text(size=12,face="bold"),#设置x轴标题的字体属性
        axis.title.y=element_text(size=12,face="bold"))+
  theme(legend.position = "none")+
  theme(
    plot.background = element_rect(fill = NA), # 不填充背景色以突出边框
    panel.border = element_rect(colour = "black", fill = NA, size = 1) # 设置边框颜色、不填充内部以及边框粗细
  )
forestsGSL
ggsave("forests-GSL.png", plot = GSL, width = 4, height = 4, units = "in", dpi = 300)

forestsSOS<-ggviolin(data, x="ZU", y="SOS", fill="ZU",color="ZU",size=0.4,width = 0.8,alpha = 0.7,
                     add="boxplot",add.params = list(fill = "white"),
                     palette=c("#7b8b6f","#D0DBCF"),#"#8696a7","#c1cbd7"
                     xlab=" ", 
                     ylab="",
                     legend.title="SOS") + 
  # 差异性检验
  stat_compare_means(method="wilcox.test", paired = T, comparisons=list(c("urban", "nonurban")),label = "p.signif",size=8) +
  #添加均值点
  stat_summary(fun = "mean",geom = "point",pch = 24,size = 3,color = "black",fill="yellow")+ #pch点形状
  
  theme_article() + 
  # 设置主次网格线
  #theme(panel.grid.major = element_line(colour = "gray90"), 
  #panel.grid.minor = element_line(colour = "gray90")) + 
  # 设置轴标题和图标标题
  theme(axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 14, 
                                  face = "bold"), legend.title = element_text(face = "bold"))+
  
  theme(plot.title = element_text(hjust =0,colour="black",size=15), # 将图标题居中
        axis.text.x=element_text(hjust = 0.5,colour="black",face="bold",size=12), #设置x轴刻度标签的字体显示倾斜角度为45度，并向下调整1(hjust = 1)，字体大小为14
        axis.text.y=element_text(hjust=0, colour="black",face="bold",size=12), #设置y轴刻度标签的字体簇，字体大小，字体样式为 bold （加粗）
        axis.title.x=element_text(size=12,face="bold"),#设置x轴标题的字体属性
        axis.title.y=element_text(size=12,face="bold"))+
  theme(legend.position = "none")+
  theme(
    plot.background = element_rect(fill = NA), # 不填充背景色以突出边框
    panel.border = element_rect(colour = "black", fill = NA, size = 1) # 设置边框颜色、不填充内部以及边框粗细
  )
forestsSOS
ggsave("forests-SOS.png", plot = SOS, width = 4, height = 4, units = "in", dpi = 300)

forestsEOS<-ggviolin(data, x="ZU", y="EOS", fill="ZU",color="ZU",size=0.4,width = 0.8,alpha = 0.7,
                     add="boxplot",add.params = list(fill = "white"),
                     palette=c("#de773f","#d8caaf"),#"#8696a7","#c1cbd7"
                     xlab=" ", 
                     ylab="",
                     legend.title="EOS") + 
  # 差异性检验
  stat_compare_means(method="wilcox.test", paired = T, comparisons=list(c("urban", "nonurban")),label = "p.signif",size=8) +
  #添加均值点
  stat_summary(fun = "mean",geom = "point",pch = 24,size = 3,color = "black",fill="yellow")+ #pch点形状
  
  theme_article() + 
  # 设置主次网格线
  #theme(panel.grid.major = element_line(colour = "gray90"), 
  #panel.grid.minor = element_line(colour = "gray90")) + 
  # 设置轴标题和图标标题
  theme(axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 14, 
                                  face = "bold"), legend.title = element_text(face = "bold"))+
  
  theme(plot.title = element_text(hjust =0,colour="black",size=15), # 将图标题居中
        axis.text.x=element_text(hjust = 0.5,colour="black",face="bold",size=12), #设置x轴刻度标签的字体显示倾斜角度为45度，并向下调整1(hjust = 1)，字体大小为14
        axis.text.y=element_text(hjust=0, colour="black",face="bold",size=12), #设置y轴刻度标签的字体簇，字体大小，字体样式为 bold （加粗）
        axis.title.x=element_text(size=12,face="bold"),#设置x轴标题的字体属性
        axis.title.y=element_text(size=12,face="bold"))+
  theme(legend.position = "none")+
  theme(
    plot.background = element_rect(fill = NA), # 不填充背景色以突出边框
    panel.border = element_rect(colour = "black", fill = NA, size = 1) # 设置边框颜色、不填充内部以及边框粗细
  )
forestsEOS
ggsave("forests-EOS.png", plot = EOS, width = 4, height = 4, units = "in", dpi = 300)

#grassland_difference
setwd("H:/paper2-vegetation type/00-data/002-table/5-Rtable/1-different type")#设置工作路径
data <- read_excel("grassland_difference.xls", sheet = "Sheet1") 
data

library(ggpubr)
library(ggplot2)
library(ggsci)
library(cowplot)
library(ggpmisc)
library(egg)#图表主题theme_article()


grasslandGSL<-ggviolin(data, x="ZU", y="GSL", fill="ZU",color="ZU",size=0.4,width = 0.8,alpha = 0.7,
                       add="boxplot",add.params = list(fill = "white"),
                       palette=c("#0E3461","#c1cbd7"),#"#8696a7","#c1cbd7"
                       xlab=" ", 
                       ylab="",
                       legend.title="GSL") + 
  # 差异性检验
  stat_compare_means(method="wilcox.test", paired = T, comparisons=list(c("urban", "nonurban")),label = "p.signif",size=8) +
  #添加均值点
  stat_summary(fun = "mean",geom = "point",pch = 24,size = 3,color = "black",fill="yellow")+ #pch点形状
  
  theme_article() + 
  # 设置主次网格线
  #theme(panel.grid.major = element_line(colour = "gray90"), 
  #panel.grid.minor = element_line(colour = "gray90")) + 
  # 设置轴标题和图标标题
  theme(axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 14, 
                                  face = "bold"), legend.title = element_text(face = "bold"))+
  
  theme(plot.title = element_text(hjust =0,colour="black",size=15), # 将图标题居中
        axis.text.x=element_text(hjust = 0.5,colour="black",face="bold",size=12), #设置x轴刻度标签的字体显示倾斜角度为45度，并向下调整1(hjust = 1)，字体大小为14
        axis.text.y=element_text(hjust=0, colour="black",face="bold",size=12), #设置y轴刻度标签的字体簇，字体大小，字体样式为 bold （加粗）
        axis.title.x=element_text(size=12,face="bold"),#设置x轴标题的字体属性
        axis.title.y=element_text(size=12,face="bold"))+
  theme(legend.position = "none")+
  theme(
    plot.background = element_rect(fill = NA), # 不填充背景色以突出边框
    panel.border = element_rect(colour = "black", fill = NA, size = 1) # 设置边框颜色、不填充内部以及边框粗细
  )
grasslandGSL
ggsave("grassland-GSL.png", plot = GSL, width = 4, height = 4, units = "in", dpi = 300)


grasslandSOS<-ggviolin(data, x="ZU", y="SOS", fill="ZU",color="ZU",size=0.4,width = 0.8,alpha = 0.7,
                       add="boxplot",add.params = list(fill = "white"),
                       palette=c("#7b8b6f","#D0DBCF"),#"#8696a7","#c1cbd7"
                       xlab=" ", 
                       ylab="",
                       legend.title="SOS") + 
  # 差异性检验
  stat_compare_means(method="wilcox.test", paired = T, comparisons=list(c("urban", "nonurban")),label = "p.signif",size=8) +
  #添加均值点
  stat_summary(fun = "mean",geom = "point",pch = 24,size = 3,color = "black",fill="yellow")+ #pch点形状
  
  theme_article() + 
  # 设置主次网格线
  #theme(panel.grid.major = element_line(colour = "gray90"), 
  #panel.grid.minor = element_line(colour = "gray90")) + 
  # 设置轴标题和图标标题
  theme(axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 14, 
                                  face = "bold"), legend.title = element_text(face = "bold"))+
  
  theme(plot.title = element_text(hjust =0,colour="black",size=15), # 将图标题居中
        axis.text.x=element_text(hjust = 0.5,colour="black",face="bold",size=12), #设置x轴刻度标签的字体显示倾斜角度为45度，并向下调整1(hjust = 1)，字体大小为14
        axis.text.y=element_text(hjust=0, colour="black",face="bold",size=12), #设置y轴刻度标签的字体簇，字体大小，字体样式为 bold （加粗）
        axis.title.x=element_text(size=12,face="bold"),#设置x轴标题的字体属性
        axis.title.y=element_text(size=12,face="bold"))+
  theme(legend.position = "none")+
  theme(
    plot.background = element_rect(fill = NA), # 不填充背景色以突出边框
    panel.border = element_rect(colour = "black", fill = NA, size = 1) # 设置边框颜色、不填充内部以及边框粗细
  )
grasslandSOS
ggsave("grassland-SOS.png", plot = SOS, width = 4, height = 4, units = "in", dpi = 300)

grasslandEOS<-ggviolin(data, x="ZU", y="EOS", fill="ZU",color="ZU",size=0.4,width = 0.8,alpha = 0.7,
                       add="boxplot",add.params = list(fill = "white"),
                       palette=c("#de773f","#d8caaf"),#"#8696a7","#c1cbd7"
                       xlab=" ", 
                       ylab="",
                       legend.title="EOS") + 
  # 差异性检验
  stat_compare_means(method="wilcox.test", paired = T, comparisons=list(c("urban", "nonurban")),label = "p.signif",size=8) +
  #添加均值点
  stat_summary(fun = "mean",geom = "point",pch = 24,size = 3,color = "black",fill="yellow")+ #pch点形状
  
  theme_article() + 
  # 设置主次网格线
  #theme(panel.grid.major = element_line(colour = "gray90"), 
  #panel.grid.minor = element_line(colour = "gray90")) + 
  # 设置轴标题和图标标题
  theme(axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 14, 
                                  face = "bold"), legend.title = element_text(face = "bold"))+
  
  theme(plot.title = element_text(hjust =0,colour="black",size=15), # 将图标题居中
        axis.text.x=element_text(hjust = 0.5,colour="black",face="bold",size=12), #设置x轴刻度标签的字体显示倾斜角度为45度，并向下调整1(hjust = 1)，字体大小为14
        axis.text.y=element_text(hjust=0, colour="black",face="bold",size=12), #设置y轴刻度标签的字体簇，字体大小，字体样式为 bold （加粗）
        axis.title.x=element_text(size=12,face="bold"),#设置x轴标题的字体属性
        axis.title.y=element_text(size=12,face="bold"))+
  theme(legend.position = "none")+
  theme(
    plot.background = element_rect(fill = NA), # 不填充背景色以突出边框
    panel.border = element_rect(colour = "black", fill = NA, size = 1) # 设置边框颜色、不填充内部以及边框粗细
  )
grasslandEOS
ggsave("grassland-EOS.png", plot = EOS, width = 4, height = 4, units = "in", dpi = 300)

#shrublands-difference
setwd("H:/paper2-vegetation type/00-data/002-table/5-Rtable/1-different type")#设置工作路径
data <- read_excel("shrublands-difference.xls", sheet = "Sheet1") 
data

library(ggpubr)
library(ggplot2)
library(ggsci)
library(cowplot)
library(ggpmisc)
library(egg)#图表主题theme_article()


shrublandsGSL<-ggviolin(data, x="ZU", y="GSL", fill="ZU",color="ZU",size=0.4,width = 0.8,alpha = 0.7,
                        add="boxplot",add.params = list(fill = "white"),
                        palette=c("#0E3461","#c1cbd7"),#"#8696a7","#c1cbd7"
                        xlab=" ", 
                        ylab="",
                        legend.title="GSL") + 
  # 差异性检验
  stat_compare_means(method="wilcox.test", paired = T, comparisons=list(c("urban", "nonurban")),label = "p.signif",size=8) +
  #添加均值点
  stat_summary(fun = "mean",geom = "point",pch = 24,size = 3,color = "black",fill="yellow")+ #pch点形状
  
  theme_article() + 
  # 设置主次网格线
  #theme(panel.grid.major = element_line(colour = "gray90"), 
  #panel.grid.minor = element_line(colour = "gray90")) + 
  # 设置轴标题和图标标题
  theme(axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 14, 
                                  face = "bold"), legend.title = element_text(face = "bold"))+
  
  theme(plot.title = element_text(hjust =0,colour="black",size=15), # 将图标题居中
        axis.text.x=element_text(hjust = 0.5,colour="black",face="bold",size=12), #设置x轴刻度标签的字体显示倾斜角度为45度，并向下调整1(hjust = 1)，字体大小为14
        axis.text.y=element_text(hjust=0, colour="black",face="bold",size=12), #设置y轴刻度标签的字体簇，字体大小，字体样式为 bold （加粗）
        axis.title.x=element_text(size=12,face="bold"),#设置x轴标题的字体属性
        axis.title.y=element_text(size=12,face="bold"))+
  theme(legend.position = "none")+
  theme(
    plot.background = element_rect(fill = NA), # 不填充背景色以突出边框
    panel.border = element_rect(colour = "black", fill = NA, size = 1) # 设置边框颜色、不填充内部以及边框粗细
  )
shrublandsGSL
ggsave("shrublandsGSL.png", plot = GSL, width = 4, height = 4, units = "in", dpi = 300)


shrublandsSOS<-ggviolin(data, x="ZU", y="SOS", fill="ZU",color="ZU",size=0.4,width = 0.8,alpha = 0.7,
                        add="boxplot",add.params = list(fill = "white"),
                        palette=c("#7b8b6f","#D0DBCF"),#"#8696a7","#c1cbd7"
                        xlab=" ", 
                        ylab="",
                        legend.title="SOS") + 
  # 差异性检验
  stat_compare_means(method="wilcox.test", paired = T, comparisons=list(c("urban", "nonurban")),label = "p.signif",size=8) +
  #添加均值点
  stat_summary(fun = "mean",geom = "point",pch = 24,size = 3,color = "black",fill="yellow")+ #pch点形状
  
  theme_article() + 
  # 设置主次网格线
  #theme(panel.grid.major = element_line(colour = "gray90"), 
  #panel.grid.minor = element_line(colour = "gray90")) + 
  # 设置轴标题和图标标题
  theme(axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 14, 
                                  face = "bold"), legend.title = element_text(face = "bold"))+
  
  theme(plot.title = element_text(hjust =0,colour="black",size=15), # 将图标题居中
        axis.text.x=element_text(hjust = 0.5,colour="black",face="bold",size=12), #设置x轴刻度标签的字体显示倾斜角度为45度，并向下调整1(hjust = 1)，字体大小为14
        axis.text.y=element_text(hjust=0, colour="black",face="bold",size=12), #设置y轴刻度标签的字体簇，字体大小，字体样式为 bold （加粗）
        axis.title.x=element_text(size=12,face="bold"),#设置x轴标题的字体属性
        axis.title.y=element_text(size=12,face="bold"))+
  theme(legend.position = "none")+
  theme(
    plot.background = element_rect(fill = NA), # 不填充背景色以突出边框
    panel.border = element_rect(colour = "black", fill = NA, size = 1) # 设置边框颜色、不填充内部以及边框粗细
  )
shrublandsSOS
ggsave("shrublands-SOS.png", plot = SOS, width = 4, height = 4, units = "in", dpi = 300)

shrublandsEOS<-ggviolin(data, x="ZU", y="EOS", fill="ZU",color="ZU",size=0.4,width = 0.8,alpha = 0.7,
                        add="boxplot",add.params = list(fill = "white"),
                        palette=c("#de773f","#d8caaf"),#"#8696a7","#c1cbd7"
                        xlab=" ", 
                        ylab="",
                        legend.title="EOS") + 
  # 差异性检验
  stat_compare_means(method="wilcox.test", paired = T, comparisons=list(c("urban", "nonurban")),label = "p.signif",size=8) +
  #添加均值点
  stat_summary(fun = "mean",geom = "point",pch = 24,size = 3,color = "black",fill="yellow")+ #pch点形状
  
  theme_article() + 
  # 设置主次网格线
  #theme(panel.grid.major = element_line(colour = "gray90"), 
  #panel.grid.minor = element_line(colour = "gray90")) + 
  # 设置轴标题和图标标题
  theme(axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 14, 
                                  face = "bold"), legend.title = element_text(face = "bold"))+
  
  theme(plot.title = element_text(hjust =0,colour="black",size=15), # 将图标题居中
        axis.text.x=element_text(hjust = 0.5,colour="black",face="bold",size=12), #设置x轴刻度标签的字体显示倾斜角度为45度，并向下调整1(hjust = 1)，字体大小为14
        axis.text.y=element_text(hjust=0, colour="black",face="bold",size=12), #设置y轴刻度标签的字体簇，字体大小，字体样式为 bold （加粗）
        axis.title.x=element_text(size=12,face="bold"),#设置x轴标题的字体属性
        axis.title.y=element_text(size=12,face="bold"))+
  theme(legend.position = "none")+
  theme(
    plot.background = element_rect(fill = NA), # 不填充背景色以突出边框
    panel.border = element_rect(colour = "black", fill = NA, size = 1) # 设置边框颜色、不填充内部以及边框粗细
  )
shrublandsEOS
ggsave("shrublands-EOS.png", plot = EOS, width = 4, height = 4, units = "in", dpi = 300)

#cropland-difference
setwd("H:/paper2-vegetation type/00-data/002-table/5-Rtable/1-different type")#设置工作路径
data <- read_excel("cropland-difference.xls", sheet = "Sheet1") 
data

library(ggpubr)
library(ggplot2)
library(ggsci)
library(cowplot)
library(ggpmisc)
library(egg)#图表主题theme_article()

croplandGSL<-ggviolin(data, x="ZU", y="GSL", fill="ZU",color="ZU",size=0.4,width = 0.8,alpha = 0.7,
                      add="boxplot",add.params = list(fill = "white"),
                      palette=c("#0E3461","#c1cbd7"),#"#8696a7","#c1cbd7"
                      xlab=" ", 
                      ylab="",
                      legend.title="GSL") + 
  # 差异性检验
  stat_compare_means(method="wilcox.test", paired = T, comparisons=list(c("urban", "nonurban")),label = "p.signif",size=8) +
  #添加均值点
  stat_summary(fun = "mean",geom = "point",pch = 24,size = 3,color = "black",fill="yellow")+ #pch点形状
  
  theme_article() + 
  # 设置主次网格线
  #theme(panel.grid.major = element_line(colour = "gray90"), 
  #panel.grid.minor = element_line(colour = "gray90")) + 
  # 设置轴标题和图标标题
  theme(axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 14, 
                                  face = "bold"), legend.title = element_text(face = "bold"))+
  
  theme(plot.title = element_text(hjust =0,colour="black",size=15), # 将图标题居中
        axis.text.x=element_text(hjust = 0.5,colour="black",face="bold",size=12), #设置x轴刻度标签的字体显示倾斜角度为45度，并向下调整1(hjust = 1)，字体大小为14
        axis.text.y=element_text(hjust=0, colour="black",face="bold",size=12), #设置y轴刻度标签的字体簇，字体大小，字体样式为 bold （加粗）
        axis.title.x=element_text(size=12,face="bold"),#设置x轴标题的字体属性
        axis.title.y=element_text(size=12,face="bold"))+
  theme(legend.position = "none")+
  theme(
    plot.background = element_rect(fill = NA), # 不填充背景色以突出边框
    panel.border = element_rect(colour = "black", fill = NA, size = 1) # 设置边框颜色、不填充内部以及边框粗细
  )
croplandGSL
ggsave("cropland-GSL.png", plot = GSL, width = 4, height = 4, units = "in", dpi = 300)

croplandSOS<-ggviolin(data, x="ZU", y="SOS", fill="ZU",color="ZU",size=0.4,width = 0.8,alpha = 0.7,
                      add="boxplot",add.params = list(fill = "white"),
                      palette=c("#7b8b6f","#D0DBCF"),#"#8696a7","#c1cbd7"
                      xlab=" ", 
                      ylab="",
                      legend.title="SOS") + 
  # 差异性检验
  stat_compare_means(method="wilcox.test", paired = T, comparisons=list(c("urban", "nonurban")),label = "p.signif",size=8) +
  #添加均值点
  stat_summary(fun = "mean",geom = "point",pch = 24,size = 3,color = "black",fill="yellow")+ #pch点形状
  
  theme_article() + 
  # 设置主次网格线
  #theme(panel.grid.major = element_line(colour = "gray90"), 
  #panel.grid.minor = element_line(colour = "gray90")) + 
  # 设置轴标题和图标标题
  theme(axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 14, 
                                  face = "bold"), legend.title = element_text(face = "bold"))+
  
  theme(plot.title = element_text(hjust =0,colour="black",size=15), # 将图标题居中
        axis.text.x=element_text(hjust = 0.5,colour="black",face="bold",size=12), #设置x轴刻度标签的字体显示倾斜角度为45度，并向下调整1(hjust = 1)，字体大小为14
        axis.text.y=element_text(hjust=0, colour="black",face="bold",size=12), #设置y轴刻度标签的字体簇，字体大小，字体样式为 bold （加粗）
        axis.title.x=element_text(size=12,face="bold"),#设置x轴标题的字体属性
        axis.title.y=element_text(size=12,face="bold"))+
  theme(legend.position = "none")+
  theme(
    plot.background = element_rect(fill = NA), # 不填充背景色以突出边框
    panel.border = element_rect(colour = "black", fill = NA, size = 1) # 设置边框颜色、不填充内部以及边框粗细
  )
croplandSOS
ggsave("cropland-SOS.png", plot = SOS, width = 4, height = 4, units = "in", dpi = 300)

croplandEOS<-ggviolin(data, x="ZU", y="EOS", fill="ZU",color="ZU",size=0.4,width = 0.8,alpha = 0.7,
                      add="boxplot",add.params = list(fill = "white"),
                      palette=c("#de773f","#d8caaf"),#"#8696a7","#c1cbd7"
                      xlab=" ", 
                      ylab="",
                      legend.title="EOS") + 
  # 差异性检验
  stat_compare_means(method="wilcox.test", paired = T, comparisons=list(c("urban", "nonurban")),label = "p.signif",size=8) +
  #添加均值点
  stat_summary(fun = "mean",geom = "point",pch = 24,size = 3,color = "black",fill="yellow")+ #pch点形状
  
  theme_article() + 
  # 设置主次网格线
  #theme(panel.grid.major = element_line(colour = "gray90"), 
  #panel.grid.minor = element_line(colour = "gray90")) + 
  # 设置轴标题和图标标题
  theme(axis.title = element_text(face = "bold"), 
        plot.title = element_text(size = 14, 
                                  face = "bold"), legend.title = element_text(face = "bold"))+
  
  theme(plot.title = element_text(hjust =0,colour="black",size=15), # 将图标题居中
        axis.text.x=element_text(hjust = 0.5,colour="black",face="bold",size=12), #设置x轴刻度标签的字体显示倾斜角度为45度，并向下调整1(hjust = 1)，字体大小为14
        axis.text.y=element_text(hjust=0, colour="black",face="bold",size=12), #设置y轴刻度标签的字体簇，字体大小，字体样式为 bold （加粗）
        axis.title.x=element_text(size=12,face="bold"),#设置x轴标题的字体属性
        axis.title.y=element_text(size=12,face="bold"))+
  theme(legend.position = "none")+
  theme(
    plot.background = element_rect(fill = NA), # 不填充背景色以突出边框
    panel.border = element_rect(colour = "black", fill = NA, size = 1) # 设置边框颜色、不填充内部以及边框粗细
  )
croplandEOS
ggsave("cropland-EOS.png", plot = EOS, width = 4, height = 4, units = "in", dpi = 300)