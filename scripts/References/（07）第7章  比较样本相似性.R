

###==================================###
###  【第7章】比较样本相似性         ###
###==================================###



#####================================================================#####
#####  7.1  比较整体相似性
#####================================================================#####

#####————————————————————————————————#####
##### 【图7-1】的绘制代码——ggplot2包绘制平行坐标图
#####————————————————————————————————#####
# 图7-1的绘制代码
library(ggplot2)
library(reshape2)
library(dplyr)

# 处理数据
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
df<-data7_1%>%
  mutate(地区=factor(地区,ordered=TRUE,levels=地区))%>% # 设置类别顺序
  melt(id.vars=c("地区","地带划分","区域划分"),
      variable.name="支出项目",value.name="支出金额")# 将数据融合成长格式

# 设置图形主题
mytheme<-theme(axis.text.y=element_text(angle=90,hjust=0.5),# 设置y轴刻度标签角度
      axis.text=element_text(size=6.5),    # 设置坐标轴刻度标签字体大小
      legend.position="inside",legend.justification=c("right","top"),# 设置图例位置为内部右上
      legend.direction="horizontal",       # 设置图例水平摆放
      legend.text=element_text(size=7),    # 设置图例字体大小
      legend.key.height=unit(0.2,"cm"),    # 设置图例键高度
      legend.background=element_blank())   # 设置图例背景色

# 图（a）31个地区
p1<-ggplot(df,aes(x=支出项目,y=支出金额,group=地区,color=地区))+
  geom_line(linewidth=0.5)+                      # 绘制折线
  geom_point(shape=21,size=1.5,fill="gray50")+   # 绘制点
  guides(color=guide_legend(nrow=8,title=NULL))+# 设置图例摆放方式，删除图例标题
  mytheme+ggtitle("(a) 31个地区")

# 图（b）按地带划分分组
p2<-ggplot(df,aes(x=支出项目,y=支出金额,group=地区,color=地带划分))+
  geom_line(linewidth=0.5)+
  geom_point(shape=21,size=1.5,fill="gray50")+
  scale_x_discrete(guide=guide_axis(n.dodge=2))+
  guides(color=guide_legend(nrow=3,title=NULL))+ # 图例排成3行,去掉图例标题
  mytheme+ggtitle("(b) 按地带划分分组")
 
# 图（c）按区域划分分组
p3<-p2+aes(x=支出项目,y=支出金额,group=地区,color=区域划分)+
  ggtitle("(c) 按区域划分分组")

p1/free((p2+p3))


#####————————————————————————————————#####
##### 【图7-2】的绘制代码——分面绘制
#####————————————————————————————————#####
# 图7-2的绘制代码(使用图7-1构建的数据框df)
library(ggplot2)
library(reshape2)
library(dplyr)
library(patchwork)

# 处理数据
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
df<-data7_1%>%
  mutate(地区=factor(地区,ordered=TRUE,levels=地区))%>%
  melt(id.vars=c("地区","地带划分","区域划分"),
       variable.name="支出项目",value.name="支出金额")

p1<-ggplot(df,aes(x=支出项目,y=支出金额,group=地区,color=地带划分))+
  geom_line(linewidth=0.5)+
  geom_point(shape=21,size=1.5,fill="gray50")+
  scale_x_discrete(guide=guide_axis(n.dodge=2))+ # 设置x轴标签为2行
  theme(axis.text.x=element_text(size=7))+
  facet_grid(地带划分~.,scale="free_y")+
  guides(color="none")+
  ggtitle("(a) 按地带划分分面")
 
p2<-p1+aes(x=支出项目,y=支出金额,group=地区,color=区域划分)+
  facet_grid(区域划分~.,scale="free")+
  ggtitle("(b) 按区域划分分面")

p1+p2+plot_layout(axis_titles="collect_x")  # 组合图形并共享x轴标题


#####————————————————————————————————#####
##### 【图7-3】的绘制代码——使用ggiraphExtra包绘制平行坐标图
#####————————————————————————————————#####
# 图7-3的绘制代码（按地带划分分组的食品烟酒和居住的比较）
library("ggiraphExtra")
require(ggplot2)
library(patchwork)
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")

# 图（a）按地带划分分组
mytheme<-theme_grey()+
theme(legend.position="inside",legend.justification=c("left","top"),
      legend.key.height=unit(0.45,"cm"),    # 设置图例键高度
      legend.background=element_blank(),    # 移除图例整体边框
      axis.text.y=element_text(angle=90,hjust=0.5))

p1<-ggPair(data7_1,aes(x=c(食品烟酒,居住),color=地带划分))+ # 按地带划分分组
  guides(color=guide_legend(nrow=3,title=NULL))+ # 图例排成3行,去掉图例标题
  mytheme+
  labs(x="支出项目",y="支出金额",title="(a) 按地带划分分组")

# 图（b）按区域划分分组
p2<-ggPair(data7_1,aes(x=c(食品烟酒,居住),color=区域划分))+# 按区域划分分组
  guides(color=guide_legend(nrow=3,title=NULL))+
  mytheme+
  labs(x="支出项目",y="支出金额",title="(b) 按区域划分分组")

p1+p2+plot_layout(axis_titles="collect_y")# 组合图形并删除重复的y轴标题


#####————————————————————————————————#####
##### 【图7-4】的绘制代码——带有分组直方图与核密度图的平行坐标图
#####————————————————————————————————#####
# 图7-4的绘制代码（按地带划分分组的4项支出的比较）
library(ggmulti);library(ggplot2);library(patchwork)
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")

p<-ggplot(data7_1,aes(食品烟酒=食品烟酒,衣着=衣着,居住=居住,
                 交通通信=交通通信,colour=地带划分))+  # 设置绘图变量
  geom_path(alpha=0.3)+              # 按观察值在数据中出现的顺序连接
  theme(axis.text.x = element_text(hjust =0), # 调整x轴标签位置
      legend.position="inside",legend.justification=c("left","top"),
      legend.key.height=unit(0.3,"cm"),       # 设置图例键高度
      legend.key.width=unit(0.3,"cm"),        # 设置图例键高度
      legend.background=element_blank())+     # 移除图例整体边框
   labs(fill=NULL)+guides(color="none")+
   coord_serialaxes()                      # 设置平行坐标

p1<-p+ggtitle("(a) 平行坐标图+分组直方图")+
  geom_histogram(alpha=0.5,aes(fill=地带划分)) # 按地带划分分组绘制直方图

p2<-p+ggtitle("(b) 平行坐标图+分组核密度图")+
  geom_density(alpha= 0.5,aes(fill=地带划分))  # 按地带划分分组绘制核密度图

# 组合图形
p1+p2


#####————————————————————————————————#####
##### 【图7-5】的绘制代码——雷达图
#####————————————————————————————————#####
# 图7-5的绘制代码（北京、天津、上海的比较）
library(ggiraphExtra)
library(ggplot2)
library(dplyr)
library(patchwork)

# 处理数据
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
df<-filter(data7_1,地区%in%c("北京","天津","上海")) # %in%表示选出向量为c("北京","天津","上海")的地区

# 图（a）使用原始数据
mytheme<-theme_bw()+                  # 使用黑白主题
   theme(legend.position="bottom",    # 设置图例位置
   axis.text.x=element_text(size=8,angle=seq(-20,-340,length.out=8)))
                                  # 设置坐标轴标签字体大小、颜色和和角度
p1<-ggRadar(data=df,aes(group=地区),    # 按地区分组
  rescale=FALSE,                        # 数据不归一化
  ylim=c(-200,20000),                   # 设置y轴范围
  alpha=0,                              # 设置颜色透明度
  size=2)+                              # 设置点的大小
  mytheme+
  labs(x="支出项目",y="支出金额",title="(a) 原始数据雷达图")

# 图（b）使用归一化数据
p2<-ggRadar(data=df,aes(group=地区),    # 按地区分组
  rescale=TRUE,                         # 数据归一化(缩放到[0,1]范围)
  ylim=c(-0.3,1),                       # 设置y轴范围
  alpha=0.3,                            # 设置颜色透明度
  size=2)+                              # 设置点的大小
  mytheme+
  labs(x="支出项目",y="归一化值",title="(b) 归一化雷达图")

p1+p2       # 组合图形


#####————————————————————————————————#####
##### 【图7-6】的绘制代码——分面雷达图
#####————————————————————————————————#####
# 图7-6的绘制代码
library(ggiraphExtra)
library(ggplot2)

# 处理数据
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
df<-dplyr::filter(data7_1,区域划分=="中南")   # 选出中南地区的省份

ggRadar(data=df,aes(group=地区,facet=地区),    # 按地区分面
  rescale=TRUE,alpha=0.3,size=2.5)+
  coord_radial()+          # 使用ggplot2提供的极坐标函数     
  theme(legend.position="none",
        axis.text.x=element_text(size=6,
        angle=seq(-20,-340,length.out=8)))


#####————————————————————————————————#####
##### 【图7-7】的绘制代码——按地带划分分组的雷达图
#####————————————————————————————————#####
# 图7-7的绘制代码
library(ggiraphExtra)
library(ggplot2)
library(patchwork)

data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")

  mytheme<-theme(legend.position="bottom",
                 axis.text.x=element_text(size=8,
                 angle=seq(-20,-340,length.out=8)))

p1<-ggRadar(data=data7_1,rescale=FALSE,aes(group=地带划分),
  alpha=0.1,size=2.5)+    # 按地带划分分组
  mytheme+guides(color=guide_legend(nrow=2))+# 图例排成2行
  coord_radial()+          # 使用ggplot2包的极坐标系统函数
  labs(x="支出项目",y="支出金额",title="(a) 按地带划分分组")

p2<-ggRadar(data=data7_1,rescale=FALSE,aes(group=区域划分),
  alpha=0.1,size=2.5)+   #  按区域划分分组
  mytheme+guides(color=guide_legend(nrow=2))+ 
  coord_radial()+   
  labs(x="支出项目",y="支出金额",title="(b) 按区域划分分组")

p1+p2     # 组合图形




#####================================================================#####
#####  7.2  比较样本间差异
#####================================================================#####

#####————————————————————————————————#####
##### 【图7-8】的绘制代码——星图(填充颜色)
#####————————————————————————————————#####
# 图7-8的绘制代码
library(dplyr)
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
mat<-data7_1%>%
  select(-c(地区,区域划分,地带划分))%>%
  as.matrix() # 转换成矩阵
rownames(mat)=data7_1[,1]                    # 设置矩阵行名称

# 图7-8（a）星图
par(mfrow=c(1,2),cex.main=1.1,font.main=1)
stars(mat,
   full=TRUE,                                # 绘制出满圆
   scale=TRUE,                               # 将列变量数据缩放到[0,1]范围
   nrow=5,                                   # 5行布局
   len=1,                                    # 设置半径或线段长度的比例
   frame.plot=TRUE,                          # 添加边框
   draw.segments=TRUE,key.loc=c(12.8,1.7,5), # 绘制线段图，并设置位置
   mar=c(0.5,0.1,2,0.1),                     # 设置图形边距
   cex=0.6,                                  # 设置标签字体大小
   main="(a) 星图")                          # 添加标题
 
# 图7-8（b）线条星图
stars(mat,
   full=TRUE,                                # 绘制出满圆
   scale=TRUE,                               # 将数据缩放到[0,1]的范围
   nrow=5,                                   # 5行布局
   len=1,                                    # 设置半径或线段长度的比例
   draw.segments=FALSE,key.loc=c(12.8,1.7,5), # 绘制线段图，并设置位置
   col.stars = rainbow(31),                  # 设置每个星（样本）的颜色
   frame.plot=TRUE,                          # 添加边框
   mar=c(0.5,0.1,2,0.1),                     # 设置图形边界
   cex=0.6,                                  # 设置标签字体大小
   main="(b) 线条星图")                      # 添加标题


#####————————————————————————————————#####
##### 【图7-9】的绘制代码——散点星图
#####————————————————————————————————#####
# 图7-9的绘制代码（使用图6-8构建的矩阵mat）
library(dplyr)
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
mat<-data7_1%>%
  select(-c(地区,区域划分,地带划分))%>%
  as.matrix() # 转换成矩阵
rownames(mat)=data7_1[,1]                    # 设置矩阵行名称

loc<-data.matrix(data.frame(data7_1$食品烟酒/4000,data7_1$医疗保健/2600))
                                             # 设置星图的位置（x轴和y轴）
stars(mat,
   location=loc,
   full=TRUE,scale=TRUE,len=0.08,
   frame.plot=TRUE,                          # 添加边框
   draw.segments=TRUE,key.loc=c(3,0.35,2),axes = TRUE,
   xlab="食品烟酒/4000",ylab="医疗保健/2600",xlim=c(1.5,3),
   mar=c(4,4,1,0.5),                         # 设置图形边界
   cex=0.7)                                  # 设置标签字体大小


#####————————————————————————————————#####
##### 【图7-10】的绘制代码——脸谱图
#####————————————————————————————————#####
# 图7-10的绘制代码（使用图6-8构建的矩阵mat）
library(aplpack)
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
mat<-data7_1%>%
  select(-c(地区,区域划分,地带划分))%>%
  as.matrix() # 转换成矩阵
rownames(mat)=data7_1[,1]  # 设置矩阵行名称

faces(mat,face.type=1,              # 设置脸谱图的类型
   ncol.plot=8,                     # 绘制成7列
   scale=TRUE,                      # 数据标准化
   cex=1)                           # 设置脸谱图标签字体的大小


#####————————————————————————————————#####
##### 【图7-11】的绘制代码——散点脸谱散
#####————————————————————————————————#####
# 图7-11的绘制代码（使用图7-8构建的矩阵mat）
library(aplpack)
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
mat<-data7_1%>%
  select(-c(地区,区域划分,地带划分))%>%
  as.matrix() # 转换成矩阵
rownames(mat)=data7_1[,1]                    # 设置矩阵行名称

par(mai=c(0.8,0.8,0.1,0.1),cex=0.8)
plot(mat[1:31,c(1,7)],xlim=c(5000,14000),ylim=c(500,5500),
  bty="n",type="n")# 绘制食品烟酒和医疗保健的散点图空图
f<-faces(mat[1:31,],plot=FALSE)        # 绘制脸谱图的空图

plot.faces(f,mat[1:31,1],mat[1:31,7],  # 绘制脸谱散点图
  width=600,height=400,cex=0.8)   # 设置脸谱图的宽度、高度和标签字体大小
  

#####————————————————————————————————#####
##### 【图7-12】的绘制代码——散点饼图
#####————————————————————————————————#####
# 图7-12的绘制代码
library(ggplot2)
library(scatterpie)
library(patchwork)

# 处理数据
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
df<-transform(data7_1[,-c(2,3)],合计=rowSums(data7_1[4:11])/1000) # 插入合计列并将结果/1000
df$地区<-as.numeric(factor(df$地区,ordered=T,levels=df$地区))# 设置地区顺序并转换成数值
f<-c("食品烟酒","衣着","居住","生活用品及服务",
     "交通通信","教育文化娱乐","医疗保健","其他用品及服务")# # 设置支出项目顺序
cols<-factor(f,ordered=T,levels=f)   # 设置填充颜色

# 绘制散点饼图
p1<-ggplot()+geom_scatterpie(data=df,aes(x=地区,y=合计),color="grey40",# 绘制散点饼图
  cols=cols,                                        # 设置饼图填充颜色标签   
  pie_scale=1.8)+                                   # 设置饼的大小
  labs(x="地区",y="支出金额/1000",title="(a) 饼的半径相同")+
  scale_x_discrete(limits=unique(data7_1$地区))+  # 设置x轴刻度标签
  guides(fill=guide_legend(nrow=1,title=NULL))+           # 图例排成1行
  coord_fixed()+                                  # 固定坐标轴比例
  theme(axis.text.x=element_text(size=8,angle=90,hjust=0.5,vjust=0.5))

p2<-ggplot()+
  geom_scatterpie(data=df,aes(x=地区,y=合计,r=合计/20),
                  color="grey40",# 绘制散点饼图,半径大小=GDP/20
  cols=cols)+
  labs(x="地区",y="支出金额/1000",title="(b) 饼的半径与支出总金额成正比")+
  scale_x_discrete(limits=unique(data7_1$地区))+  # 设置x轴刻度标签
  geom_scatterpie_legend(df$合计/15,x=23,y=55)+  # 添加图例
  geom_hline(yintercept=mean(df$合计),color="grey30",linewidth=0.3)+ # 添加y的均值线
  coord_fixed()+guides(fill="none")+
  theme(axis.text.x=element_text(size=8,angle=90,hjust=0.5,vjust=0.5))

# 组合图形并共享图例
p1+p2+plot_layout(guides="collect")&theme(legend.position="bottom")



#####================================================================#####
#####  7.3  对样本进行分类
#####================================================================#####

#####————————————————————————————————#####
##### 【图7-13】的绘制代码——层次聚类树—factoextra包绘制
#####————————————————————————————————#####
# 图7-13的绘制代码
library(factoextra)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(patchwork)

# 处理数据
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
mat<-data7_1%>%
  select(-c(地区,区域划分,地带划分))%>%as.matrix() # 转换成矩阵
rownames(mat)=data7_1[,1]                    # 设置矩阵行名称

d<-dist(scale(mat),method="euclidean")# 采用euclidean距离计算样本的点间距离
hc<-hclust(d,method="ward.D2")
                          # 采用ward.D法计算类间距离并用层次聚类法聚类
cols=brewer.pal(4,"Set1")  

# 图（a）垂直摆放
p1<-fviz_dend(hc,k=4,                          # 设置分类数
          cex=0.6,                             # 设置数据标签的字体大小
          horiz=FALSE,                         # 垂直摆放图形
          k_colors=brewer.pal(4,"Set1"),       # 设置聚类集群的线条颜色
          color_labels_by_k=TRUE,              # 自动设置数据标签颜色
          lwd=0.5,                             # 设置分支和矩形的线宽
          type="rectangle",                    # 设置绘图类型为矩形
          rect=TRUE,                           # 绘制聚类集群矩形
          #rect_border=brewer.pal(4,"Set1"),   # 使用不同的颜色矩形标记类别
          rect_fill=TRUE,                      # 设置标记框的填充颜色
          main="(a) 垂直摆放")                 # 设置标题

# 图（b）水平摆放
p2<-fviz_dend(hc,k=4,                          # 设置分类数
          cex=0.6,                             # 设置数据标签的字体大小
          horiz=TRUE,                          # 水平摆放图形
          k_colors=brewer.pal(4,"Set1"),       # 设置聚类集群的线条颜色
          color_labels_by_k=TRUE,              # 自动设置数据标签颜色
          lwd=0.6,                             # 设置分支和矩形的线宽
          type="rectangle",                    # 设置绘图类型为矩形
          rect=TRUE,                           # 绘制聚类集群矩形
          #rect_border=brewer.pal(4,"Set1"),   # 使用不同的颜色矩形标记类别
          rect_fill=TRUE,                      # 设置标记框的填充颜色
          main="(b) 水平摆放")                 # 显示标题

p1+p2+plot_layout(widths=c(2,1))  # 组合图形，列宽比为2：1


#####————————————————————————————————#####
##### 【图7-14】的绘制代码——层次聚类树—factoextra包绘制
#####————————————————————————————————#####
# 图7-14（a）的绘制代码（4类）
p1<-fviz_dend(hc,k=4,                          # 分成4类
          cex=0.7,                             # 设置数据标签的总体大小
          horiz=FALSE,                         # 垂直摆放图形
          k_colors=brewer.pal(4,"Set1")  ,     # 设置聚类集群的线条颜色
          color_labels_by_k=TRUE,              # 自动设置数据标签颜色
          lwd=0.8,                             # 设置分支和矩形的线宽
          type="circular",                     # 设置绘图类型为矩形
          rect=TRUE,                           # 使用不同的颜色矩形标记类别
          rect_lty=1,rect_fill=TRUE)+          # 设置标记框的线型和填充颜色
     ggtitle("(a) 圆形")

# 图7-15（b）的绘制代码（4类）
p2<-fviz_dend(hc,k=4,                          # 分成4类
          cex=0.8,                             # 设置数据标签的大小
          horiz=FALSE,                         # 垂直摆放图形
          k_colors=brewer.pal(4,"Set1")  ,     # 设置聚类集群的线条颜色
          color_labels_by_k=TRUE ,             # 自动设置数据标签颜色
          lwd=0.8,                             # 设置分支和矩形的线宽
          type="phylogenic",                   # 设置绘图类型为矩形
          rect=TRUE,                           # 使用不同的颜色矩形标记类别
          repel=TRUE,                          # 避免图中的文本标签重叠
          rect_lty=1,rect_fill=TRUE)+          # 设置标记框的线型和填充颜色
     ggtitle("(b) 植物形")

p1+p2  # 组合图形


#####————————————————————————————————#####
##### 【图7-15】的绘制代码——层次聚类树（网络图形式）
#####————————————————————————————————#####
# 图7-15（a）的绘制代码（使用图7-13的层次聚类结果hc）
library(networkD3)
library(dplyr)

# 处理数据
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
mat<-data7_1%>%
  select(-c(地区,区域划分,地带划分))%>%as.matrix() # 转换成矩阵
rownames(mat)=data7_1[,1]                    # 设置矩阵行名称

d<-dist(scale(mat),method="euclidean")# 采用euclidean距离计算样本的点间距离
hc<-hclust(d,method="ward.D2")
                          # 采用ward.D法计算类间距离并用层次聚类法进行聚类

# 图（a）垂直展示
dendroNetwork(hc,
   height=500,width=600,   # 网络图框架区域的高度和宽度(以像素为单位)
   fontSize=8,             # 设置节点文本标签的数字字体大小（以像素为单位）
   #linkColour="black",    # 设置链接线的颜色
   nodeColour="lightgreen",# 设置节点圆的填充颜色
   #nodeStroke="green",    # 设置节点圆圈的颜色
   textColour=c("black","red","green","blue")[cutree(hc,4)],
                           # 分成4类，设置文本标签的颜色与类别匹配
   #textOpacity=1,         # 设置文本标签颜色的透明度
   textRotate=90,          # 设置文本标签旋转的度数
   opacity=1,              # 设置节点的透明度
   linkType="diagonal",    # 设置连线类型为对角线（可选肘型elbow）
   treeOrientation="vertical")# 设置树图的方向为垂直（设置horizontal为水平)

# 图（b）水平展示
dendroNetwork(hc,
   height=500,width=600,   # 网络图框架区域的高度和宽度(以像素为单位)
   fontSize=8,             # 设置节点文本标签的数字字体大小（以像素为单位）
   #linkColour="black",    # 设置链接线的颜色
   nodeColour="lightgreen",# 设置节点圆的填充颜色
   #nodeStroke="green",    # 设置节点圆圈的颜色
   textColour=c("black","red","green","blue")[cutree(hc,4)],
                           # 分成4类，设置文本标签的颜色与类别匹配
   #textOpacity=1,         # 设置文本标签颜色的透明度
   textRotate=0,           # 设置文本标签旋转的度数
   opacity=1,              # 设置节点的透明度
   linkType="diagonal",    # 设置连线类型为对角线（可选肘型elbow）
   treeOrientation="horizontal")# 设置树图的方向为水平


#####————————————————————————————————#####
##### 【图7-16】的绘制代码——k-means聚类图
#####————————————————————————————————#####
# 图7-16的绘制代码
library(factoextra)
library(ggplot2)
library(dplyr)
library(patchwork)

data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
mat<-data7_1%>%
  select(-c(地区,区域划分,地带划分))%>%
  as.matrix()
rownames(mat)=data7_1[,1]                     # 设置矩阵行名称

# 图（a）分成4类
set.seed(12)                                  # 设置随机数种子
km<-kmeans(mat,centers=4)                     # 分成4类
p1<-fviz_cluster(km,mat[,-1],
   repel=TRUE,                                # 避免图中的文本标签重叠
   ellipse.type="confidence",                 # 画出置信椭圆
   labelsize=9,                               # 设置标签字体大小
   pointsize=2,                               # 设置中心点大小
   ggtheme=theme_test(),                      # 设置主题
   legend="bottom",                           # 设置图例位置
   main = "(a) 分成4类")
   
# 图（b）分成3类
km<-kmeans(mat,centers=3)                     # 分成3类
p2<-fviz_cluster(km,mat[,-1],repel=TRUE,ellipse.type="convex",
   ggtheme=theme_test(),legend="bottom",labelsize=8,main="(b) 分成3类")

p1+p2         # 组合图形


#####————————————————————————————————#####
##### 【图7-17】的绘制代码——热图——可视化数值大小
#####————————————————————————————————#####
# 图7-17的绘制代码
## 安装包
## install.packages("devtools")
## devtools::install_github("jokergoo/ComplexHeatmap")

library(ComplexHeatmap)
library(dplyr)

# 处理数据
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
mat<-data7_1%>%
  select(-c(地区,区域划分,地带划分))%>%
  as.matrix()# 转换成矩阵
rownames(mat)=data7_1[,1]                        # 设置矩阵行名称

# 绘制热图
p1<-Heatmap(t(mat),name="支出金额",            # 矩阵转置，设置矩阵名称
    height=unit(5,"cm"),                       # 设置图形高度
    cluster_rows=FALSE,cluster_columns=FALSE,  # 行列不分类
    row_title="2D热图",                        # 设置行标题名称
    row_title_side="left",                     # 设置行标题位置
    row_names_side ="left",                    # 设置行名称位置
    row_names_gp=gpar(fontsize=6),row_names_rot=0,# 设置行标签字体大小
    column_names_gp=gpar(fontsize=7),column_names_rot=90, # 设置列标签名称字体大小和旋转角度
    show_heatmap_legend=TRUE)                  # 显示图例

p2<-Heatmap3D(t(mat),name="支出金额",          # 矩阵转置，设置矩阵名称
    height=unit(5,"cm"),                       # 设置图形高度
    cluster_rows=FALSE,cluster_columns=FALSE,  # 行列不分类
    row_title="3D热图",                        # 设置行标题名称
    row_title_side="left",                     # 设置行标题位置
    row_names_side ="left",                    # 设置行名称位置
    row_names_gp=gpar(fontsize=6),row_names_rot=0,# 设置行标签名称字体大小
    column_names_gp=gpar(fontsize=7),column_names_rot=90, # 设置列标签字体大小和角度
    show_heatmap_legend=TRUE)                  # 显示图例

ht=p1 %v% p2                    # 垂直组合图形
draw(ht,ht_gap=unit(0.6,"cm"))  # 打印图形并设置子图间隔


#####————————————————————————————————#####
##### 【图7-18】的绘制代码——热图——可视化数据分布（频数分布）
#####————————————————————————————————#####
# 图7-18的绘制代码（使用图7-17构建的矩阵mat）
library(ComplexHeatmap)
library(dplyr)

# 处理数据
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
mat<-data7_1%>%
  select(-c(地区,区域划分,地带划分))%>%
  as.matrix()# 转换成矩阵
rownames(mat)=data7_1[,1]                        # 设置矩阵行名称

# 图（a）2D频数直方图（热图）
p1<-frequencyHeatmap(mat,stat="count",# 设置绘图数据为计数（可选密度或比例）
    column_title="(a) 2D频数分布直方图",  # 设置列标题名称
    column_names_gp=gpar(fontsize=7),column_names_rot=20,
    ylab="支出金额",                      # 设置y轴标签
    show_heatmap_legend=TRUE)

# 图（b）3D频数直方图（热图）
p2<-frequencyHeatmap(mat,use_3d = TRUE,stat="count",
    column_title="(b) 3D频数分布直方图",
    column_names_gp=gpar(fontsize=7),column_names_rot=20,
    ylab="支出金额",
    show_heatmap_legend=TRUE)

p1+p2         # 水平组合图形


#####————————————————————————————————#####
##### 【图7-19】的绘制代码——热图——可视化数据分布——密度分布热图
#####————————————————————————————————#####
# 图7-19的绘制代码（使用图7-17构建的矩阵mat）
library(ComplexHeatmap)
library(dplyr)

# 处理数据
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
mat<-data7_1%>%
  select(-c(地区,区域划分,地带划分))%>%
  as.matrix()# 转换成矩阵
rownames(mat)=data7_1[,1]                        # 设置矩阵行名称

# 图（a）密度分布-热图
p1<-densityHeatmap(mat,width=unit(5,"cm"),       # 设置图形宽度
    column_title="(a) 支出项目密度分布",         # 设置列标题名称
    column_names_gp=gpar(fontsize=5),column_names_rot=20,
    ylab="支出金额",                # 设置y轴标签
    show_heatmap_legend=TRUE)


# 图（b）密度分布-热图
p2<-densityHeatmap(t(mat),
    column_title="(b) 地区密度分布",
    column_names_gp=gpar(fontsize=5),column_names_rot=90,
    ylab="支出金额",
    show_heatmap_legend=TRUE)

ht<-p1+p2         # 水平组合图形
draw(ht,ht_gap=unit(0,"cm"))  # 打印图形并设置子图间隔


#####————————————————————————————————#####
##### 【图7-20】的绘制代码——聚类热图
#####————————————————————————————————#####
# 图7-20的绘制代码（使用图7-17构建的矩阵mat）
library(ComplexHeatmap)
library(dplyr)

# 处理数据
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
mat<-data7_1%>%
  select(-c(地区,区域划分,地带划分))%>%
  as.matrix()# 转换成矩阵
rownames(mat)=data7_1[,1]          # 设置矩阵行名称

# 图（a）地区分4类,支出项目分2类
p1<-Heatmap(mat,name="支出金额",   # 设置矩阵名称
    row_km=4,column_km=2,          # 行分??4类，列分成2类
    row_gap=unit(0.5,"mm"),        # 设置行类别间隔
    column_gap=unit(1,"mm"),       # 设置列类别间隔
    column_title="(a) 地区分4类,支出项目分2类",  # 设置标题名称
    column_title_gp=gpar(fontsize=13), # 设置标题字体大小
    column_names_gp=gpar(fontsize=7),column_names_rot=20,# 设置列名字体大小和角度
    row_names_gp=gpar(fontsize=6), # 设置行名字体大小
    show_heatmap_legend=TRUE)      # 显示图例

# 图（b）地区分4类,支出项目分3类
p2<-Heatmap(mat,name="支出金额",row_km=4,column_km=3,# 行分成4类，列分成3类
    row_gap=unit(0.5,"mm"),column_gap=unit(1,"mm"),
    column_title="(b) 地区分4类,支出项目分3类",
    column_title_gp=gpar(fontsize=13),# col="black",fill="gray"
    column_names_gp=gpar(fontsize=7),column_names_rot=20,
    row_names_gp=gpar(fontsize=6), # 设置行名字体大小
    show_heatmap_legend=TRUE)      # 显示图例

p1+p2   # 组合圆形


#####————————————————————————————————#####
##### 【图7-21】的绘制代码——密度分布聚类热图
#####————————————————————————————————#####
# 图-21的绘制代码（使用图6-17构建的矩阵mat）
library(ComplexHeatmap)
library(dplyr)

# 处理数据
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
mat<-data7_1%>%
  select(-c(地区,区域划分,地带划分))%>%
  as.matrix()# 转换成矩阵
rownames(mat)=data7_1[,1]                     # 设置矩阵行名称

# 图（a）地区分4类的密度分布聚类热图
p1<-densityHeatmap(t(mat),column_km=4,        # 地区分4类
    #column_gap=unit(0.5,"mm"),       # 设置列类别间隔
    clustering_distance_columns="ks",         # 使用"ks"距离
    column_title="(a) 地区分4类",
    column_names_gp=gpar(fontsize=6),column_names_rot=90,
    ylab="支出金额",
    show_heatmap_legend=F)                    # 不显示图例

# 图（b）支出项目分3类的密度分布聚类热图
p2<-densityHeatmap(mat,width=unit(6,"cm"),    # 设置图形宽度
    column_km=3,                              # 支出项目分3类
    #column_gap=unit(0.5,"mm"),       # 设置列类别间隔
    clustering_distance_columns="ks",         
    column_title="(b) 支出项目分3类",
    column_names_gp=gpar(fontsize=6),column_names_rot=20,
    ylab="支出金额",
    show_heatmap_legend=F)

ht<-p1+p2         # 水平组合图形
draw(ht,ht_gap=unit(0,"cm"))


#####————————————————————————————————#####
##### 【图7-22】的绘制代码——组合聚类热图
#####————————————————————————————————#####
# 图7-22的绘制代码（使用图6-17构建的矩阵mat）
library(ComplexHeatmap)
library(dplyr)

# 处理数据
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
mat<-data7_1%>%
  select(-c(地区,区域划分,地带划分))%>%
  as.matrix()# 转换成矩阵
rownames(mat)=data7_1[,1]                       # 设置矩阵行名称

# 创建注释
# 注释分类区块
block<-HeatmapAnnotation(bar=anno_block(gp=gpar(fill=2:5),# 设置填充颜色
   labels=c("block1","block2","block3","block4"),  # 设置标签
   labels_gp=gpar(col="black",fontsize=7)))        # 设置字体大小和颜色

# 注释条形图
bar1<-rowAnnotation(bar=anno_barplot(t(mat), # 绘制行注释条形图（支出项目）
   gp=gpar(fill=rainbow(31))))               # 设置填充颜色
bar2<-HeatmapAnnotation(bar=anno_barplot(mat,# 绘制列注释条形图（地区）
   gp=gpar(fill=rainbow(8)))) 

# 注释小提琴图
violin=HeatmapAnnotation(violin=anno_density(t(mat),type="violin",
   height=unit(1.5,"cm"),gp=gpar(fill=rainbow(31))))

# 绘制热图并添加注释
p1<-Heatmap(t(mat),name="支出金额",height=unit(4.5,"cm"),
    row_km=FALSE,column_km=4,column_gap=unit(0.5,"mm"),# 设置列类别数和类别间隔
    show_row_dend=TRUE,             # 显示行聚类树
    show_column_dend=FALSE,         # 不显示列聚类树
    top_annotation=block,           # 顶部注释(分类区块)
    right_annotation=bar1,          # 右侧注释(支出项目条形图)
    bottom_annotation=bar2,         # 底部注释(地区条形图)          
    row_names_gp=gpar(fontsize=8),  # 设置行标签字体大小
    column_title="注释分类区块+注释条形图+注释小提琴图")

p2<-densityHeatmap(t(mat),ylab="支出金额",  # 绘制密度分布图
    column_names_gp=gpar(fontsize=8),column_names_rot=90,
    top_annotation=violin)          # 顶部注释为小提琴图
   
ht<-p1%v%p2         # 垂直组合图形
draw(ht,ht_gap=unit(0,"cm"))


#####————————————————————————————————#####
##### 【图7-23】的绘制代码——圆形聚类热图（4.3.2）
#####————————————————————————————————#####
# 图7-23（a）的绘制代码（使用图6-17构建的矩阵mat）
library(circlize)
library(ComplexHeatmap)
library(dplyr)

# 处理数据
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
mat<-data7_1%>%
  select(-c(地区,地带划分,区域划分))%>%
  as.matrix()# 转换成矩阵
rownames(mat)=data7_1$地区                        # 设置矩阵行名称

# 图（a）31个地区未分组
# 图形设置
circos.par(gap.after=c(60))   # 调整圆环首尾间的距离，数值越大，距离越宽
col_fun1=colorRamp2(c(300,6000,18000),c("blue","white","red"))# 设置颜色函数

# 绘制聚类热图
circos.heatmap(mat,col=col_fun1,      # 设置颜色
   bg.border="gold",bg.lwd=2,bg.lty=1,# 设置背景边线颜色、线宽和线型
   dend.side="inside",                # 聚类树放在环形内侧（设置outside则显示在圆环外圈）
   dend.track.height=0.25,   # 设置聚类树高度
   track.height=0.35,        # 设置聚类热图高度    
   rownames.side="outside",  # 行标签名放在环形外侧（与dend.side不能在同一侧，必须一内一外）
   rownames.col="black",     # 行标签颜色
   rownames.cex=0.8,         # 行标签字体大小
   cell.border ="white",     # 单元格边线颜色 
   cluster=TRUE)             # 对行聚类(cluster=FALSE则不显示聚类树)
# 绘制图例
lgd=Legend(title="",col_fun=col_fun1)
grid.draw(lgd)

circos.clear()       # 重置圆形布局参数

# 图（b）按地带划分分组
color=colorRamp2(c(300,6000,18000),c("blue","white","red"))# 设置颜色
split=data7_1$地带划分          # 设置分区变量

# 绘制聚类热图
circos.heatmap(mat,col=color,split=split,
   bg.border="gold",bg.lwd=2,bg.lty=1,# 设置背景边线颜色、线宽和线型
   dend.side="inside", # 聚类树放在环形内侧（设置outside则显示在圆环外圈）
   dend.track.height=0.25,   # 设置聚类树高度
   track.height=0.35,        # 设置聚类热图高度    
   rownames.side="outside",  # 行标签名放在环形外侧（与dend.side不能在同一侧，必须一内一外）
   rownames.col="black",     # 行标签颜色
   rownames.cex=0.8,         # 行标签字体大小
   cell.border ="white",     # 单元格边线颜色 
   show.sector.labels=TRUE,  # 显示分区标签      
   cluster=TRUE)             # 对行聚类(cluster=FALSE则不显示聚类树)
# 绘制图例
lgd=Legend(title="",col_fun=color)
grid.draw(lgd)

circos.clear()       # 重置圆形布局参数







#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####
#####  END
#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####



