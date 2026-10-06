

###==================================###
###  【第5章】展示数据分布           ###
###==================================###



#####================================================================#####
#####  5.1  展示分布形状
#####================================================================#####

#####————————————————————————————————#####
##### 【图5-1】的绘制代码——普通直方图（AQI的直方图） 
#####————————————————————————————————#####
# 图5-1的绘制代码
library(ggplot2)
library(patchwork)
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")

# 绘制图形p1、p2和p3
p<-ggplot(data=data5_1,aes(x=AQI))

p1<-p+geom_histogram(fill="lightgreen",color="gray50")+ 
                            # 设置直方图的填充颜色和边框颜色
  ggtitle("(a) 默认分组")   # 设置标题（函数默认bins=30,即将数据分成30组）

p2<-p+geom_histogram(bins=15,fill="lightgreen",color="gray50")+
                                      #  指定分成15组
    ggtitle("(b) 分成15组")

p3<-p+geom_histogram(binwidth=20,fill="lightgreen",color="gray50")+# 指定箱宽（组距）为20
   ggtitle("(c) 箱宽为20")

p1+p2+p3        # 组合图形


#####————————————————————————————————#####
##### 【图5-2】的绘制代码——为直方图添加信息 
#####————————————————————————————————#####
# 图5-2的绘制代码
library(ggplot2)
library(patchwork)
library(e1071)   # 为使用其函数计算偏度系数和峰度系数
df<-read.csv("C:/mydata/chap05/data5_1.csv")

# 图(a) 添加地毯图、偏度系数和峰度系数
h1<-ggplot(data=df,aes(x=AQI))+    
  geom_histogram(fill="lightgreen",color="gray50")# 绘制直方图

p1<-h1+geom_rug(linewidth=0.2,color="blue3")+ # 添加地毯图,须线的宽度为0.2
  annotate("text",x=310,y=43,                 # 添加偏度系数
      label=paste("偏度系数 = ",round(skewness(df$AQI),4)),size=3)+      
  annotate("text",x=310,y=38,                 # 添加峰度系数
      label=paste("峰度系数 = ",round(kurtosis(df$AQI),4)),size=3)+  
  ylab("count")+
  ggtitle("(a) 添加地毯图、偏度和峰度系数")

# 图（b）添加频数多边形中位数点
p2<-h1+geom_freqpoly(color="red3")+  # 添加频数多边形
  geom_point(x=median(df$AQI),y=0,shape=21,size=4,fill="yellow")+# 添加中位数点
  annotate("text",x=median(df$AQI),y=4.2,label="中位数",size=3,color="red3")+ # 添加注释文本
  ylab("count")+
  ggtitle("(b) 添加频数多边形和中位数点")

# 图(c) 添加核密度曲线
h2<-ggplot(data=df,aes(x=AQI))+     
geom_histogram(aes(y=after_stat(density)),fill="lightgreen",color="gray50")
p3<-h2+geom_density(color="blue2",linewidth=0.7)+ # 添加核密度曲线
  annotate("segment",x=235,xend=175,y=0.0035,yend=0.0035,color="blue",linewidth=0.6,
  arrow=arrow(angle=15,length=unit(0.1,"inches")))+  # 添加带箭头的线
  annotate("text",x=290,y=0.0035,label="核密度曲线",size=3)+ # 添加注释文本
  geom_vline(xintercept=quantile(df$AQI),linetype="twodash",
             color="grey50",linewidth=0.5)+  # 添加分位数线（垂直线）
  ggtitle("(c) 添加核密度曲线和分位数线")

# 图(d) 添加理论正态曲线和均值线
p4<-h2+stat_function(fun=dnorm,args=list(mean=mean(df$AQI),sd=sd(df$AQI)),
     linetype="twodash",color="red2",linewidth=0.8)+ # 添加理论正态分布曲线
  geom_vline(xintercept=mean(df$AQI),linetype="twodash",linewidth=0.6,color="red")+              # 添加x均值线，并设置线形、线宽和颜色
  annotate("text",x=165,y=0.008,label="均值线",size=3)+  # 添加注释文本
  annotate("text",x=280,y=0.004,label="理论正态分布曲线",size=3)+  # 添加注释文本
  ggtitle("(d) 添加理论正态曲线和均值线")

(p1+p2)/(p3+p4)        # 组合图形


#####————————————————————————————————#####
##### 【图5-3】的绘制代码——AQI和PM2.5的叠加直方图和镜像直方图
#####————————————————————————————————#####
# 图5-3的绘制代码
library(ggplot2)
library(reshape2)
library(dplyr)
library(patchwork)
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")

# 图（a）垂直镜像直方图
p1<-ggplot(data5_1)+aes(x=x)+
  geom_histogram(aes(x=AQI,y=after_stat(density)),
     color="grey50",fill="red",alpha=0.3)+ # 绘制AQI的直方图（上图）
  annotate("text",x=160,y=0.0065,label="AQI",color="red")+ # 添加标签
  geom_histogram(aes(x=PM2.5,y=-after_stat(density)),color="grey50",
     fill="blue",alpha=0.3)+ # 绘制PM2.5的直方图（下图）
  annotate("text",x=160,y=-0.0065,label="PM2.5",color="blue")+ # 添加标签
  xlab("指标值")+
  ggtitle("(a) 垂直镜像直方图")

# 图（b）水平镜像直方图
p2<-p1+coord_flip()+
  ggtitle("(b) 水平镜像直方图")

p1+p2        # 组合图形


#####————————————————————————————————#####
##### 【图5-4】的绘制代码——按质量等级分组的AQI的直方图
#####————————————————————————————————#####
# 图5-4的绘制代码
library(ggplot2)
library(reshape2)
library(patchwork)

# 处理数据
df<-read.csv("C:/mydata/chap05/data5_1.csv")
df$质量等级<-factor(df$质量等级,ordered=TRUE,
   levels=c("优","良","轻度污染","中度污染","重度污染","严重污染"))# 设置类别顺序

# 绘制图形p1和p2
cols=c("green","yellow","orange","red","purple","maroon")# 设置质量等级的标准颜色向量
p<-ggplot(df)+aes(x=AQI,fill=质量等级)+
   scale_fill_manual(values=cols)         # 自定义颜色
   
p1<-p+geom_histogram(position="identity",binwidth=20,color="gray",alpha=0.7)+ 
   ggtitle("(a) 叠加分组直方图")

p2<-p+geom_histogram(binwidth=20,color="gray",alpha=0.7)+
   ggtitle("(b) 堆叠分组直方图")

p1+p2+plot_layout(guides="collect")&      # 组合图形并共享图例
   theme(legend.position="bottom")&       # 设置图例位置（底部）
   guides(fill=guide_legend(nrow=1))      # 图例排成1行


#####————————————————————————————————#####
##### 【图5-5】的绘制代码——按指标分面
#####————————————————————————————————#####
# 图5-5的绘制代码
library(ggplot2)
library(reshape2)
library(dplyr)
library(e1071)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(-c(日期,AQI,质量等级))%>%  # 删除不需要的变量
  melt(variable.name="指标",value.name="指标值")  # 融合数据

# 计算偏度系数
labels<-df%>%group_by(指标)%>%
   summarise(skewness=skewness(指标值))
                          # 按指标分组计算偏度系数并创建一个新数据框
labels$skewness<-sprintf("skewness == %.3f",labels$skewness)
                          # 返回包含文本和变量值的格式化组合的字符向量

# 绘制分面直方图
ggplot(df)+aes(x=指标值,fill=指标)+
  geom_histogram(color="gray50")+
  geom_text(x=0,y=31,aes(label=skewness),data=labels,parse=T,hjust=-0.85,
      size=3,color="grey35")+              # 为每个分面图添加偏度系数
  scale_fill_brewer(palette="Set3")+       # 设置调色板
  guides(fill="none")+                     # 移除fill产生的图例
  facet_wrap(~指标,ncol=3,scale="free")    # 按指标分面，自由设置坐标轴



#####  5.1.2  #####
#####————————————————————————————————#####
##### 【图5-6】的绘制代码——带宽对核密度图曲线的影响
#####————————————————————————————————#####
# 图5-6的绘制代码
library(ggplot2)
library(patchwork)
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")

# 设置图形主题
mytheme<-theme(plot.title=element_text(size="12"), # 设置主标题字体大小
   axis.title=element_text(size=10),               # 设置坐标轴标签字体大小
   axis.text=element_text(size=9),                 # 设置坐标轴刻度字体大小
   legend.text=element_text(size="8"))             # 设置图例字体大小

# 绘制AQI的核密度图
p<-ggplot(data5_1,aes(x=AQI))
p1<-p+geom_density(bw=5,color="blue3",fill="blue",alpha=0.1)+ # 带宽为5
  ggtitle("(a) bw=5")
p2<-p+geom_density(bw=10,color="blue3",fill="blue",alpha=0.1)+# 带宽为10
  ggtitle("(b) bw=10")
p3<-p+geom_density(bw=20,color="blue3",fill="blue",alpha=0.1)+
  ggtitle("(c) bw=20")                                        # 带宽为20
p1+p2+p3            # 组合图形


#####————————————————————————————————#####
##### 【图5-7】的绘制代码——核密度比较图
#####————————————————————————————————#####
# 图5-7的绘制代码
library(ggplot2)
library(reshape2)
library(dplyr)
library(patchwork)
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")

# 处理数据
df<-data5_1%>%
  select(-c(日期,二氧化硫,二氧化氮,一氧化碳))%>%  # 删除不需要的变量
  melt(id.vars="质量等级",variable.name="指标",value.name="指标值")
 
# 绘制图形p1和p2
p1<-ggplot(df)+aes(x=指标值)+
  geom_density(aes(group=指标,color=指标),alpha=0)+
  theme(legend.position="inside",
        legend.justification=c("right","top"),    # 设置图例位置
        legend.key.size=unit(0.4,"cm"),           # 设置图例大小
        legend.key.height=unit(0.5,"cm"),         # 设置图例键高度
        legend.background=element_blank())+        # 移除图例整体边框
  ggtitle("(a) 核密度比较曲线(alpha=0)")            # 添加标题

p2<-p1+geom_density(aes(group=指标,color=指标,fill=指标),alpha=0.3)+
  ggtitle("(b) 核密度比较曲线(alpha=0.3)")

p1+p2+plot_layout(axes="collect_y")   # 组合图形并删除重复的y轴


#####————————————————————————————————#####
##### 【图5-8】的绘制代码——镜像核密度比较图
#####————————————————————————————————#####
# 图5-8的绘制代码（代码不放在书中）
library(ggplot2)
library(reshape2)
library(dplyr)
library(patchwork)

data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(AQI,PM2.5,PM10,臭氧浓度)  # 选择绘图变量

# 图（a）AQI和PM2.5的镜像核密度图
p1<-ggplot(df)+aes(x=x)+
  geom_density(aes(x=AQI,y=after_stat(density)),fill="red",alpha=0.3)+ # 绘制AQI的核密度图（上图）
  annotate("text",x=160,y=0.0065,label="AQI",color="red")+ # 添加标签
  geom_density(aes(x=PM2.5,y=-after_stat(density)),fill="blue",alpha=0.3)+ 
                                            # 绘制PM2.5的核密度图（下图）
  annotate("text",x=160,y=-0.0065,label="PM2.5",color="blue")+ # 添加标签
  labs(x="指标值",title="(a) AQI和PM2.5的镜像核密度图")
 
# 图（b）二氧化氮和臭氧浓度的镜像核密度图
p2<-ggplot(df)+aes(x=x)+
  geom_density(aes(x=PM10,y=after_stat(density)),color="grey50",
                   fill="red",alpha=0.3)+ # 绘制PM10的直方图（上图）
  annotate("text",x=180,y=0.005,label="PM10",color="red")+ # 添加标签
  geom_density(aes(x=臭氧浓度,y=-after_stat(density)),color="grey50",
                   fill="blue",alpha=0.3)+ # 绘制臭氧浓度的直方图（下图）
  annotate("text",x=180,y=-0.005,label="臭氧浓度",color="blue")+# 添加标签
  labs(x="指标值",title="(b) PM10和臭氧浓度的镜像核密度图")

p1+p2        # 组合图形


#####————————————————————————————————#####
##### 【图5-9】的绘制代码——按质量等级分组的核密度图
#####————————————————————————————————#####
# 图5-9的绘制代码
library(ggplot2)
library(patchwork)

# 处理数据
df<-read.csv("C:/mydata/chap05/data5_1.csv")
df$质量等级<-factor(df[,3],ordered=TRUE,
   c("优","良","轻度污染","中度污染","重度污染","严重污染"))# 设置类别顺序

# 设置图形主题
mytheme<-theme(plot.title=element_text(size="12"), # 设置主标题字体大小
   axis.title=element_text(size=10),               # 设置坐标轴标签字体大小
   axis.text=element_text(size=9),                 # 设置坐标轴刻度字体大小
   legend.text=element_text(size="9"))             # 设置图例字体大小

# 绘制图形p1和p2
p1<-ggplot(df)+aes(x=AQI,fill=质量等级)+
  geom_density(color="gray50",alpha=0.5)+
  scale_fill_manual(values=c("green","yellow","orange",
                             "red","purple","maroon"))+
  ggtitle("(a) AQI")

p2<-p1+aes(x=PM2.5,fill=质量等级)+ggtitle("(b) PM2.5")

# 组合图形
p1+p2+plot_layout(guides="collect")&    # 组合图形并共享图例
  theme(legend.position="bottom")&      # 设置图例位置
  guides(fill=guide_legend(nrow=1))     # 图例排成1行


#####————————————————————————————————#####
##### 【图5-10】的绘制代码——ggplot2——按质量等级分组、按指标分面
#####————————————————————————————————#####
# 图5-10的绘制代码
library(ggplot2)
library(reshape2)
library(dplyr)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(-c(日期,AQI))%>%              # 删除不需要的变量
  melt(id.vars="质量等级",variable.name="指标",value.name="指标值")%>%
  mutate(质量等级=factor(质量等级,ordered=TRUE,
    levels=c("优","良","轻度污染","中度污染","重度污染","严重污染"))) # 修改类别顺序

# 绘制按质量等级分组、按指标分面的核密度图
ggplot(df)+aes(x=指标值,group=质量等级,fill=质量等级)+
  geom_density(color="gray60",alpha=0.3)+
  scale_fill_manual(values=c("green","yellow","orange","red",
                                  "purple","maroon"))+  # 自定义颜色
  facet_wrap(~指标,ncol=3,scale="free")+                # 按指标3列分面
  theme(legend.position="bottom")+                      # 设置图例位置
  guides(fill=guide_legend(nrow=1))                     # 图例排成1行


#####————————————————————————————————#####
##### 【图5-11】的绘制代码——按质量等级分组核密度图与平行坐标图
#####————————————————————————————————#####
# 图5-11的绘制代码
library(ggplot2)
library(ggmulti)
df<-read.csv("C:/mydata/chap05/data5_1.csv")
df$质量等级<-factor(df$质量等级,ordered=TRUE,
   levels=c("优","良","轻度污染","中度污染","重度污染","严重污染"))# 设置类别顺序

p<-ggplot(df,aes(AQI=AQI,PM2.5=PM2.5,PM10=PM10,臭氧浓度=臭氧浓度))+# 选择绘图变量
  geom_path(alpha=0.1)+        # 按照观察值在数据中出现的顺序连接
  coord_serialaxes()           # 设置平行坐标

p+geom_density(aes(fill=质量等级),alpha=0.4)+ # 按质量等级分组绘制核密度图
  scale_fill_manual(values=c("green","yellow","orange",
                             "red","purple","maroon"))+
  theme(legend.position="bottom",
        legend.key.size=unit(0.5,"cm"))+  # 设置图例键大小
  guides(fill=guide_legend(nrow=1))



#####  5.1.2  #####
#####————————————————————————————————#####
##### 【图5-12】的绘制代码——7项指标的脊线图
#####————————————————————————————————#####
# 图5-12的绘制代码
library(ggplot2)
library(ggridges)
library(RColorBrewer)
library(reshape2)
library(plyr)
library(dplyr)
library(patchwork)
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")

# 处理数据
df<-data5_1%>%select(-质量等级)%>%
  melt(variable.name="指标",value.name="指标值")%>%
  ddply("指标",transform,标准化值=scale(指标值)) # 计算标准化值并返回数据框

# 图（a）原始数据脊线图
palette<-rev(brewer.pal(11,"Spectral"))          # 设置调色板
p1<-ggplot(df,aes(x=指标值,y=指标,fill=after_stat(density)))+
  geom_density_ridges_gradient(scale=2.5,rel_min_height=0.01)+
  scale_x_continuous(expand=c(0.01,0.01))+    # 设置x轴扩展范围
  scale_y_discrete(expand=c(0.02,0.01))+      # 设置y轴扩展范围
  scale_fill_gradientn(colors=palette)+       # 自定义调色板
  theme(legend.position="inside",legend.justification=c("right","bottom"),
        legend.key.height=unit(0.3,"cm"),        # 设置图例位置和键高度        
        legend.background=element_blank(),       # 设置图例背景
        axis.text.y=element_text(size=7,angle=90,hjust=0.5))+# 设置y轴刻度标签大小
  ggtitle("(a) 原始数据脊线图")

# 图（b）标准化脊线图
p2<-p1+aes(x=标准化值,y=指标,fill=after_stat(density))+
  ggtitle("(b) 标准化脊线图")

p1+p2        # 组合图形


#####————————————————————————————————#####
##### 【图5-13】的绘制代码——脊线图(ridgeline plot)
#####————————————————————————————————#####
# 图5-13的绘制代码
library(ggplot2)
library(ggridges)
library(RColorBrewer)
library(patchwork)

# 处理数据
df<-read.csv("C:/mydata/chap05/data5_1.csv")
df$质量等级<-factor(data5_1[,3],ordered=TRUE,
   levels=c("优","良","轻度污染","中度污染","重度污染","严重污染"))# 设置类别顺序

# 绘制图p1和p2
palette<-rev(brewer.pal(11,"Spectral"))            # 设置调色板
p1<-ggplot(df,aes(x=AQI,y=质量等级,fill=after_stat(density))) + 
  geom_density_ridges_gradient(scale=2.5,rel_min_height=0.01)+
  scale_x_continuous(expand=c(0.018,0.01))+  # 设置x轴扩展范围
  scale_y_discrete(expand=c(0.01,0.1))+     # 设置y轴扩展范围
  scale_fill_gradientn(colours = palette)+
  theme(legend.position="inside",legend.justification=c("right","bottom"),
        legend.key.height=unit(0.3,"cm"),            # 设置图例键高度        
        legend.background=element_blank(),
        axis.text.y=element_text(angle=90,hjust=0))+ # 调整y轴标签角度
  ggtitle("(a) AQI的脊线图")

p2<-p1+aes(x=PM10,y=质量等级,fill=after_stat(density))+ 
  ggtitle("(b) PM10的脊线图")

p1+p2        # 组合图形


#####————————————————————————————————#####
##### 【图5-14】的绘制代码——各月的脊线图
#####————————————————————————————————#####
# 图5-14的绘制代码
library(ggplot2)
library(ggridges)
library(reshape2)
library(dplyr)
library(RColorBrewer)
library(lubridate)                 # 为使用函数month提取月份

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(日期,AQI,PM2.5,PM10)%>%   # 选择绘图变量
   mutate(日期=as.Date(日期),                  # 将日期转化成日期变量
          月份=factor(month(日期,              # 在数据框中添加月份因子
               label=TRUE,      # 将月份显示为字符串，FALSE则显示为数字
               abbr=TRUE)))%>%  # 显示标签的缩写版本（数字月份，如1月）       
   melt(id.vars=c("日期","月份"),variable.name="指标",value.name="指标值")

# 绘制脊线图
palette<-rev(brewer.pal(11,"Spectral"))
ggplot(df,aes(x=指标值,y=月份,fill=after_stat(density)),color=指标)+
  geom_density_ridges_gradient(scale=1.8,rel_min_height=0.01)+
  scale_x_continuous(expand=c(0.01,0))+
  scale_y_discrete(expand=c(0.02,0.02))+
  scale_fill_gradientn(colors=palette)+
  theme(legend.position="bottom",legend.key.width=unit(1.2,"cm"),
        legend.key.height=unit(0.4,"cm"))+
facet_wrap(指标~.,ncol=3,scale="free")  # 按3列分面，自由设置坐标轴



#####  5.1.2  箱线图和小提琴图  #####

#####————————————————————————————————#####
##### 【图5-17】的绘制代码——箱线图+mean
#####————————————————————————————————#####
# 图5-17的绘制代码
library(ggplot2);library(reshape2);library(patchwork)

data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-melt(data5_1[,-c(1,2)],variable.name="指标",value.name="指标值")

# 图（a）垂直摆放（默认）
p1<-ggplot(df,aes(x=指标,y=指标值,fill=指标))+
  geom_boxplot()+                            # 绘制箱线图
  scale_fill_brewer(palette="Set2")+         # 设置填充颜色
  stat_summary(fun="mean",geom="point",shape=21,size=2.5,fill="white")+
                                             # 添加均值点
  guides(fill="none")+ggtitle("(a) 垂直摆放(默认)")

# 图（b）水平摆放（坐标轴互换）
p2<-p1+coord_flip()+ggtitle("(b) 水平摆放(坐标轴互换)")

p1+p2        # 组合图形


#####————————————————————————————————#####
##### 【图5-18】的绘制代码——对数变换和标准化图变换—以观察分布形状
#####————————————————————————————————#####
# 图5-18的绘制代码
library(ggplot2);library(reshape2);library(plyr)
library(dplyr);library(patchwork)
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")

# 数据处理
df<-data5_1%>%select(-c(日期,AQI))%>%                # 删除不需要的变量
  melt(variable.name="指标",value.name="指标值")%>%  # 融合数据
  ddply("指标",transform,标准化值=scale(指标值))     # 计算标准化值

# 绘制箱线图
p1<-ggplot(df,aes(x=指标,y=log10(指标值),fill=指标))+  # y值取对数
  geom_boxplot(outlier.size=0.8)+                 # 设置离群点大小
  scale_fill_brewer(palette="Set2")+              # 设置填充颜色
  scale_x_discrete(guide=guide_axis(n.dodge=2))+  # x轴标签为2行
  guides(fill="none")+labs(y="对数值",title="(a) 对数变换")

p2<-p1+aes(x=指标,y=标准化值)+
  ggtitle("(b) 标准化变换")

p1+p2        # 组合图形

## 注：y=log10(指标值)等价于scale_y_log10 函数


#####————————————————————————————————#####
##### 【图5-19】的绘制代码——按因子分组的不同样本量的箱线图——箱子宽度与样本量的平方根成比例
#####————————————————————————————————#####
# 图5-19绘制代码
library(ggplot2);library(reshape2);library(patchwork)
library(EnvStats)   # 为使用stat_n_text函数添加样本量

# 数据处理
df<-read.csv("C:/mydata/chap05/data5_1.csv")
df$质量等级<-factor(df$质量等级,ordered=TRUE,
  levels=c("优","良","轻度污染","中度污染","重度污染","严重污染")) # 设置类别顺序

# 绘制箱线图
cols=c("green","yellow","orange","red","purple","maroon")# 设置质量等级的标准颜色向量
p1<-ggplot(df,aes(x=质量等级,y=PM10,fill=质量等级))+
  geom_boxplot(varwidth=TRUE,fill=cols)+
  stat_n_text(size=3,color="brown")+      # 添加分组样本量 
  ggtitle("(a) PM10的不等宽箱线图")

p2<-ggplot(df,aes(x=质量等级,y=臭氧浓度,fill=质量等级))+
  geom_boxplot(varwidth=TRUE,fill=cols)+
  stat_n_text(size=3,color="brown",y.pos=300)+ # 设置样本量标签在y轴的位置
  ggtitle("(b) 臭氧浓度的不等宽箱线图")

p1+p2        # 组合图形


#####————————————————————————————————#####
##### 【图5-20】的绘制代码——各月份臭氧浓度的箱线图
#####————————————————————————————————#####
# 5-20的绘制代码
library(ggplot2)
library(dplyr)
library(ggsci)
library(patchwork)
library(lubridate)                 # 为使用函数month提取月份

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%mutate(日期=as.Date(日期),    # 将日期转化成日期变量
    月份=factor(month(日期,label=TRUE,abbr=TRUE))) # 添加月份因子
# 按中位数排序
order_df<-with(df,reorder(月份,臭氧浓度,median))  # 根据臭氧浓度的中位数对数据框排序
f<-factor(order_df,levels=rev(levels(order_df))) # 按中位数降序重新序排序月份因子

# 图（a）按月份排序（默认）
p1<-ggplot(df,aes(x=月份,y=臭氧浓度,fill=月份))+
  geom_boxplot(notch=TRUE,notchwidth=0.5)+  # 绘制凹槽箱线图
  scale_fill_rickandmorty()+                # 使用ggsci包的调色板
  guides(fill="none")+
  labs(x="月份",title="(a) 按月份排序 (默认)")

# 图（b）按中位数排序
p2<-ggplot(df,aes(x=f,y=臭氧浓度,fill=月份))+
  geom_boxplot(notch=TRUE,notchwidth=0.5)+
  scale_fill_rickandmorty()+
  guides(fill="none")+
  labs(x="月份",title="(b) 按中位数排序")

p1+p2+plot_layout(axes="collect_y") # 组合图形并删除重复的y轴


#####————————————————————————————————#####
##### 【图5-21】的绘制代码——标准化变换—以观察分布形状
#####————————————————————————————————#####
# 图5-21的绘制代码
library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(patchwork)

# 数据处理
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(质量等级,二氧化氮,臭氧浓度)%>%
  melt(variable.name="指标",value.name="指标值")%>%
  ddply("指标",transform,标准化值=scale(指标值))%>% # 计算标准化值并添加到数据框
  mutate(质量等级=factor(质量等级,ordered=TRUE,
  levels=c("优","良","轻度污染","中度污染","重度污染","严重污染"))) # 设置类别顺序

# 绘制箱线图
cols=c("green","yellow","orange","red","purple","maroon")# 设置质量等级的标准颜色向量
p1<-ggplot(df)+aes(x=指标,y=指标值,fill=质量等级)+
  geom_boxplot(outlier.size=1)+
  scale_fill_manual(values=cols)+
  ggtitle("(a) 原始数据")

p2<-p1+aes(x=指标,y=标准化值,fill=质量等级)+ggtitle("(b) 标准化变换")

# 组合图形
p1+p2+plot_layout(guides="collect")&  # 组合图形并共享图例
  theme(legend.position="bottom")&      # 设置图例位置
  guides(fill=guide_legend(nrow=1))     # 图例排成1行


#####————————————————————————————————#####
##### 【图5-22】的绘制代码——小提琴图
#####————————————————————————————————#####
# 图5-22的绘制代码
library(ggplot2);library(reshape2);library(plyr)
library(dplyr)
library(patchwork)

# 数据处理
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(-c(日期,AQI))%>%               # 删除不需要的变量
  melt(variable.name="指标",value.name="指标值")%>% # 融合数据
  ddply("指标",transform,标准化值=scale(指标值))    # 计算标准化值

# 设置图形主题
mytheme<-theme(plot.title=element_text(size="11"), # 设置主标题字体大小
   axis.title=element_text(size=10),               # 设置坐标轴标签字体大小
   axis.text=element_text(size=9),                # 设置坐标轴刻度字体大小
   legend.text=element_text(size="8"))            # 设置图例字体大小

# 图（a）原始数据小提琴图
p1<-ggplot(df,aes(x=指标,y=指标值,fill=指标))+
   geom_violin(scale="width",trim=FALSE)+
   geom_boxplot(outlier.size=0.7,outlier.color="white",size=0.3,
               width=0.2,fill="white")+  # 添加并设置箱线图和离群点参数
   scale_fill_brewer(palette="Set2")+
   stat_summary(fun=mean,geom="point",shape=21,size=2)+# 添加均值点
   guides(fill="none")+
   ggtitle("(a) 原始数据小提琴图")

# 图（b）数据标准化后的小提琴图
p2<-ggplot(df,aes(x=指标,y=标准化值,fill=指标))+
   geom_violin(scale="width")+
   geom_boxplot(,outlier.size=0.7,outlier.color="black",size=0.3,
          width=0.2,fill="white")+
   scale_fill_brewer(palette="Set2")+
   guides(fill="none")+
   ggtitle("(b) 标准化小提琴图")

p1+p2        # 组合图形


#####————————————————————————————————#####
##### 【图5-23】的绘制代码——按质量等级分组、按指标分面
#####————————————————————————————————#####
# 图5-23的绘制代码
library(ggplot2);library(reshape2);library(dplyr)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(-c(日期,AQI))%>%
  melt(id.vars="质量等级",variable.name="指标",value.name="指标值")%>%
  mutate(质量等级=factor(质量等级,ordered=TRUE,
    levels=c("优","良","轻度污染","中度污染","重度污染","严重污染"))) # 设置类别

# 绘制按质量等级分组、按指标分面的小提琴图
ggplot(df,aes(x=指标,y=指标值,fill=质量等级))+
     geom_violin(scale="width",
     draw_quantiles=c(0.25,0.5,0.75),color="grey30",linewidth=0.5)+ 
                                                # 绘制分位数线水平线
scale_fill_manual(values=c("green","yellow","orange",
                            "red","purple","maroon"))+
   theme(legend.position="bottom")+
   guides(x="none",fill=guide_legend(nrow=1))+# 删除x轴，图例1行摆放
   facet_wrap(~指标,ncol=3,scale="free")      # 按指标3列分面


#####————————————————————————————————#####
##### 【图5-24】的绘制代码——半小提琴图
#####————————————————————————————————#####
# 图5-24的绘制代码
library(ggplot2)
library(gghalves)
library(patchwork)

# 数据处理
df<-read.csv("C:/mydata/chap05/data5_1.csv")
df$质量等级<-factor(df$质量等级,ordered=TRUE,
  levels=c("优","良","轻度污染","中度污染","重度污染","严重污染"))  # 设置类别顺序
cols=c("green","yellow","orange","red","purple","maroon")# 设置质量等级的标准颜色向量

# 图（a）半小提琴图+箱线图+扰动点
p<-ggplot(df,aes(x=质量等级,y=PM10,fill=质量等级))+
  geom_half_violin(scale="width",width=1,color="grey40",  # 绘制半小提琴图并设置宽度和边线颜色
  position=position_nudge(x=0.2), # 设置小提琴图的位置（为点或箱线图预留空间）
  side="r",alpha=0.7)+  # 绘制半小提琴图的右侧（默认side="l"，即左侧）
  scale_fill_manual(values=cols)+ # 自定义颜色
  coord_flip()+                   # 坐标轴互换
  guides(fill="none")+            # 删除图例
  theme(axis.text.y=element_text(angle=90,hjust=0.5)) # 调整y轴标签角度

p1<-p+geom_boxplot(width=0.3,linewidth=0.5)+ # 绘制箱线图并设置宽度和线宽
  geom_jitter(aes(fill=质量等级),shape=21,size=1.5,width=0.15)+# 设置扰动点的填充颜色、形状、大小和宽度
  ggtitle("(a) 半小提琴图+箱线图+扰动点")    

# 图（b）半小提琴图+半箱线图+点
p2<-p+geom_half_boxplot(width=0.3,linewidth=0.5)+ # 绘制半箱线图
  geom_point(aes(fill=质量等级),shape=21,size=1.5,alpha=0.5)+  # 绘制点
  ggtitle("(b) 半小提琴图+半箱线图+点")

p1+p2+plot_layout(axes="collect_y") # 组合图形并删除重复的y轴


#####————————————————————————————————#####
##### 【图5-25】的绘制代码——半小提琴图+箱线图+扰动点
#####————————————————————————————————#####
# 图5-25的绘制代码
library(ggplot2)
library(gghalves)
library(patchwork)

# 数据处理
df<-read.csv("C:/mydata/chap05/data5_1.csv")
df$质量等级<-factor(df$质量等级,ordered=TRUE,
  levels=c("优","良","轻度污染","中度污染","重度污染","严重污染"))  # 设置类别顺序
cols=c("green","yellow","orange","red","purple","maroon")# 设置质量等级的标准颜色向量

# 图（a）AQI的半小提琴图+箱线图+扰动点
p1<-ggplot(df,aes(x=1,y=AQI,fill=质量等级))+
  geom_half_violin(scale="area",width=1,color="grey30", # 设置半小提琴图宽度为"area"
  position=position_nudge(x=0.15),side="r",alpha=0.5)+
  geom_boxplot(width=0.06,linewidth=0.5,
               alpha=0.5,position=position_nudge(x=0.1))+
  geom_jitter(aes(x=0.88,fill=质量等级),shape=21,size=1.5,
             width=0.15,alpha=0.5)+
  scale_fill_manual(values=cols)+
  guides(fill=guide_legend(nrow=1))+  # 设置图例摆放成1行
  coord_flip()+
  ggtitle("(a) AQI的半小提琴图+箱线图+扰动点")    

# 图（b）PM2.5的半小提琴图+箱线图+扰动点
p2<-p1+aes(x=1,y=PM2.5,fill=质量等级)+
    ggtitle("(b) PM2.5的半小提琴图+箱线图+扰动点") 

# 组合图形
p1+p2+plot_layout(guides="collect")&  # 组合图形并共享图例
theme(legend.position="bottom")&      # 设置图例位置
guides(fill=guide_legend(nrow=1))     # 图例排成1行


#####————————————————————————————————#####
##### 【图5-26】的绘制代码——对置小提琴图（Split Violin Plot）
#####————————————————————————————————#####
# 图5-26的绘制代码
# 安装包：devtools::install_github("psyteachr/introdataviz")
library(ggplot2)
library( introdataviz)
library(reshape2)
library(dplyr)
library(patchwork)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(质量等级,AQI,PM2.5)%>%              # 选择绘图变量
  melt(variable.name="指标",value.name="指标值")%>%      # 融合数据
  mutate(质量等级=factor(质量等级,ordered=TRUE,
     levels=c("优","良","轻度污染","中度污染","重度污染","严重污染")))  # 设置类别顺序

# 图（a）水平对置
p1<-ggplot(df)+aes(x=质量等级,y=指标值,fill=指标)+
  geom_split_violin(scale="width",trim=FALSE)+
  theme(legend.position="inside",               
      legend.justification=c("left","top"),# 设置图例位置
      axis.text.y=element_text(angle=90,hjust=0.5), # 设置y轴标签角度
      legend.background=element_blank(),            # 移除图例整体边框
      legend.direction="vertical",      # 设置图例位置和摆放方向（垂直）
      legend.key.height=unit(0.5,"cm"))+            # 设置图例键高度
  ggtitle("(a) 水平对置")

# 图（b）垂直对置
p2<-p1+coord_flip()+
  ggtitle("(b) 垂直对置")
  
p1+p2        # 组合图形



#####================================================================#####
#####  5.2  数据分布位置和范围
#####================================================================#####

#####————————————————————————————————#####
##### 【图5-27】的绘制代码——带状图
#####————————————————————————————————#####
# 图5-27的绘制代码
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggstats)
library(patchwork)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(日期,AQI)%>%
    mutate(日期=as.Date(日期),    # 将日期转化成日期变量
    月份=factor(month(日期,label=TRUE,abbr=TRUE)))%>% # 添加月份因子
    filter(月份<="6月")           # 选出前6个月的数据

# 图（a）原始数据带状图
p1<-ggplot(df,aes(x=AQI,y=月份,fill=月份))+
    geom_point(aes(fill=月份),shape=21,size=1.5)+  # 绘制点
    scale_fill_brewer(palette="Set2")+
    ggstats::geom_stripped_rows()+         # 添加行交替背景色       
    theme_test()+ggtitle("(a) 原始数据带状图")

# 图（b）数据扰动后的带状图
p2<-p1+geom_jitter(shape=21,size=1.5,width=0.2)+ # 绘制扰动点，设置点的对象和扰动宽度
    ggtitle("(b) 数据扰动后的带状图")

# 图（c）箱线图+原始数据带状图
p3<-p1+geom_boxplot()+                     # 绘制箱线图
    geom_point(aes(fill=月份),shape=21,size=1.5)+
    ggtitle("(c) 箱线图+原始数据带状图")

# 图（d）小提琴图+数据扰动后的带状图
p4<-p1+geom_violin(scale="width")+      # 绘制小提琴图
    geom_jitter(aes(fill=月份),shape=21,size=1.5,width=0.2)+
    ggtitle("(d) 小提琴图+数据扰动后的带状图")

# 组合图形
p1+p2+p3+p4+plot_layout(nrow=2,axes="collect")& # 组合图形并删除重复的y轴和x轴
   guides(fill="none",color="none")             # 删除图例


#####————————————————————————————————#####
##### 【图5-28】的绘制代码——威尔金森点图
#####————————————————————————————————#####
# 图5-28的绘制代码（以AQI、PM2.5、PM10、二氧化氮和臭氧浓度为例）
library(ggplot2);library(reshape2);library(dplyr);library(patchwork)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(AQI,PM2.5,PM10,二氧化氮,臭氧浓度)%>%
  melt(variable.name="指标",value.name="指标值")   # 选择变量并转化成长格式

# 绘制图形
p<-ggplot(df,aes(x=指标,y=指标值,fill=指标))+theme(legend.position="none")

p1<-p+geom_dotplot(dotsize=2.5,binaxis="y",binwidth=3,stackdir="center")+ # 绘制点图
  ggtitle("(a) 居中堆叠")

p2<-p+geom_dotplot(dotsize=2.5,binaxis="y",binwidth=3)+ # 绘制点图
  ggtitle("(b) 向上堆叠")

p3<-p+geom_violin(width=1.5)+     # 绘制小提琴图
  geom_dotplot(dotsize=2,binaxis="y",binwidth=3.5,stackdir="center")+
  ggtitle("(c) 小提琴图+居中堆叠")

p4<-p+geom_boxplot(aes(x=as.numeric(指标)+0.08,group=指标),width=0.25,notch=TRUE)+
  geom_dotplot(dotsize=2.5,aes(x=as.numeric(指标)-0.08,group=指标),
               width=0.5,binaxis="y",binwidth=2.5,stackdir="down")+
  scale_x_continuous(breaks=1:nlevels(df$指标),labels=levels(df$指标))+
  ggtitle("(d) 凹槽箱线图+向下堆叠")

((p1+p2)+plot_layout(axes="collect_y"))/  # 组合图形并删除重复的y轴
   ((p3+p4)+plot_layout(axes="collect_y"))


#####————————————————————————————————#####
##### 【图5-29】的绘制代码——威尔金森点图
#####————————————————————————————————#####
# 图5-29的绘制代码（以AQI为例）
library(ggplot2)
library(dplyr) 
library(lubridate)
library(patchwork)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%mutate(日期=as.Date(日期),            # 将日期转化成时间变量
   月份=factor(month(日期,label=TRUE,abbr=TRUE)),   # 添加月份因子
   质量等级=factor(质量等级,ordered=TRUE,
      levels=c("优","良","轻度污染","中度污染","重度污染","严重污染")))  # 设置类别顺序

# 图（a）按质量等级分组
cols=c("green","yellow","orange","red","purple","maroon")
p1<-ggplot(df,aes(x=质量等级,y=AQI,fill=质量等级))+
  geom_dotplot(dotsize=2,binaxis="y",binwidth=4,stackdir="center")+ # 绘制点图
  scale_fill_manual(values=cols)+   # 自定义颜色
  theme(legend.position="none")+ggtitle("(a) 按质量等级分组")

# 图（b）按月份分组
p2<-ggplot(df,aes(x=月份,y=AQI,fill=质量等级))+ 
  geom_dotplot(dotsize=2,binaxis="y",binwidth=5)+ # 绘制点图
  scale_fill_manual(values=cols)+
  theme(legend.position="inside",legend.justification=c("right","top"),
        legend.background=element_blank())+
  guides(fill=guide_legend(nrow=2,title=NULL))+# 图例排成2行,去掉图例标题
  ggtitle("(b) 按月份分组")

p1+p2+plot_layout(axis_titles="collect_y") # 组合图形并删除重复的y轴标题


#####————————————————————————————————#####
##### 【图5-30】的绘制代码——蜂群图
#####————————————————————————————————#####
# 图5-30的绘制代码
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggbeeswarm)
library(lubridate)
library(patchwork)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df1<-data5_1%>%select(AQI,PM2.5,PM10,二氧化氮,臭氧浓度)%>%
  melt(variable.name="指标",value.name="指标值")     # 将数据转化成长格式

# 图（a）5项指标的蜂群图
p<-ggplot(df1,aes(x=指标,y=指标值))
p1<-p+geom_beeswarm(cex=0.8,shape=21,fill="black",size=0.7,aes(color=指标))+# 设置蜂群的宽度、点的形状、大小和填充颜色
 theme(legend.position="none")+ggtitle("(a) 蜂群图")

# 图（b）箱线图+蜂群图
p2<-p+geom_boxplot(size=0.5,outlier.size=0.8,aes(color=指标))+
 geom_beeswarm(shape=21,cex=0.8,size=0.8,aes(color=指标))+
 theme(legend.position="none")+ggtitle("(b) 箱线图+蜂群图")

# 图(c)各月份AQI的蜂群图
df3<-data5_1%>%mutate(日期=as.Date(日期),
    月份=factor(month(日期,label=TRUE,abbr=TRUE)),   # 添加月份因子
    质量等级=factor(质量等级,ordered=TRUE,
      levels=c("优","良","轻度污染","中度污染","重度污染","严重污染")))  # 设置类别顺序
p3<-ggplot(df3,aes(x=月份,y=AQI))+
 geom_beeswarm(cex=1.5,shape=21,size=1.5,color="black",aes(fill=质量等级))+
 scale_fill_manual(values=c("green","yellow","orange",
                            "red","purple","maroon"))+
 theme(legend.position="inside",legend.position.inside=c(0.85,0.5),   
       legend.background=element_blank())+
 coord_flip()+
 ggtitle("(c) 各月份AQI的蜂群图")

p1/p2|p3                      # 使用patchwork包组合图形


#####————————————————————————————————#####
##### 【图5-31】的绘制代码——云雨图
#####————————————————————————————————#####
# 图5-31的绘制代码（以PM10和臭氧浓度为例）
library(ggplot2)
library(gghalves)
library(patchwork)

# 数据处理
df<-read.csv("C:/mydata/chap05/data5_1.csv")
df$质量等级<-factor(df$质量等级,ordered=TRUE,
  levels=c("优","良","轻度污染","中度污染","重度污染","严重污染"))  # 设置类别顺序
cols=c("green","yellow","orange","red","purple","maroon")# 设置质量等级的标准颜色向量

# 图（a）垂直摆放
p1<-ggplot(df,aes(x=质量等级,y=PM10,fill=质量等级))+
  geom_half_violin(scale="width",width=1,  # 设置半小提琴图的宽度
     position=position_nudge(x=0.12),side="r",alpha=0.6)+ # 设置位置
  geom_boxplot(position=position_nudge(x=0.2),width=0.2,linewidth=0.5)+ # 设置箱线图的宽度和线宽 
  geom_dotplot(position=position_nudge(x=0.1),alpha=0.8, # 设置点图位置
      dotsize=2,width=0.5,binaxis="y",binwidth=3,stackdir="down")+
  scale_fill_manual(values=cols)+    # 自定义颜色
  theme(axis.text.y=element_text(angle=90,hjust=0.5))+
  guides(fill="none")+               # 删除图例
  ggtitle("(a) PM10 (垂直摆放)")   

# 图（b）水平摆放
p2<-p1+aes(x=质量等级,y=臭氧浓度,fill=质量等级)+
  coord_flip()+
  ggtitle("(b) 臭氧浓度 (水平摆放)") 

p1+p2 # 组合图形


#####————————————————————————————————#####
##### 【图5-32】的绘制代码——绘制极差图
#####————————————————————————————————#####
# 图5-32的绘制代码（数据：data5_1）
library(ggESDA)
library(ggplot2)
library(dplyr)
library(patchwork)
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")

# 处理数据
d<-data5_1[,-1]%>%mutate(质量等级=factor(质量等级,ordered=T,levels=c("优","良","轻度污染","中度污染","重度污染","严重污染")))
df<-classic2sym(d,groupby="质量等级")$intervalData  # 转换成符号数据（间隔）

# 图（a）线条极差图
p1<-ggInterval_index(df,plotAll=TRUE)+  # 绘制全部数据
   theme(axis.text.x=element_text(angle=90,hjust=0.5,vjust=0.5))+
   labs(x="指标值",y="质量等级",title="(a) 线条极差图 (用线条表示间隔)")
# 图（b）图像极差图
p2<-ggInterval_indexImage(df,plotAll=TRUE)+
    theme(legend.position="inside",legend.position.inside=c(0.095,0.18),
          legend.key.size=unit(0.2,"cm"),legend.key.height=unit(0.2,"cm"),
          legend.key.width=unit(0.3,"cm"),
          legend.text=element_text(size=6),
          legend.background=element_blank())+
    labs(x="质量等级",title="(b) 图像极差图 (用图像表示间隔)")

p1/p2      # 组合图形


#####————————————————————————————————#####
##### 【图5-33】的绘制代码——按月分组绘制五数概括图
#####————————————————————————————————#####
# 图5-33的绘制代码（数据：data5_1）
library(ggplot2)
library(dplyr)
library(reshape2)
library(lubridate)
library(patchwork)

# 处理数据（计算五数概括值）
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(-质量等级)%>%mutate(日期=as.Date(日期), # 将日期转化成日期变量
   月份=month(日期,label=TRUE,abbr=TRUE))%>%select(-日期)%>%
   group_by(月份)%>%summarise_all(quantile)%>% # 按月分组计算所有指标的五数概括值
   mutate(五数概括=c("min","q25%","median","q75%","max"))%>%
   melt(id.vars=c("月份","五数概括"),variable.name="指标",value.name="五数概括值")

# 绘制图形
ggplot(df)+aes(x=月份,y=五数概括值)+
  geom_line(aes(group=月份),linewidth=0.5,color="grey50")+        # 绘制线
  geom_line(aes(group=五数概括),linewidth=0.3,color="grey50")+
  geom_point(size=1.5,shape=21,aes(fill=五数概括))+  # 绘制点
  scale_fill_discrete(limits=c("min","q25%","median","q75%","max"))+ # 修改图例顺序
  theme(legend.position="inside",legend.position.inside=c(0.9,0.25))+
  coord_flip()+
  facet_wrap(指标~.,ncol=4,scale="free")  # 按指标4列分面，自由设置坐标轴



#####================================================================#####
#####  5.3  检验数据分布
#####================================================================#####

#####————————————————————————————————#####
##### 【图5-35】的绘制代码——正态分布的检验（Q-Q图）
#####————————————————————————————————#####
# 图5-35的绘制代码（数据：data5_1）
library(ggplot2)
library(qqplotr)
library(patchwork)

df<-read.csv("C:/mydata/chap05/data5_1.csv")

# 绘制Q-Q图
p<-ggplot(df,aes(sample=AQI))+
  stat_qq_point(distribution="norm")+ # 绘制Q-Q点（默认理论概率分布函数为正态）
  stat_qq_band(bandType="pointwise")+        # 绘制置信带（默认）
  stat_qq_line(linewidth=1,color="red")+     # 绘制Q-Q线
  labs(x="理论分位数",y="样本分位数",title="(a) AQI的Q-Q图")

# 绘制直方图
h<-ggplot(df,aes(x=AQI))+
  geom_histogram(bins=20,aes(y=after_stat(density)),
                 fill="lightskyblue",color="gray50")+
  geom_density(color="red2",linewidth=0.7)+ # 添加核密度曲线
  theme_light()+
  theme(axis.title=element_text(size=8),axis.text=element_text(size=6),
     panel.background=element_rect(fill="lightyellow"),# 设置图形面板背景色
     plot.background=element_rect(fill="lightblue"))   # 设置图形整体背景色

# 插入直方图
p1<-p+annotation_custom(grob=ggplotGrob(h),xmin=-100,
                        xmax=160,ymin=185,ymax=440)  

# 绘制P-P图
p_value<-shapiro.test(df$AQI)$p.value  # 提取 Shapiro-Wilk正态性检验的P值
p2<-ggplot(df,aes(sample=AQI))+
  stat_pp_point(distribution="norm")+# 绘制P-P点（默认理论概率分布函数为正态）
  stat_pp_band(bandType="boot")+             # 绘制置信带
  stat_pp_line(linewidth=1,color="red")+     # 绘制P-P线（x=y）
  annotate("text",x=0.3,y=0.95,size=5,color="red2",
           label=paste("p_value =",round(p_value,18)))+ 
  labs(x="理论概率",y="样本累积概率",title="(b) AQI的P-P图")

p1+p2       # 组合图形


#####————————————————————————————————#####
##### 【图5-36】的绘制代码——正态分布的检验（Q-Q图和P-P图）
#####————————————————————————————————#####
# 图5-36的绘制代码（数据：data5_1）
library(ggplot2)
library(qqplotr)
library(reshape2)
library(dplyr)
library(patchwork)

data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(-c(日期,AQI,质量等级))%>%
  melt(variable.name="指标",value.name="指标值")

# 图（a）Q-Q图
p1<-ggplot(df,aes(sample=指标值,color=指标))+
  stat_qq_point(distribution="norm",size=1)+ # 绘制Q-Q点（默认理论概率分布函数为正态）
  stat_qq_band(bandType="pointwise")+        # 绘制置信带
  stat_qq_line(color="red")+                 # 绘制Q-Q线
  guides(color="none")+
  labs(x="理论分位数",y="样本分位数",title="(a) Q-Q图")+
  facet_wrap(~指标,ncol=2,scale="free")       # 按指标2列分面

# 图（b）P-P图并添加Shapiro-Wilk正态性检验的P值
labels<-df%>%group_by(指标)%>%               # 按指标分组
  summarise(p_value=shapiro.test(指标值)$p.value) # 构建Shapiro-Wilk正态性检验P值的数据框
p2<-ggplot(df,aes(sample=指标值,color=指标))+
  stat_pp_point(distribution="norm",size=1)+   # 绘制P-P点
  stat_pp_band(bandType="boot")+ # 绘制置信带
  stat_pp_line(color="red")+     # 绘制P-P线
  guides(color="none")+theme_bw()+
  labs(x="理论累积概率",y="样本累积概率",title="(b) P-P图")+
  theme(strip.background=element_rect(fill="lightyellow",color="grey50"))+
                                        # 设置分面背景颜色和边框颜色
  geom_label(data=labels,aes(label=p_value),x=0.7,y=0.1,parse=TRUE,
       size=2.5,color="darkred",inherit.aes=FALSE)+   # 为每个分面图添加P值 
  facet_wrap(~指标,ncol=2,scale="free")        # 按指标2列分面

p1+theme(plot.margin=unit(c(0,20,0,0),"pt"))+p2 # 组合图形并设置p1的边距



#####================================================================#####
#####  5.4  展示推断信息
#####================================================================#####

#####————————————————————————————————#####
##### 【图5-37】的绘制代码——带有方差分析信息的箱线图和小提琴图
#####————————————————————————————————#####
# 图5-37的绘制代码
library(ggplot2)
library(ggpubr)
library(agricolae)
library(dplyr)
library(patchwork)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
levels=c("优","良","轻度污染","中度污染","重度污染","严重污染")   # 设置类别顺序
cols=c("green","yellow","orange","red","purple","maroon")   # 设置颜色向量
data5_1$质量等级<-factor(data5_1$质量等级,ordered=TRUE,levels=levels)

# 图（a）小提琴图 + 方差分析和多重比较P值
compared<-list(c("优","良"),c("良","中度污染"),
               c("优","严重污染"),c("中度污染","重度污染"),
               c("重度污染","严重污染"))  # 列出比较组（6个质量等级共有15种组合，可根据需要选择）
p1<-ggplot(data5_1,aes(x=质量等级,y=臭氧浓度,fill=质量等级))+
  geom_violin(scale="width",trim=FALSE)+ # 绘制小提琴图
  geom_boxplot(width=0.2,fill="white")+  # 添加箱线图
  scale_fill_manual(values=cols)+
  guides(fill="none")+
  ggtitle("(a) 小提琴图 + 方差分析和多重比较 P 值")+
  stat_compare_means(comparisons=compared,method="t.test")+ 
  stat_compare_means(method="anova",label.y=460) # 设置方差分析P值的位置

# 图（b）箱线图 + 方差分析P值和多重比较字母
# 按质量等级分组计算最大值并返回数据框
df1<-data5_1%>%select(质量等级,臭氧浓度)%>%      
  group_by(质量等级)%>%dplyr::summarise(max=max(臭氧浓度))
# 拟合方差分析模型并提取方差分析P值
model<-aov(臭氧浓度~质量等级,data=data5_1)      # 拟合方差分析模型
summary_model<-summary(model)
p_value<-summary_model[[1]][["Pr(>F)"]][1]   # 提取方差分析P值
# 提取方差分析多重比较的字母
HSD<-HSD.test(model,"质量等级")    # 使agricolae包做Tukey-Kramer的HSD检验
df2<-data.frame(HSD$groups)%>%    # 提取多重比较的字母并转换成数据框
  mutate(质量等级=c("中度污染","轻度污染","重度污染","良","优","严重污染"),.before=臭氧浓度) # 添加质量等级列
df<-merge(df1,df2,by="质量等级")   # 按质量等级合并数据框
df$质量等级<-factor(df$质量等级,ordered=TRUE,levels=levels)   # 设置类别顺序
p2<-ggplot(data5_1,aes(x=质量等级,y=臭氧浓度,fill=质量等级))+
  geom_boxplot(fill=cols)+      # 绘制箱线图
  ggtitle("(b) 箱线图 + 方差分析 P 值和多重比较字母")+
  geom_text(data=df,aes(y=max,label=groups),vjust=-0.5,size=6,colour="red2")+ # 添加字母
  annotate("text",x=1.6,y=320,label=paste("Anova: p =",round(p_value,15)))      # 添P值

p1+p2     # 组合图形


# 注：用P值比较结果
TukeyHSD(model)  # 结论相同



#####————————————————————————————————#####
##### 【图5-38】的绘图代码——方差分析
#####————————————————————————————————#####
# 图5-38的绘制代码
library(ggstatsplot)
library(ggplot2)

# 处理数据
df<-read.csv("C:/mydata/chap05/data5_1.csv")
df$质量等级<-factor(df[,3],ordered=TRUE,
  levels=c("优","良","轻度污染","中度污染","重度污染","严重污染"))   # 设置类别顺序

# 绘制箱线图和小提琴图
set.seed(2025)
ggbetweenstats(df,x=质量等级,y=臭氧浓度,
  plot.type="boxviolin",   # 同时绘制箱线图和小提琴图（默认）
  type="parametric",       # 采用参数检验方法（默认）
  bf.message=FALSE,
  centrality.point.args=list(size=4,color="darkred"),# 均值点的大小和颜色
  centrality.label.args=list(size=3,nudge_x=0.2),  # 均值标签的大小和位置偏移量
  #ggthemes::scale_fill_manual(values=cols),
  ggtheme=theme_grey(),                            # 设置图形主题
  ggsignif.args=list(textsize=3,tip_length=0.01)) # 配对P值标签的大小

## ggthemes::theme_fivethirtyeight(),# 调色板
## ggthemes::scale_color_fivethirtyeight(),


#####————————————————————————————————#####
##### 【图5-39】的绘图代码——海盗图
#####————————————————————————————————#####
# 图5-39的绘制代码
library(ggplot2)
library(ggpirate)
library(ggpubr)
library(ggsignif)
library(dplyr)
library(patchwork)

# 处理数据
df<-read.csv("C:/mydata/chap05/data5_1.csv")
df$质量等级<-factor(df$质量等级,ordered=TRUE,
   levels=c("优","良","轻度污染","中度污染","重度污染","严重污染"))
cols=c("green","yellow","orange","red","purple","maroon")

# 图（a）二氧化氮
p1<-ggplot(df,aes(x=质量等级,y=二氧化氮,fill=质量等级)) +
  geom_pirate(violins_params=list(fill="deepskyblue",alpha=0.2,width=0.8))+ # 设置小提琴图的参数
  scale_fill_manual(values=cols)+
  stat_compare_means(method="anova",label.y=165)+ # 设置方差分析P值的位置
  geom_signif(test="t.test",comparisons=list(c("优","良"),c("轻度污染", "中度污染"),
      c("重度污染","严重污染")),    # 列出比较组
        map_signif_level=FALSE)+    # 列出检验P值
  ggtitle("(a) 二氧化氮")

# 图（b）臭氧浓度
p2<-ggplot(df,aes(x=质量等级,y=臭氧浓度,fill=质量等级)) +
  geom_pirate(violins_params=list(fill="deepskyblue",alpha=0.2,width=0.8))+
  scale_fill_manual(values=cols)+
  stat_compare_means(method="anova",label.y=350)+
  geom_signif(test="t.test",comparisons=list(c("优","良"),c("轻度污染", "中度污染"),c("重度污染","严重污染")),map_signif_level=TRUE)+
  ggtitle("(b) 臭氧浓度")

p1+p2   # 组合图形


#####————————————————————————————————#####
##### 【图5-40】的绘图代码——点图+置信区间
#####————————————————————————————————#####
# 图5-40的绘制代码
library(ggplot2)
library(ggbeeswarm)
library(Hmisc)
library(reshape2)
library(dplyr) 
library(lubridate)
library(ggstats)
library(patchwork)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(日期,AQI)%>%
    mutate(日期=as.Date(日期),     # 将日期转化成日期变量
    月份=factor(month(日期,label=TRUE,abbr=TRUE)))%>% # 添加月份因子
    filter(月份<="6月")            # 选出前6个月的数据

# 按月份分组计算t分布置信区间
df1<-df%>%group_by(月份)%>%           # 按月份分组
     dplyr::summarise(Mean=mean(AQI),        # 计算均值
               df=n()-1,              # 使用dplyr包中的n()函数计算自由度
               se=sd(AQI)/sqrt(df+1), # 计算标准误
               E=qt(0.975,df)*se,     # 计算估计误差
               Lower=Mean-E,          # 计算置信下限
               Upper=Mean+E)          # 计算置信上限

# 图（a）t 分布置信区间+原始点
p1<-ggplot(df,aes(x=AQI,y=月份))+
  geom_point(fill="deepskyblue2",shape=21,size=1.2)+  # 绘制点
  geom_point(data=df1,aes( x=Mean,y=月份),size=4,color="red2")+# 绘制均值点
  geom_errorbar(data=df1,aes(x=Mean,y=月份,xmin=Lower,xmax=Upper),
                width=0.3,linewidth=1.2,color="red2")+     # 绘制均值区间
  xlim(0,1.05*max(df$AQI))+
  geom_stripped_rows(odd="lightblue",even="lightyellow",alpha=0.2)+  # 添加行交替背景色（奇数=“浅蓝色”，偶数=“浅黄色”）
  theme_test()+ggtitle("(a) t 分布置信区间+原始点")

# 图（b）bootstrap置信区间+扰动点
set.seed(2025)  # 设置随机数种子以重现图形
p2<-ggplot(df,aes(x=AQI,y=月份))+
 geom_jitter(fill="deepskyblue2",shape=21,size=1.2,height=0.2)+# 绘制扰动点
 stat_summary(fun.data="mean_cl_boot",color="red2",size=0.7,linewidth=1)+
 xlim(0,1.05*max(df$AQI))+
 geom_stripped_rows(odd="lightblue",even="lightyellow",alpha=0.2)+
 theme_test()+ggtitle("(b) bootstrap置信区间+扰动点")

# 图（c）t 分布置信区间+箱线图+威尔金森点
p3<-ggplot(df,aes(x=AQI,y=月份))+
  geom_boxplot(color="steelblue4")+
  geom_dotplot(fill="deepskyblue2",width=0.5,dotsize=1.7,
              binwidth=5,stackdir="center")+ # 绘制威尔金森点图
  geom_pointrange(data=df1,aes(x=Mean,y=月份,xmin=Lower,xmax=Upper),
          size=0.8,linewidth=1,color="red2")+ # 使用数据框df1绘制置信区间
  xlim(0,1.05*max(df$AQI))+
  geom_stripped_rows(odd="lightblue",even="lightyellow",alpha=0.2)+
  theme_test()+ggtitle("(c) t 分布置信区间+箱线图+威尔金森点")

# 图（d）bootstrap置信区间+小提琴图+蜂群点
p4<-ggplot(df,aes(x=AQI,y=月份))+
  geom_violin(color="steelblue4",scale="width")+
  geom_beeswarm(cex=2,shape=21,fill="deepskyblue2",size=1.5)+# 绘制蜂群点
  stat_summary(fun.data="mean_cl_boot",color="red2",size=0.7,linewidth=1)+
  xlim(0,1.05*max(df$AQI))+
  geom_stripped_rows(odd="lightblue",even="lightyellow",alpha=0.2)+
  theme_test()+ggtitle("(d) bootstrap置信区间+小提琴图+蜂群点")

# 组合图形
p1+p2+p3+p4+plot_layout(nrow=2,axes="collect")& # 组合图形并删除重复的y轴和x轴
   guides(fill="none",color="none")             # 删除图例


#####————————————————————————————————#####
##### 【图5-41】的绘图代码——均值的分位数区间
#####————————————————————————————————#####
# 图5-41的绘制代码
library(ggplot2)
library(ggdist)
library(dplyr)
library(reshape2)
library(patchwork)
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")

# 数据处理（从AQI、PM2.5和PM10中各随机抽取样本量为50的样本）
set.seed(2026)
df<-data.frame(AQI=sample(data5_1$AQI,size=50,replace=F),
               PM2.5=sample(data5_1$PM2.5,size=50,replace=F),
               PM10=sample(data5_1$PM10,size=50,replace=F))%>%
    melt(variable.name="指标",value.name="指标值")

# 图（a）威尔金森点+均值的分位数区间
p1<-ggplot(df,aes(x=指标值,y=指标,color=指标,fill=指标))+
    geom_dots(layout="bin",  # 设置点的布局方式为分箱（默认威尔金森点图）
    side="top")+             # 设置点的位置在顶部
    stat_dotsinterval(point_interval="mean_qi",# 生成均值的分位数区间
    color="black",           # 设置区间线的颜色
    .width=c(0.80,0.95),     # 确定区间宽度的概率向量
    interval_size_range=c(0.5,1.2))+  # 设置区间线的宽度范围
    ggtitle("(a) 威尔金森点+均值的分位数区间")

# 图（b）蜂群图+均值的分位数区间
p2<-ggplot(df,aes(x=指标值,y=指标,color=指标,fill=指标))+
    geom_dots(layout="swarm",# 设置点的布局方式为蜂群 
    side="both")+            # 设置点的位置在两侧
    stat_dotsinterval(point_interval="mean_qi",color="black",
                      .width=c(0.80,0.95),
    interval_size_range=c(0.5,1.2))+
    ggtitle("(b) 蜂群点+均值的分位数区间")

# 图（c）云雨图+均值的分位数区间
p3<-ggplot(df,aes(x=指标值,y=指标,fill=指标))+
  stat_halfeye(point_interval="mean_qi",aes(fill=指标),
               .width=c(0.80,0.95))+
  stat_dotsinterval(side="bottom",scale=0.7,slab_linewidth=NA)+
  scale_fill_brewer(palette = "Set2")+
  ggtitle("(c) 云雨图+均值的分位数区间")

# 图（d）眼图（小提琴图）+均值的分位数区间
p4<-ggplot(df,aes(x=指标值,y=指标,fill="指标"))+
  stat_eye(point_interval="mean_qi",aes(fill=指标),.width=c(0.80,0.95))+
  scale_fill_brewer(palette="Set2")+
  ggtitle("(d) 眼图+均值的分位数区间")

p1+p2+p3+p4+plot_layout(nrow=2,axes="collect")&           
    guides(fill="none",color="none")   # 组合图形，删除重复的坐标轴标签







#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####
#####  END
#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####

