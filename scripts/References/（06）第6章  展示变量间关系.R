

###==================================###
###  【第6章】展示变量间关系         ###
###==================================###



#####================================================================#####
#####  6.2  普通散点图
#####================================================================#####

#####————————————————————————————————#####
##### 【图6-3】的绘制代码——散点图+置信带
#####————————————————————————————————#####
# 图6-3的绘制代码
library(ggplot2)
library(patchwork)

df<-read.csv("C:/mydata/chap06/data6_1.csv")

mytheme<-theme(legend.background=element_blank(),  # 移除图例整体边框
               legend.key.height=unit(0.35,"cm"),  # 设置图例键高度
               legend.text=element_text(size=7))   # 设置图例字体大小

p1<-ggplot(data=df,aes(x=总股本,y=每股收益))+
    geom_point(aes(fill=总股本),shape=21,size=2.5,alpha=0.8)+  # 设置点的形状、大小和填充颜色
    stat_ellipse(linetype=6,color="grey50")+  # 添加置信椭圆
    scale_fill_distiller(palette="YlOrRd")+   # 设置调色板
    geom_rug(color="steelblue")+              # 添加地毯图
    stat_smooth(method=lm,color="blue4",fill="deepskyblue")+  # 添加线性拟合线、设置线的颜色和置信带的颜色
    annotate("point",x=mean(df$总股本),y=mean(df$每股收益),shape=21,
              fill="yellow",size=4)+   # 添加均值点
    mytheme+
    theme(legend.position="inside",
          legend.position.inside=c(0.13,0.23))+ # 设置图例位置
    ggtitle("(a) 散点图+地毯图+线性拟合+置信带")

p2<-ggplot(data=df,aes(x=每股净资产,y=每股收益))+
    geom_point(aes(fill=每股净资产),shape=21,size=2.5,alpha=0.8)+
    stat_ellipse(linetype=6,color="grey50")+
    scale_fill_distiller(palette="YlOrRd")+
    geom_rug(position="jitter",color="steelblue")+# 添加扰动后的地毯图
    stat_smooth(method=loess,color="blue4",fill="deepskyblue")+
    annotate("point",x=mean(df$每股净资产),y=mean(df$每股收益),
             shape=21,fill="yellow",size=4)+   # 添加均值点
    mytheme+
    theme(legend.position="inside",
          legend.position.inside=c(0.92,0.27))+
    labs(fill="每股\n净资产")+    # 修改图例标题（换行）
    ggtitle("(b) 散点图+地毯图+loess拟合+置信带")

p1+p2     # 组合图形


#####————————————————————————————————#####
##### 【图6-4】的绘制代码——散点图+置信带+预测带
#####————————————————————————————————#####
# 图6-4的绘制代码
library(ggplot2);library("ggformula");library(patchwork)
data6_1<-read.csv("C:/mydata/chap06/data6_1.csv")

# 图（a）散点图+置信带
p1<-ggplot(data=data6_1,aes(x=每股净资产,y=每股收益))+
   geom_point(fill=5,shape=21,size=2,alpha=0.8)+
   geom_lm(interval="confidence",color="red",fill="blue")+ # 添加拟合直线和置信带
   theme_bw()+
   ggtitle("(a) 散点图+线性拟合+置信带")

# 图（b）散点图+置信带+预测带
p2<-ggplot(data=data6_1,aes(x=每股净资产,y=每股收益))+
   geom_point(fill=5,shape=21,size=2,alpha=0.8)+
   geom_lm(interval="confidence",fill="blue")+# 添加拟合直线和置信带
   geom_lm(interval="prediction",color="red",fill="skyblue")+  # 添加拟合直线和预测带
   theme_bw()+
   ggtitle("(b) 散点图+线性拟合+置信带+预测带")

 p1+p2      # 组合图形


#####————————————————————————————————#####
##### 【图6-5】的绘制代码——散点图添+相关系数+回归线+回归模型信息
#####————————————————————————————————#####
# 图6-5的绘制代码
library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(ggpmisc)  # 为使用stat_poly_eq函数添加回归信息
df<-read.csv("C:/mydata/chap06/data6_1.csv")

mytheme<-theme(legend.background=element_blank(),  # 移除图例整体边框
        legend.key.height=unit(0.35,"cm"),         # 设置图例键高度
        legend.text=element_text(size=7))          # 设置图例字体大小
p1<-ggplot(data=df,aes(x=总股本,y=每股收益))+
  geom_point(aes(fill=总股本),shape=21,size=2.5)+
  geom_rug(color="steelblue")+                # 添加地毯图
  scale_fill_gradientn(colors=rev(brewer.pal(11,"RdYlBu")))+# 设置调色板
  stat_poly_line(formula=y~x) +         # 拟合线性模型并绘制回归线和置信带
  stat_poly_eq(use_label(c("eq","R2","P")),formula=y~x,
       label.x="right",label.y="top")+  # 添加回归方程、决定系数R方、F检验的P值 
    
  mytheme+
  theme(legend.position="inside",legend.position.inside=c(0.13,0.2))+
  ggtitle("(a) 散点图+线性拟合+模型信息")

p2<-ggplot(data=df,aes(x=每股净资产,y=每股收益))+
  geom_point(aes(fill=每股净资产),shape=21,size=2.5)+
  geom_rug(color="steelblue")+                # 添加地毯图
  scale_fill_gradientn(colors=rev(brewer.pal(11,"RdYlBu")))+
  stat_poly_line(formula=y~poly(x,2,raw=TRUE)) + # 拟合二次模型（或写成formula=y~x+I(x^2)）
  stat_poly_eq(use_label(c("eq","adj.R2")),formula=y~poly(x,2,raw=TRUE),
       label.x="left",label.y="top")+       # 添加回归方程表达式和R方
  mytheme+
  theme(legend.position="inside",legend.position.inside=c(0.92,0.24))+
  labs(fill="每股\n净资产",title="(b) 散点图+多项式拟合+模型信息")

p1+p2     # 组合图形


#####————————————————————————————————#####
##### 【图6-6】的绘制代码——散点图+回归摘要表
#####————————————————————————————————#####
# 图6-6的绘制代码
library(ggplot2)
library(ggpmisc)
library(RColorBrewer)
library(patchwork)

df<-read.csv("C:/mydata/chap06/data6_1.csv")

mytheme<-theme_test()+theme(legend.background=element_blank(), # 移除图例整体边框
         legend.key.height=unit(0.35,"cm"),  # 设置图例键高度
         legend.text=element_text(size=7))   # 设置图例字体大小

p1<-ggplot(data=df,aes(x=总股本,y=每股收益))+
  geom_point(aes(fill=总股本),shape=21,size=2.5,alpha=0.9)+
  scale_fill_gradientn(colors=rev(brewer.pal(11,"RdYlBu")))+
  geom_smooth(method="lm",formula=y~x) +
  stat_fit_tb(method="lm",
              method.args=list(formula=y~x),
              tb.vars = c(Parameter="term",          # 表格中的变量
                          Estimate="estimate",       # 点估计值
                          "s.e."="std.error",        # 标准误
                          "italic(t)"="statistic",   # t统计量
                          "italic(P)"="p.value"),    # 检验的P值
              label.y=0.98, label.x=0.98,            # 设置位置
              parse=TRUE)+
  mytheme+
  theme(legend.position="inside",legend.position.inside=c(0.12,0.18))+
  labs(title="(a) 总股本与每股收益",subtitle="(散点图+线性拟合+模型摘要表)")

p2<-ggplot(data=df,aes(x=每股净资产,y=每股收益))+
  geom_point(aes(fill=每股净资产),shape=21,size=2.5,alpha=0.9)+
  scale_fill_gradientn(colors=rev(brewer.pal(11,"RdYlBu")))+
  geom_smooth(method="lm",formula=y~x) +
  stat_fit_tb(method="lm",method.args=list(formula=y~x),
     tb.vars = c(Parameter="term",Estimate="estimate",
                "s.e."="std.error","italic(t)"="statistic",
                "italic(P)"="p.value"),label.y=0.98,label.x=0.02,parse=TRUE)+
  mytheme+
  theme(legend.position="inside",legend.position.inside=c(0.92,0.22))+
  labs(fill="每股\n净资产",title="(b) 每股净资产与每股收益",
       subtitle="(散点图+线性拟合+模型摘要表)")

p1+p2     # 组合图形


#####————————————————————————————————#####
##### 【图6-7】的绘制代码——散点图+残差线
#####————————————————————————————————#####
# 图6-7的绘制代码
library(ggplot2)
library(dplyr)
library(ggpmisc)
library(RColorBrewer)
library(patchwork)

# 处理数据
d<-read.csv("C:/mydata/chap06/data6_1.csv")
df1<-d%>%select(股票类型,每股收益,每股净资产)%>% # 选择变量
    filter(股票类型=="食品类")       # 筛选出食品类股票
df2<-d%>%select(股票类型,每股收益,每股净资产)%>%
    filter(股票类型=="汽车类")       # 筛选出汽车类股票

# 绘制散点图
p1<-ggplot(data=df1,aes(x=每股净资产,y=每股收益))+
  geom_point(aes(fill=每股收益,color=每股收益),shape=21,size=3)+
  geom_smooth(method="lm",formula=y~x) +
  stat_poly_line(formula=y~x) +         # 拟合线性模型并绘制回归线和置信带
  stat_poly_eq(use_label(c("eq","R2","P")),formula=y~x,
       label.x="left",label.y="top")+# 添加回归方程、决定系数R方、F检验P值 
   stat_fit_deviations(color="red3")+
   scale_fill_gradientn(colors=rev(brewer.pal(11,"RdYlBu")))+
   theme_bw()+
   theme(legend.background=element_blank(),  # 移除图例整体边框
        panel.grid=element_blank(),                    # 移除网格线
        legend.position="inside",legend.justification=c("right","bottom"),
                                                       # 设置图例位置
        legend.key.height=unit(0.35,"cm"),             # 设置图例键高度
        plot.title=element_text(size=13))+             # 设置标题字体大小
   guides(color="none")+
   ggtitle("(a) 食品类股票","(每股收益与每股净资产的线性拟合及残差)")

p2<-p1%+%df2+
   ggtitle("(b) 汽车类股票","(每股收益与每股净资产的线性拟合及残差)")

p1+p2     # 组合图形


#####————————————————————————————————#####
##### 【图6-8】的绘制代码——散点图+边际图
#####————————————————————————————————#####
# 图6-8的绘制代码
library(ggplot2)
library(ggExtra)
library(viridis)

df<-read.csv("C:/mydata/chap06/data6_1.csv")

# 绘制散点图
p<-ggplot(data=df,aes(x=总股本,y=每股收益))+theme_grey(base_size=10)+
   geom_point(aes(fill=总股本),shape=21,size=2,alpha=0.8)+
   scale_fill_viridis(option="plasma")+              # 设置调色板
   theme(plot.title=element_text(size=12),           # 设置标题字体大小
         legend.background=element_blank(),  # 移除图例整体边框
         legend.position="inside",legend.position.inside=c(0.13,0.28),
         legend.key.height=unit(0.35,"cm"),  # 设置图例键高度
         legend.text=element_text(size=7))  # 设置图例字体大小
# 添加边际图
p1<-p+ggtitle("(a) 散点图+边际直方图")
p11<-ggMarginal(p1,type="histogram",color="grey50",
     xparams=list(fill="gold"),yparams=list(fill="red"),alpha=0.3)
                     # 添加边际直方图，设置边际图的边线颜色和填充颜色

p2<-p+ggtitle("(b) 散点图+边际核密度图")
p22<-ggMarginal(p2,type="density",color="grey50",
     xparams=list(fill="gold"),yparams=list(fill="red"),alpha=0.3)
                                                    # 添加边际核密度图

p3<-p+geom_rug(position="jitter",linewidth=0.5,color="steelblue")+# 添加地毯图
    stat_smooth(method=lm,color="red",fill="blue4",linewidth=0.8)+  # 添加线性拟合线、设置线的颜色和置信带的颜色
    ggtitle("(c) 散点图+地毯图+线性拟合+边际箱线图")
p33<-ggMarginal(p3,type="boxplot",color="grey50",
    xparams=list(fill="gold"),yparams=list(fill="red"),alpha=0.3)# 添加边际箱线图

p4<-p+geom_rug(color="steelblue")+    # 添加地毯图
    stat_smooth(method=loess,color="red",fill="blue4",linewidth=0.8)+
                                                # 添加局部加权回归拟合线
    ggtitle("(d) 散点图+地毯图+loess拟合+边际小提琴图")
p44<-ggMarginal(p4,type="violin",color="grey50",
     xparams=list(fill="gold"),yparams=list(fill="red"),alpha=0.3)   # 添加边际小提琴图

gridExtra::grid.arrange(p11,p22,p33,p44,ncol=2)  # 组合图形



#####================================================================#####
#####  6.2  密度散点图和分箱散点图
#####================================================================#####

#####————————————————————————————————#####
#####  【图6-9】的绘制代码——密度散点图
#####————————————————————————————————#####
# 图6-9的绘制代码
library(ggplot2)
library(viridis)     # 色盲友好的颜色配色包
library(patchwork)

# 构建数据框
set.seed(1234)
n=5000
df<-data.frame(x=c(rnorm(n,10,5),rnorm(n,20,6),rnorm(n,30,5)),
               y=c(rnorm(n,20,5),rnorm(n,8,5),rnorm(n,30,5)))

# 图（a）普通散点图
p<-ggplot(data=df,aes(x=x,y=y))+theme_test()
p1<-p+geom_point()+
  geom_vline(xintercept=mean(df$x),linetype="twodash",
             color="grey50",linewidth=0.5)+
  geom_hline(yintercept=mean(df$y),linetype="twodash",
             color="grey50",linewidth=0.5)+ 
  ggtitle("(a) 普通散点图")

# 图（b）密度散点图
p2<-p+stat_density_2d(geom="raster",aes(fill=after_stat(density)),contour=FALSE)+ # 绘制二维核密度图
    scale_fill_viridis_c(option="H")+
    guides(fill="none")+                          # 删除图例
    ggtitle("(b) 二维核密度图")

# 图（c）散点图+密度等高线
p3<-p+geom_point(color="grey20")+                 # 绘制散点图
    geom_density_2d()+                            # 添加等高线
    ggtitle("(c) 散点图+密度等高线")

# 图（d）散点图+密度等高线带
p4<-p+geom_point(color="grey20")+
    geom_density_2d_filled(alpha=0.8)+
    geom_density_2d(linewidth=0.25,colour="black")+ # 设置等高线宽度和颜色
    guides(fill="none")+
    ggtitle("(d) 散点图+密度等高线带")

p1+p2+p3+p4+plot_layout(nrow=2,byrow=TRUE)  # 按2行组合图形，按行填充各图


#####————————————————————————————————#####
#####  【图6-10】的绘制代码——分箱散点图
#####————————————————————————————————#####
# 图6-10的绘制代码（使用图6-9构建的数据df）
library(ggplot2)
library(viridis)
library(patchwork)

# 构建数据框
set.seed(1234)
n=5000
df<-data.frame(x=c(rnorm(n,10,5),rnorm(n,20,6),rnorm(n,30,5)),
               y=c(rnorm(n,20,5),rnorm(n,8,5),rnorm(n,30,5)))

p<-ggplot(df,aes(x=x,y=y))+
   theme_test()+
   theme(legend.position="inside",legend.position.inside=c(0.2,0.92),
         legend.background=element_blank(),  # 移除图例整体边框
         legend.key.height=unit(0.35,"cm"),
         legend.key.width=unit(0.4,"cm"),
         legend.text=element_text(size=7),
         legend.direction="horizontal")

# 图（a）bins=30
p1<-p+geom_hex(bins=30,linewidth=0.3,color="black")+   # 六边形分箱
    scale_fill_viridis_c(option="H")+             # 选择配色方案
    ggtitle("(a) bins=30")

# 图（b）bins=20
p2<-p+geom_hex(bins=20,linewidth=0.3,color="black")+   # 六边形分箱
    scale_fill_viridis_c(option="H")+             # 选择配色方案
    ggtitle("(b) bins=20")

p1+p2    # 组合图形


#####————————————————————————————————#####
#####  【图6-11】的绘制代码——3D分箱散点图
#####————————————————————————————————#####
# 图6-11（a）的绘制代码（将H替换成C即为图（b））
library(ggplot2)
library(viridis)     # 色盲友好的颜色配色包
library(rayshader)   # 将ggplot2图形转换成3D图形的包

set.seed(1234)
n=5000  
df<-data.frame(x=c(rnorm(n,10,5),rnorm(n,20,6),rnorm(n,30,5)),
               y=c(rnorm(n,20,5),rnorm(n,8,5),rnorm(n,30,5)))
p<-ggplot(df,aes(x=x,y=y)) +
  geom_hex(bins=20,linewidth=0.5,color="black")+  # 绘制六边形分箱散点图
  scale_fill_viridis_c(option="H")
plot_gg(p,width=5,height=5,scale=300,multicore=FALSE) # 转换成3D图形



#####================================================================#####
#####  6.3  分组散点图
#####================================================================#####

#####————————————————————————————————#####
##### 【图6-12】的绘制代码——同时按颜色和点型分组
#####————————————————————————————————#####
# 图6-12的绘制代码
library(ggplot2)
library(patchwork)
data6_1<-read.csv("C:/mydata/chap06/data6_1.csv")

# 图（a）用点的大小和颜色分组（上市板块）
mytheme<-theme(axis.text.x = element_text(hjust =0), # 调整x轴标签位置
      legend.position="inside",legend.justification=c("right","bottom"),
      legend.key.height=unit(0.3,"cm"),       # 设置图例键高度
      legend.key.width=unit(0.3,"cm"),        # 设置图例键高度
      legend.background=element_blank())      # 移除图例整体边框

p1<-ggplot(data=data6_1,aes(x=每股净资产,y=每股收益,size=上市板块,color=上市板块,alpha=0.5))+
  geom_point()+
  scale_color_brewer(palette="Set1")+
  scale_alpha(guide="none")+               # 删除alpha的图例
  mytheme+ggtitle("(a) 用点的大小和颜色分组(上市板块)")

# 图（b）用点的形状和颜色分组（股票类型）
p2<-ggplot(data=data6_1,aes(x=每股净资产,y=每股收益,shape=股票类型,color=股票类型))+
  geom_point(size=2.5)+                     # 设置点的大小
  scale_shape_manual(values=c(1,10,16,17))+ # 设置点的形状
  scale_color_brewer(palette="Set1")+
  scale_alpha(guide="none")+
  mytheme+ggtitle("(b) 用点的形状和颜色分组(股票类型)")

p1+p2     # 组合图形


#####————————————————————————————————#####
##### 【图6-13】的绘制代码——按股票类型和上市板块分面的散点图
#####————————————————————————————————#####
# 图6-13的绘制代码
library(ggplot2);library(ggpmisc)
data6_1<-read.csv("C:/mydata/chap06/data6_1.csv")

ggplot(data=data6_1,aes(x=每股净资产,y=每股收益,group=股票类型,color=上市板块))+
  geom_point(aes(fill=上市板块),color="black",shape=21,size=2)+ # 设置点
  stat_poly_line(formula=y~x) +   # 拟合线性模型并绘制回归线和置信带
  theme(legend.position="none",                 # 移除图例
        panel.spacing.x=unit(0.2,"lines"),      # 设置子图的x轴间距
        panel.spacing.y=unit(0.2,"lines"))+     # 设置子图的y轴间距
  facet_grid(上市板块~股票类型,scale="free")# 按股票类型和上市板块交叉分面


#####————————————————————————————————#####
##### 【图6-14】的绘制代码——分组散点图+回归信息
#####————————————————————————————————#####
# 图6-14的绘制代码
library(ggplot2);library(ggpmisc)
library(patchwork)

df<-read.csv("C:/mydata/chap06/data6_1.csv")

p1<-ggplot(data=df,aes(x=每股净资产,y=每股收益,fill=上市板块,color=上市板块))+
  geom_point(color="black",shape=21,size=2,alpha=0.6)+
  stat_poly_line(formula=y~x) +   # 拟合线性模型并绘制回归线和置信带
  stat_poly_eq(use_label("eq"),formula=y~x,
      label.x="left",label.y="top")+  # 添加回归方程
  theme(legend.position="inside",legend.justification=c("right","bottom"),
          legend.background=element_blank(),  # 移除图例整体边框
          legend.text=element_text(size=7))+  # 设置图例字体大小
  ggtitle("(a) 按上市板块分组")

p2<-p1+facet_grid(上市板块~.)+    
  stat_poly_eq(use_label("R2"),formula=y~x,label.x=0.96,label.y=0.1)+  # 添加决定系数
  theme(legend.position="none")+ggtitle("(b) 按上市板块分面")

p1+p2+plot_layout(widths=c(2,1)) # 组合图形，列宽为2:1


#####————————————————————————————————#####
##### 【图6-15】的绘制代码——分组散点图+边际图
#####————————————————————————————————#####
# 图6-15的绘制代码
library(ggplot2)
library(ggside)
library(patchwork)
df<-read.csv("C:/mydata/chap06/data6_1.csv")

# 绘制散点图
p<-ggplot(data=df,aes(x=每股净资产,y=每股收益,color=上市板块))+
  geom_point(size=2)

# 添加边际图
mytheme<-theme(legend.position="inside",legend.position.inside=c(0.8,0.15),
         legend.background=element_blank(),
         legend.key.height=unit(0.35,"cm"),
         legend.key.width=unit(0.4,"cm"),
         legend.text=element_text(size=7),
         axis.text.x=element_text(size=7,angle=90,hjust=0.5),
         axis.text.y=element_text(size=7))

p1<-p+geom_xsideboxplot(aes(y=上市板块),orientation="y")+ # 绘制x轴的边际箱线图,图层方向为y
  geom_ysideboxplot(aes(x=上市板块),notch=TRUE,orientation="x")+# 绘制y轴的边际箱线图,图层方向为x
  mytheme+ggtitle("(a) 边际图为箱线图")

p2<-p+geom_xsidedensity()+       # 绘制x轴的边际核密度图
  geom_ysidedensity()+           # 绘制y轴的边际核密度图
  guides(x=guide_axis(check.overlap=TRUE),
         y=guide_axis(check.overlap=TRUE))+# 删除x轴和y轴刻度重叠标签
  mytheme+ggtitle("(b) 边际图为核密度图")

p1+p2            # 使用patchwork包组合图形


#####————————————————————————————————#####
##### 【图6-16】的绘制代码——分面散点图+边际图
#####————————————————————————————————#####
# 图6-16的绘制代码
library(ggplot2);library(ggside)
data6_1<-read.csv("C:/mydata/chap06/data6_1.csv")
p<-ggplot(data=data6_1,aes(x=每股净资产,y=每股收益,group=股票类型))+
  geom_point(aes(fill=上市板块),color="black",shape=21,size=2)+ # 设置点
  theme(legend.position="bottom",legend.key.height=unit(0.5,"cm"))+
  facet_grid(上市板块~股票类型,scale="free")# 按上市板块和股票类型分面

p+geom_xsidedensity(aes(y=after_stat(density),fill=上市板块))+# 绘制边际核密度图
  geom_ysidedensity(aes(x=after_stat(density),color=股票类型))+
  ggside(collapse = "y")+    # 设置y轴边际图折叠
  guides(x=guide_axis(check.overlap=TRUE),
         y=guide_axis(check.overlap=TRUE))# 删除x轴和y轴刻度重叠标签



#####================================================================#####
#####  6.4  散点图矩阵和相关系数矩阵
#####================================================================#####

#####————————————————————————————————#####
##### 【图6-17】的绘制代码——散点图矩阵
#####————————————————————————————————#####
# 图6-17的绘制代码
library(GGally);library(ggplot2)
data6_1<-read.csv("C:/mydata/chap06/data6_1.csv")

# （a）对角线上方绘制相关系数
ggpairs(data6_1,columns=3:8,                     # 选择图变量
  lower=list(continuous=wrap(size=0.8,
             color="red4","smooth",method="lm",se=TRUE)),
                              # 对角线下方绘制散点图及线性回归线和置信带
  upper=list(continuous=wrap("cor",color="red4")),# 对角线上方绘制相关系数
  diag=list(continuous=wrap("densityDiag",color="steelblue")))+ # 对角线上绘制核密度曲线
  theme_test()+theme(plot.title=element_text(size=20))+
  ggtitle("(a) 对角线上方绘制相关系数")

# （b）对角线上方绘制密度等高线
ggpairs(data6_1,columns=3:8,
  lower=list(continuous=wrap(size=0.8,
             color="blue4","smooth",method="loess",se=TRUE)),
                             # 对角线下方绘制散点图及loess回归线和置信带
  upper=list(continuous=wrap("density",color="blue4")),# 对角线上方绘制密度等高线
  diag=list(continuous=wrap("barDiag",bins=20,color="steelblue")))+ # 对角线上绘制直方图
  theme_test()+theme(plot.title=element_text(size=20))+
  ggtitle("(b) 对角线上方绘制密度等高线")


#####————————————————————————————————#####
##### 【图6-18】的绘制代码——GGally函数绘制的分组散点图矩阵
#####————————————————————————————————#####
# 图6-18的绘制代码
library(GGally)
data6_1<-read.csv("C:/mydata/chap06/data6_1.csv")

# 图（a） 按股票类型分组
ggpairs(data6_1,columns=3:5,
    ggplot2::aes(color=股票类型),
    diag=list(continuous=wrap("densityDiag",alpha=0.3)))+
    ggplot2::scale_color_brewer(palette = "Set1")+ 
    ggtitle("(a) 按股票类型分组")

# 图（b） 按上市板块分组
ggpairs(data6_1,columns=3:5,
    ggplot2::aes(color=上市板块),
    diag=list(continuous=wrap("densityDiag",alpha=0.3)))+ 
    ggplot2::scale_color_brewer(palette = "Set1")+
    ggtitle("(b) 按上市板块分组")


#####————————————————————————————————#####
##### 【图6-19】的绘制代码——密度估计散点图矩阵
#####————————————————————————————————#####
# 图6-10（a）的绘制代码（设置pointcolor=c("A","B","C","D","E")[4]即为图（b））
## 安装包：：devtools::install_github("Chuanping-Zhao/corhex")
library(corhex)
library(ggplot2)
library(dplyr)

# 处理数据
data6_1<-read.csv("C:/mydata/chap06/data6_1.csv")
df<-data6_1%>%select(-c("股票类型","上市板块"))%>%   # 选择绘图变量
              mutate(index=factor(1:nrow(data6_1)))       # 添加索引列

# 绘制散点图矩阵
corpairs(dt=df,id.col="index",   # 设置索引列
  cor.method=c("pearson", "kendall", "spearman")[1], # 选择相关系数的计算方法为pearson
  bin=30,     # 设置分箱数（默认为50）
  plottype=c("hex","point")[2],  # 下部面板的绘图类型，默认为“点”（可选分箱hex）
  pointcolor=c("A","B","C","D","E")[3],  # 设置散点图调色板名称的向量，默认值为“C”
  pointsize=1,  # 设置点的大小
  kde2d.n=50)+   # 设置密度计算的网格大小，默认值为50
  ggtitle("(a) 配色方案C")+
  theme(plot.title = element_text(size = 18, hjust = 0.5))+
  guides(x=guide_axis(check.overlap=TRUE),y=guide_axis(check.overlap=TRUE))  # 删除x轴和y轴刻度重叠标签


#####————————————————————————————————#####
##### 【图6-20】的绘制代码——相关系数矩阵
#####————————————————————————————————#####
# 图6-20的绘制代码
require(ggplot2)
library(patchwork)
data6_1<-read.csv("C:/mydata/chap06/data6_1.csv")

# 图（a）ggCor函数绘制相关系数矩阵
library(ggiraphExtra)
mytheme<-theme(axis.text.x=element_text(size=8,angle=20,
                                        hjust=0.5,vjust=0.5), 
               axis.text.y=element_text(size=8))

p1<-ggCor(data6_1,whaw=1,    # 计算Pearson相关系数
    digits=4,                # 保留4为小数
    label=3,                 # 显示相关系数及其检验的P值
    mode=2)+                 # 绘制半角矩阵
  mytheme+
  theme(legend.position="inside",
        legend.position.inside=c(0.85,0.75))+
  ggtitle("(a) ggCor函数绘制的相关系数矩阵")

# 图（b）ggcorrplot函数绘制相关系数矩阵
library(ggcorrplot)
r.mat<-cor(data6_1[,3:8])            # 计算相关系数矩阵
p.mat <- cor_pmat(data6_1[,3:8])     # 计算相关系数检验的P值矩阵
p2<-ggcorrplot(r.mat,lab=TRUE,lab_size=3.5,# 显示相关系数标签并设置字体大小
   type="lower",          # 绘制半角矩阵
   p.mat=p.mat,           # 用于标记的P值矩阵
   insig="pch",           # 不显著的相关系数显示为符号
   outline.color="white", # 设置轮廓线颜色
   title="(b) ggcorrplot函数绘制的相关系数矩阵")+
  mytheme+
  theme(legend.position="inside",legend.position.inside=c(0.15,0.75))
   
p1+p2    # 组合图形


#####————————————————————————————————#####
##### 【图6-21】的绘制代码——相关系数矩阵
#####————————————————————————————————#####
# 图6-21的绘制代码
## 安装包：devtools::install_github("caijun/ggcorrplot2")
library(ggcorrplot2)
library(ggplot2)
library(psych)     # 为使用corr.test函数计算相关系数及检验矩阵
library(patchwork)

# 计算相关系数及其检验的P值矩阵
data6_1<-read.csv("C:/mydata/chap06/data6_1.csv")
cor<-corr.test(data6_1[,3:8],method="pearson")  # 计算Pearson相关系数矩阵
r.mat<-cor$r                 # 提取相关系数矩阵
p.mat<-cor$p                 # 提取检验的p值矩阵

# 图（a）相关系数和椭圆叠加显著性标记
p1<-ggcorrplot.mixed(r.mat,  # 绘制相关系数矩阵
  upper="number",            # 上半角为相关系数
  lower="ellipse",           # 下半角为椭圆
  p.mat = p.mat,
  number.digits=2,           # 相关系数小数位数为2
  insig="label_sig",         # 绘制显著性水平标记
  sig.lvl=c(0.05,0.01,0.001),# 标记“*”、“**”、“***”表示显著性水平依次为0.05、0.01、0.001）
  pch.cex=5)+                # 设置标记大小
  ggtitle("(a) 相关系数和椭圆叠加显著性标记")

# 图（b）圆形和方形叠加选择性标记
p2<-ggcorrplot.mixed(r.mat,upper="circle",lower="square",# 上半角为圆,下半角为方形
  p.mat=p.mat,insig="label_sig",sig.lvl=c(0.05,0.01,0.001))+
  ggtitle("(b) 圆形和方形叠加显著性标记")

p1+p2+plot_layout(guides="collect")  # 组合图形并共享图例



#####================================================================#####
#####  6.5  3D散点图和气泡图
#####================================================================#####

#####————————————————————————————————#####
##### 【图6-22】的绘制代码——3D 散点图
#####————————————————————————————————#####
# 图6-22的绘制代码
library(scatterplot3d)
data6_1<-read.csv("C:/mydata/chap06/data6_1.csv")
attach(data6_1)
par(mfrow=c(2,2),cex=0.8,cex.axis=0.9,font.main=1)

# 图（a）
s3d<-scatterplot3d(x=每股净资产,y=净资产收益率,z=每股收益,
  col.axis="blue",col.grid="lightblue",pch=10,type="p",
  highlight.3d=TRUE,cex.lab=1,mar=c(3,3,2,2),main="(a) type=p")

# 图（b）
s3d<-scatterplot3d(x=每股净资产,y=净资产收益率,z=每股收益,
  col.axis="blue",col.grid="lightblue",pch=16,
  highlight.3d=TRUE,type="h",cex.lab=1,mar=c(3,3,2,2),main="(b) type=h")

# 图（c）
s3d<-scatterplot3d(x=每股净资产,y=净资产收益率,z=每股收益,
  col.axis="blue",col.grid="lightblue",pch=6,highlight.3d=TRUE,
  type="h",box=FALSE,cex.lab=1,mar=c(3,3,2,2),main="(c) box=FALSE")

# 图（d）
s3d<-scatterplot3d(x=每股净资产,y=净资产收益率,z=每股收益,
  col.axis="blue",col.grid="lightblue",pch=16,highlight.3d=TRUE,
  type="h",box=TRUE,cex.lab=1,mar=c(3,3,2,2),main="(d) 添加二元回归面")
fit<-lm(每股收益~每股净资产+净资产收益率)
s3d$plane3d(fit,col="grey30")


#####————————————————————————————————#####
##### 【图6-24】的绘制代码——ggplot2绘制的气泡图
#####————————————————————————————————#####
# 图6-24的绘制代码
library(ggplot2)
library(RColorBrewer)
library(patchwork)
df<-read.csv("C:/mydata/chap06/data6_1.csv")

# 图（a）气泡大等于总股本
mytheme<-theme(panel.grid.minor=element_blank(),   # 移除次网格线
               legend.background=element_blank(),  # 移除图例整体边框
         legend.position="inside",legend.justification=c("right","bottom"),
               legend.key.height=unit(0.35,"cm"),  # 设置图例键高度
               legend.text=element_text(size=7))   # 设置图例字体大小

p1<-ggplot(df,aes(x=每股净资产,y=每股收益,color=总股本))+
  geom_point(aes(size=总股本,fill=总股本),
             shape=21,color="black",alpha=0.8)+  # 设置气泡大小=总股本
  scale_size(range=c(1,7))+                      # 设置点的大小
  guides(size="none")+                           # 删除size的图例
  scale_fill_distiller(name="总股本",palette="Spectral")+# 设置调色板
  mytheme+
  ggtitle("(a) 气泡大小 = 总股本")

# 图（b）按上市板块分组
p2<-ggplot(df,aes(x=每股净资产,y=每股收益,fill=上市板块))+
  geom_point(aes(size=总股本),shape=21,color="black",alpha=0.6)+
  scale_size(range=c(1,7))+                  # 设置点的大小
  guides(size="none")+                       # 删除size的图例
  annotate("text",x=6,y=3.2,label="气泡大小 = 总股本",size=4)+# 添加注释文本
  mytheme+
  ggtitle("(b) 按上市板块分组")

p1+p2            # 组合图形


#####————————————————————————————————#####
##### 【图6-25】的绘制代码——分面气泡图
#####————————————————————————————————#####
# 图6-25的绘制代码
library(ggplot2)
library(RColorBrewer)
library(patchwork)

df<-read.csv("C:/mydata/chap06/data6_1.csv")

# 图（a）按股票类型分面
p<-ggplot(df,aes(x=每股收益,y=净资产收益率,size=每股净资产))+
  scale_size(range=c(1,6))+
  scale_color_brewer(palette="Set1")+ # 设置调色板
  theme(panel.grid.minor=element_blank())         # 去掉次网格线
p1<-p+geom_point(aes(fill=股票类型),shape=21,color="black",alpha=0.6)+
  facet_grid(.~股票类型)+
  guides(fill="none")+
  ggtitle("(a) 按股票类型分面")

# 图（b）按上市板块分面
p2<-p+geom_point(aes(fill=上市板块),shape=21,color="black",alpha=0.6)+
  facet_grid(.~上市板块)+
  guides(fill="none")+
  ggtitle("(b) 按上市板块分面")

p1+p2+plot_layout(heights=c(1,1.3),       # 组合图形，行高为1:1.3
       guides="collect")&theme(legend.position="bottom")  





#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####
#####  END
#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####


