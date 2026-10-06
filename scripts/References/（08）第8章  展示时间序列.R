

###==================================###
###  【第8章】展示时间序列           ###
###==================================###


#####================================================================#####
#####  8.1  展示序列的变动特征
#####================================================================#####

#####————————————————————————————————#####
##### 【图8-1】的绘制代码——折线图
#####————————————————————————————————#####
# 图8-1的绘制代码（数据：data5_1）
library(ggplot2);library(reshape2)
library(dplyr);library(ggsci);library(patchwork)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(-c(AQI,质量等级))%>%          # 删除不需要的变量
   mutate(日期=as.Date(日期))%>%                   # 将日期设置为日期变量
   melt(id.vars="日期",variable.name="指标",value.name="指标值") # 融合数据

# 图（a）按指标分组
p<-ggplot(df,aes(x=日期,y=指标值,color=指标))+    # 设置x轴、y轴和线的颜色
   geom_line(linewidth=0.5)+                      # 绘制折线图
   scale_color_npg()                              # 使用Nature的配色
p1<-p+scale_x_date(expand=c(0,0),date_breaks="1 month",date_labels="%b")+# 设置x轴间隔为1个月
   theme(legend.position="inside",legend.justification="top",
         legend.background=element_blank())+
   guides(color=guide_legend(nrow=2,title=NULL))+ # 图例排成2行，去掉标题
   ggtitle("(a) 按指标分组")

# （b）按指标分面
p2<-p+scale_x_date(expand=c(0,0),date_breaks="3 month",date_labels="%b")+# 设置x轴间隔为3个月
   facet_wrap(~指标,ncol=3,scale="free")+  # 按指标3列分面，自由设置坐标轴
   guides(color="none")+
   ggtitle("(b) 按指标分面")

p1/p2+plot_layout(heights=c(1,1.6))# 组合图形，行高为1:1.6


#####————————————————————————————————#####
##### 【图8-2】的绘制代码——高亮显示折线图
#####————————————————————————————————#####
# 图8-2的绘制代码（使用图8-1绘制的p1和p2）
library(gghighlight)   # 为使用gghighlight函数高亮显示某类数据
library(ggpol)         # 为使用geom_tshighlight函数添加高亮显示矩形
library(patchwork)

# 图（a）高亮显示臭氧浓度
p11<-p1+gghighlight(指标=="臭氧浓度",label_key=指标,# 设置高亮显示的变量和映射的列名
  unhighlighted_params=list(linewidth=0.5,colour=NULL,alpha=0.2))+ # 设置非高亮显示
  theme_test()+
  ggtitle("(a) 高亮显示臭氧浓度")

# （b）高亮显示第3季度
p22<-p2+geom_tshighlight(aes(xmin=as.Date("01/07/2025",format="%d/%m/%Y"), 
   xmax=as.Date("30/09/2025",format="%d/%m/%Y")),# 设置x轴的最小值和最大值
   color="grey",size=0.3,fill="steelblue",alpha=0.005)+
   ggtitle("(b) 高亮显示第3季度")

p11/p22+plot_layout(heights=c(1,1.6))# 组合图形，行高为1:1.6


#####————————————————————————————————#####
##### 【图8-3】的绘制代码——按月和按周分面的折线图
#####————————————————————————————————#####
# 图8-3的绘制代码
library(ggplot2);library(dplyr);library(lubridate);library(patchwork)
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")

# 处理数据
df1<-data5_1%>%mutate(日期=as.Date(日期),         # 将日期转化成日期变量
   季度=factor(quarter(日期,type="year.quarter")),# 在数据框中添加季度因子
   月份=factor(month(日期)),                      # 在数据框中添加月份因子
   week=factor(week(日期)))                      # 在数据框中添加周因子

# 图（a）按季分面（全年）
p1<-ggplot(df1,aes(x=日期,y=AQI,color=季度))+
   geom_ribbon(aes(ymin=mean(AQI)-2*sd(AQI),ymax=mean(AQI)+2*sd(AQI),
     fill=季度),alpha=0.3,linewidth=0.1)+ # 添加均值±2个标准差范围的阴影带
   geom_line()+geom_point(size=1)+
   geom_hline(yintercept=mean(data5_1$AQI),
              color="grey50",linewidth=0.3)+    # 添加年均值线
   scale_x_date(expand=c(0,0),date_breaks="1 month",date_labels="%b")+# 设置x轴间隔为1个月
   facet_wrap(~季度,ncol=4,scale="free_x")+     # 按季度4分面
   guides(fill="none",color="none")+            # 删除图例
   labs(x=NULL,title="(a) 按季分面 (全年)")

# 图（b）按月分面（全年）
p2<-p1+aes(x=日期,y=AQI,color=月份)+
   facet_wrap(~月份,ncol=3,scale="free_x")+     # 按月份3列分面
   theme(strip.text.x=element_blank())+         # 删除分面标签
   labs(x="日期 (月)",title="(b) 按月分面 (全年)")

# 图（c）按周分面（1-3月份）
df3<-df1%>%subset(日期>="2025/1/1" & 日期<="2025/3/31") # 选出1-3月份的数据
p3<-ggplot(df3,aes(x=日期,y=AQI,color=week)) +
    geom_ribbon(aes(ymin=AQI-sd(AQI),ymax=AQI+sd(AQI),fill=week),
                alpha=0.3,linewidth=0.1) +
    geom_line()+geom_point(size=1)+
  scale_x_date(expand=c(0,0),date_breaks="1 week",date_labels="%W")+# 设置x轴间隔为1周
  facet_wrap(~week,ncol=3,scale="free_x")+
  theme(strip.text.x=element_blank(),
        legend.position="inside",legend.position.inside=c(0.68,0.06),
        legend.key.size=unit(0.2,"cm"), legend.key.height=unit(0.1,"cm"))+
  guides(fill=guide_legend(ncol=5))+
  labs(x="日期 (周)",y=NULL,title="(c) 按周分面 (1-3月份)")

(p1/(p2+p3+plot_layout(widths=c(1.3,1)))+  # 组合图形，p2和p3列宽为1.3:1
    plot_layout(heights=c(1,5)))           # 行高为1:5


#####————————————————————————————————#####
##### 【图8-4】的绘制代码——面积图
#####————————————————————————————————#####
# 图8-4的绘制代码（使用图8-1绘制的p1和p2）
library(ggplot2);library(reshape2);library(dplyr)
library(ggsci);library(patchwork)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(-c(AQI,质量等级))%>%        # 删除不需要的变量
   mutate(日期=as.Date(日期))%>%                 # 将日期设置为日期变量
   melt(id.vars="日期",variable.name="指标",value.name="指标值") # 融合数据

# 图（a）按指标分组
p<-ggplot(df,aes(x=日期,y=指标值,fill=指标))+    # 设置x轴、y轴和线的颜色
   geom_area(linewidth=0.5)+                     # 绘制折线图
   scale_fill_npg()                              # 使用Nature的配色
p1<-p+scale_x_date(expand=c(0,0),date_breaks="1 month",date_labels="%b")+# 设置x轴间隔为1个月
   theme(legend.position="inside",legend.justification="top",
         legend.key.height=unit(0.4,"cm"),
         legend.background=element_blank())+
   guides(fill=guide_legend(nrow=2,title=NULL))+ # 图例排成2行，去掉标题
   ggtitle("(a) 按指标分组")

# （b）按指标分面
p2<-p+guides(fill="none")+
   scale_x_date(expand=c(0,0),date_breaks="3 month",date_labels="%b")+# 设置x轴间隔为3个月
   facet_wrap(~指标,ncol=3,scale="free")+  # 按指标3列分面，自由设置坐标轴
   ggtitle("(b) 按指标分面")

p1/p2+plot_layout(heights=c(1,1.6))# 组合图形，行高为1:1.6


#####————————————————————————————————#####
##### 【图8-5】的绘制代码——堆叠面积图
#####————————————————————————————————#####
# 图8-5的绘制代码（数据：data5_1）
library(ggplot2)
library(reshape2)
library(dplyr)
library(patchwork)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%select(-c(AQI,质量等级))%>%
    mutate(日期=as.Date(日期))%>% 
    subset(日期>="2025/1/1" & 日期<="2025/1/31")%>%# 筛选出1月份的绘图数据
    melt(id.vars="日期",variable.name="指标",value.name="指标值")

# 图（a）堆叠面积图
p<-ggplot(df,aes(x=日期,y=指标值,fill=指标))+  # 按指标分组
  theme(legend.position="bottom",
        legend.key.height=unit(0.4,"cm"),
        legend.key.width=unit(0.5,"cm"))
p1<-p+geom_area()+                             # 绘制面积图
  geom_line(position="stack",color="grey")+    # 绘制线图
  scale_fill_brewer(palette="Blues")+          # 设置配色方案（蓝色）
  ggtitle("(a) 堆叠面积图")

# 图（b）百分比堆叠面积图
p2<-p+geom_area(position="fill",color="grey")+ # 绘制百分百堆叠面积图
  scale_fill_brewer(palette="Reds")+
  scale_y_continuous(labels=scales::percent)+  # y轴显示百分比标签
  ylab("百分比")+ggtitle("(b) 百分比堆叠面积图")

p1+p2             # 组合图形



#####================================================================#####
#####  8.2  探索多序列的变化模式
#####================================================================#####

#####————————————————————————————————#####
##### 【图8-6】的绘制代码——流线图
#####————————————————————————————————#####
# 图8-6的绘制代码（数据：data5_1）
library(ggplot2)
library(ggstream)
library(lubridate)
library(reshape2)
library(dplyr)
library(patchwork)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df1<-data5_1%>%select(日期,AQI,PM2.5)%>%
   mutate(日期=as.Date(日期))%>%    # 将日期转化成日期变量
   melt(id.vars="日期",variable.name="指标",value.name="指标值")

df2<-data5_1%>%select(-c(AQI,质量等级))%>%
   mutate(日期=as.Date(日期))%>% 
   melt(id.vars="日期",variable.name="指标",value.name="指标值")

# 绘制流线图
p1<-ggplot(df1,aes(x=日期,y=指标值,group=指标,fill=指标))+
    geom_stream(bw=0.7)+                  # 绘制流线图，设置带宽=0.7
    theme(legend.position="bottom",legend.key.height=unit(0.4,"cm"))+
    guides(fill=guide_legend(nrow=2))+    # 图例排成2行
    labs(y=NULL,title="(a) AQI和PM2.5")   # 去掉y轴标题

p2<-p1 %+% df2+ggtitle("(b) 6项空气污染指标")

p1+theme(plot.margin=unit(c(0,10,0,0),"pt"))+p2  # 组合图形（设置p1的边距）


#####————————————————————————————————#####
##### 【图8-7】的绘制代码——一个虚拟的时间序列的折线图和面积图
#####————————————————————————————————#####
# 图8-7的绘制代码（不放在书中）
library(ggplot2)
library(patchwork)

x = 1:365
y = round(x * sin(0.1 * x))
df<-data.frame(时间=x,观测值=y)

# 图（a）绘制折线图
p1<-ggplot(df,aes(x=时间,y=观测值))+           # 设置x轴、y轴
   geom_line(color="red")+                     # 绘制折线图
   geom_hline(yintercept=0,linewidth=0.3,color="steelblue")+
   ggtitle("(a) 折线图")+theme_bw()

# 图（b）面积图
df<-data.frame(时间=x,观测值=y)
df1<-approx(df$时间,df$观测值,n=2000) # 插值到1000
df2<-data.frame(时间=df1$x,观测值=df1$y)

df2$color[df2$观测值>=0]<-"pos"  # 设置正负颜色
df2$color[df2$观测值<0]<-"neg"

p2<-ggplot(df2,aes(x=时间,y=观测值),fill=color)+          # 设置x轴、y轴
   geom_area(aes(fill=color))+ # 绘制面积图
   geom_line()+
   geom_hline(yintercept=0,linewidth=0.3,color="steelblue")+
   theme_bw()+theme(legend.position="none")+
   ggtitle("(b) 面积图")

p1+p2


#####————————————————————————————————#####
##### 【8-8】的绘制代码——一个虚拟的时间序列的地平线图（latticeExtra包)
#####————————————————————————————————#####
# 图8-8的绘制代码
x = 1:365
y = x * sin(0.1 * x)
df<-data.frame(x,y)
library(latticeExtra)
horizonplot(ts(y),main="地平线图",
  colorkey=TRUE,                       # 显示图例
  par.settings=list(par.main.text=list(cex=1,font=1)))


#####————————————————————————————————#####
##### 【图8-9】的绘制代码——latticeExtra绘制的地平线图
#####————————————————————————————————#####
# 图8-9的绘制代码（data5_1）
library(latticeExtra);library(dplyr)

data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
dt<-data5_1%>%select(-c(日期,质量等级))%>%ts()  # 选择绘图数据并生成时间序列对象
horizonplot(dt,main="latticeExtra 包绘制的地平线图",
  layout=c(1,7),                              # 1列7行的页面布局
  colorkey=TRUE,                              # 显示色键（图例）
  par.settings=list(par.main.text=list(cex=1,font=1)))# 设置主标题字体大小


#####————————————————————————————————#####
##### 【图8-10】的绘制代码——绘制地平线图
#####————————————————————————————————#####
# 图8-10的绘制代码（data5_1）
library(ggplot2)
library(ggHoriPlot)
library(lubridate)
library(reshape2)
library(dplyr)
library(ggthemes)            # 为了使用theme_few主题
library(patchwork)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df1<-data5_1%>%select(-质量等级)%>%mutate(日期=as.Date(日期),
   季度=quarter(日期))%>%      # 添加季度因子
   filter(季度==1)%>%         # 选出1季度
   melt(id.vars=c("日期","季度"),variable.name="指标",value.name="指标值")
df2<-data5_1%>%select(-质量等级)%>%mutate(日期=as.Date(日期))%>%
   melt(id.vars="日期",variable.name="指标",value.name="指标值")

# 设置图形主题
mytheme<-theme_few()+
  theme(panel.spacing.y=unit(0,"lines"),          # 设置y轴间隔为0
     strip.text.y = element_text(angle=0),        # 设置y轴标签角度
     axis.text.y = element_blank(),               # 删除y轴标签
     axis.title.y = element_blank(),              # 删除y轴标题
     axis.ticks.y = element_blank(),              # 删除y轴刻度线
     panel.border = element_blank())              # 删除边线

# 图（a） 1季度数据的地平线图
p1<-ggplot(df1)+aes(x=日期,y=指标值,fill=指标)+
  geom_horizon(origin='min',horizonscale=10,show.legend=FALSE) + # 绘制地平线图,原点为最小值，地平线图的切割点为10，不显示图例
  scale_x_date(expand=c(0,1),date_breaks="1 month",date_labels="%b")+# 设置x轴间隔为1个月（向后扩展1期）
  facet_grid(指标~.)+                          # 按指标分面
  scale_fill_hcl(palette='RdYlBu',reverse=F)+  # 设置调色板（颜色不反转）
  mytheme+
  ggtitle("(a) 1季度数据的地平线图")

# 图（b） 全年数据的地平线图
p2<-p1 %+% df2+ggtitle("(b) 全年数据的地平线图")

p1/p2          # 组合图形



#####================================================================#####
#####  8.3  比较不同时间点的差异
#####================================================================#####

#####————————————————————————————————#####
##### 【图8-11】的绘制代码——瀑布图
#####————————————————————————————————#####
# 图8-11的绘制代码（data5_1）
## 安装包：devtools::install_github('Ather-Energy/ggTimeSeries')
library(ggplot2)
library(ggTimeSeries)
library(dplyr)
library(reshape2)
library(patchwork)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%
   select(c(日期,AQI,臭氧浓度))%>%
   mutate(日期=as.Date(日期))%>%
   subset(日期>="2025/1/1" & 日期<="2025/3/31")%>% # 选出1~3月份的数据
   melt(id.vars="日期",variable.name="指标",value.name="指标值")

# 绘制瀑布图
p1<-ggplot_waterfall(dtData=df,cXColumnName="日期",cYColumnName="指标值",
    nArrowSize=0.2)+                              # 设置箭头的大小
    scale_color_manual(values=c("blue2","red2","green2"))+# 数值线颜色
    facet_wrap(.~指标,scale="free")+
    theme(panel.background=element_rect(fill = "grey98"))+
    guides(color="none")+
    ggtitle("(a) 用带箭头的线和颜色表示变化")
    
p2<-ggplot(df,aes(x=日期,y=指标值))+
    stat_waterfall()+
    facet_wrap(.~指标,scale="free")+
    theme(panel.background=element_rect(fill="grey98"))+
    ggtitle("(b) 用条形和颜色表示变化")
   
p1/p2          # 组合图形


#####————————————————————————————————#####
##### 【图8-12】的绘制代码——斜线图
#####————————————————————————————————#####
# 图8-12的绘制代码（data3_3）
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggrepel)
library(patchwork)

# 处理数据
data3_3<-read.csv("C:/mydata/chap03/data3_3.csv",check.name=FALSE)
df1<-data3_3 %>%
  select(-区域划分)%>%
  filter(地带划分%in%c("中部地带","西部地带"))%>% # 选出中部地带和西部地带
  melt(id.vars=c("地区","地带划分"),variable.name="年份",value.name="地区生产总值")   # 融合数据为长格式

df2<-data3_3[,c(1,2,4,8)]%>%
  filter(地带划分%in%c("中部地带","西部地带"))%>% # 选出2019年和2023年的数据以及中部地带和西部地带
  melt(id.vars=c("地区","地带划分"),variable.name="年份",value.name="地区生产总值")

# 图（a）2019—2023年各年地区生产总值的变化
p1<-ggplot(df1,aes(x=年份,y=地区生产总值,color=地带划分))+ 
  geom_vline(xintercept="2019年",linewidth=0.5,linetype=1,color="grey")+# 绘制垂线
  geom_vline(xintercept="2020年",linewidth=0.5,linetype=1,color="grey")+ 
  geom_vline(xintercept="2021年",linewidth=0.5,linetype=1,color="grey")+ 
  geom_vline(xintercept="2022年",linewidth=0.5,linetype=1,color="grey")+ 
  geom_vline(xintercept="2023年",linewidth=0.5,linetype=1,color="grey")+ 
  geom_point(size=1.5)+  # 绘制点
  geom_line(aes(group=地区,color=地带划分),linetype=1,linewidth=0.5)+ # 绘制线
  geom_text_repel(data=df1%>%filter(年份=="2019年"),hjust=1.4,
                  size=2,aes(label=地区))+  # 绘制地区标签,避免重叠
  geom_text_repel(data=df1%>%filter(年份=="2023年"),hjust=-0.5,
                  size=2,aes(label=地区))+ 
  scale_color_brewer(palette="Set2")+   # 设置颜色
  theme(plot.title=element_text(size=11),
        panel.background=element_rect(fill = "grey98"))+
  ggtitle("(a) 2019—2023年地区生产总值的变化")

# 图（b）2019年和2023年地区生产总值的变化
p2<-p1%+%df2+
  ggtitle("(b) 2019年和2023年地区生产总值的变化")

p1+p2+plot_layout(guides="collect",axes="collect_y")&
    theme(legend.position="bottom")# 共享图例并删除重复的y轴


#####————————————————————————————————#####
##### 【图8-13】的绘制代码——凹凸图
#####————————————————————————————————#####
# 图8-13的绘制代码（data3_3）
library(ggplot2)
library(reshape2)
library(dplyr)
library(patchwork)
data3_3<-read.csv("C:/mydata/chap03/data3_3.csv",check.name=FALSE)

# 图（a）2019—2023年地区生产总值的排名变化
df1<-data3_3%>%select(-区域划分)%>%
  melt(id.vars=c("地区","地带划分"),variable.name="年份",value.name="地区生产总值")%>%   # 融合数据为长格式
  group_by(年份)%>%      # 按年份分组
  mutate(地区生产总值排名=rank(地区生产总值)) # 添加地区生产总值排名列
p1<-ggplot(df1,aes(x=年份,y=地区生产总值排名,color=地带划分))+ 
  geom_vline(xintercept="2019年",linewidth=0.5,linetype=1,color="grey")+# 绘制垂线
  geom_vline(xintercept="2023年",linewidth=0.5,linetype=1,color="grey")+ 
  geom_point(aes(group=地区),size=2)+  # 绘制点
  geom_line(aes(group=地区),linetype=1,linewidth=0.5,
     arrow=arrow(angle=15,length=unit(0.1,"inches")))+ # 绘制带箭头的线
  geom_text(data=df1%>%filter(年份=="2019年"),hjust=1.4,size=2,
            aes(label=地区))+  # 绘制地区标签
  geom_text(data=df1%>%filter(年份=="2023年"),hjust=-0.5,size=2,
            aes(label=地区))+ 
  scale_color_brewer(palette="Set1")+   # 设置颜色
  theme(plot.title=element_text(size=11),
        panel.background=element_rect(fill = "grey98"))+
  ggtitle("(a) 2019—2023年地区生产总值的排名变化")

# 图（b）2019年和2023年地区生产总值的排名变化
df2<-data3_3[,c(1,2,4,8)]%>%    # 选择绘图变量
  melt(id.vars=c("地区","地带划分"),variable.name="年份",
       value.name="地区生产总值")%>%
  group_by(年份)%>%mutate(地区生产总值排名=rank(地区生产总值))

p2<-p1%+%df2+ggtitle("(b) 2019年和2023年地区生产总值的排名变化")

p1+p2+plot_layout(guides="collect",axes="collect_y")&
    theme(legend.position="bottom")# 共享图例并删除重复的y轴



#####================================================================#####
#####  8.4  观察每一天的数值变动
#####================================================================#####

#####————————————————————————————————#####
##### 【图8-14】的绘制代码——日历图——AQI
#####————————————————————————————————#####
# 图8-14的绘制代码
library(openair)
library(lubridate)                 # 为使用函数year提取年份
library(dplyr)

# 处理数据
data8_1<-read.csv("C:/mydata/chap08/data8_1.csv")
df<-data8_1%>%select(日期,AQI)%>%        # 选择变量
  mutate(date=as.Date(日期),             # 将日期转化成日期变量
    year=year(date))%>%                  # 提取年份并添加到添加到数据框
    filter(year=="2025")                 # 筛选出2025年的数据

# 绘制日历图
## Sys.setlocale(locale="C")      # 修改计算机系统以合理显示x轴标签
calendarPlot(df,pollutant="AQI",cols="heat",year=2025,month=c(1:12))


#####————————————————————————————————#####
##### 【图8-15】的绘制代码——画出1-3月份AQI的日历图
#####————————————————————————————————#####
# 图8-15的绘制代码（使用图8-14构建的数据框df）
# 处理数据
data8_1<-read.csv("C:/mydata/chap08/data8_1.csv")
df<-data8_1%>%select(日期,AQI)%>%        # 选择变量
  mutate(date=as.Date(日期),             # 将日期转化成日期变量
    year=year(date))%>%                  # 提取年份并添加到添加数据框
    filter(year=="2025")                 # 筛选出2025年的数据

calendarPlot(selectByDate(df,month=c(1,2,3),year=2025),# 选择月份
   pollutant="AQI",
   key.position="bottom",                                # 设置图例位置
   breaks=c(0,50,100,150,200,300,450),                   # 设置分组向量
   labels=c("优","良","轻度污染","中度污染","重度污染","严重污染"), # 设置标签向量
   cols=c("green","yellow","orange","red","purple","maroon"))# 设置颜色向量


#####————————————————————————————————#####
##### 【图8-16】的绘制代码——多年份AQI的日历
#####————————————————————————————————#####
# 图8-16的绘制代码
library(ggTimeSeries)
library(lubridate)                 # 为使用函数year提取年份
library(RColorBrewer)

# 处理数据
data8_1<-read.csv("C:/mydata/chap08/data8_1.csv")
df<-data8_1%>%select(日期,AQI)%>%mutate(date=as.Date(日期), 
       year=year(date))                   # 添加月份列

# 绘制日历图
ggplot_calendar_heatmap(dtDateValue=df,
  cDateColumnName="date",                         # 设置日期的列名
  cValueColumnName="AQI",                         # 设置数据的列名
  vcGroupingColumnNames="year",                   # 设置分组的列名
  dayBorderSize=0.2,dayBorderColour="grey60",  # 设置每天的边界线大小和颜色
  monthBorderSize=0.5,monthBorderColour="white")+ # 设置月份间的边界线大小和颜色
  scale_fill_gradientn(colors=rev(brewer.pal(11,"Spectral")))+# 设置调色板
  theme(panel.spacing.y=unit(0.1,"lines"))+       # 设置y轴子图间距
  facet_grid(year~.)                              # 按年份分面



#####================================================================#####
#####  8.5  寻找序列的变化趋势
#####================================================================#####

#####————————————————————————————————#####
##### 【图8-17】的绘制代码——平滑随机成分
#####————————————————————————————————#####
# 图8-17的绘制代码（数据：data5_1）
library(ggplot2)
library(reshape2)
library(dplyr)
library(forecast)     # 为了使用ma(移动平均)函数
library(patchwork)

# 图（a）AQI??7日和30日移动平均折线图
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df1<-data5_1%>%select(日期,AQI)%>%    # 选择绘图变量
    mutate(日期=as.Date(日期),        # 修改日期变量
           ma7=ma(AQI,order=7,centre=TRUE),
           ma30=ma(AQI,order=30,centre=TRUE))%>%     # 添加移动平均列
    melt(id.vars="日期",variable.name="指标",value.name="指标值")# 融合数据
linewidth<-ifelse(df1$指标=="AQI",0.4,0.8)           # 设置线宽度

p1<-ggplot(df1,aes(x=日期,y=指标值,color=指标))+
  geom_line(linewidth=linewidth)+                    # 绘制折线图
  scale_color_manual(values=c("grey50","deepskyblue2","red"))+# 自定义颜色
  scale_x_date(expand=c(0,0),date_breaks="1 month",date_labels="%b")+
  theme(legend.position="inside",legend.justification ="top",
        legend.background=element_blank(), 
        panel.grid.minor=element_blank())+       # 去掉次网格线
  guides(color=guide_legend(nrow=1,title=NULL))+ # 图例排成1行，去掉标题
  ggtitle("(a) AQI的7日和30日移动平均")

# 图（b）6项指标的30日移动平均折线图
df2<-data5_1%>%select(-c(AQI,质量等级))%>%
    mutate(日期=as.Date(日期))%>%
    melt(id.vars="日期",variable.name="指标",value.name="指标值")%>%
    group_by(指标)%>%                           # 按指标分组
    mutate(ma=ma(指标值,order=30,centre=TRUE))  # 添加移动平均列

# 绘制图形
p2<-ggplot(df2,aes(x=日期,y=指标值,color=指标))+
  geom_line(linewidth=0.2)+                 # 绘制原始数据折线图
  geom_line(aes(y=ma),linewidth=0.8)+       # 绘制移动平均折线图
  scale_x_date(expand=c(0,0),date_breaks="1 month",date_labels="%b")+
  guides(color="none")+
  facet_wrap(~指标,ncol=3,scale="free")+     # 按指标分面，自由设置坐标轴
  theme_test()+theme(axis.text.x=element_text(size=7,angle=90,hjust=1,vjust=1))+# 设置x轴标签角度
  ggtitle("(b) 6项指标的30日移动平均")

p1/p2+plot_layout(heights=c(1,1.8))          # 组合图形，行高为1:1.8


#####————————————————————————————————#####
##### 【代码框8-18】——成分分解图
#####————————————————————————————————#####
# 图8-18的绘制代码
library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)
library(patchwork)
data8_2<-read.csv("C:/mydata/chap08/data8_2.csv",check.names=FALSE)

# 图（a）观测值图
df<-data8_2%>%
   melt(variable.name="年份",value.name="销售量")%>%  # 融合数据
   mutate(日期=seq(as.Date("2020-01-01"),by="months",length=72), # 添加日期列 
   月份=month(日期,label=TRUE,abbr=TRUE))               # 添加月份因子

p1<-ggplot(df,aes(x=日期,y=销售量))+
  geom_line(color="red2",linewidth=0.8)+
  geom_point(shape=21,fill="red2",color="grey50",size=1.5)+
  scale_x_date(date_breaks="1 year",date_labels="%Y")+ # 设置x轴间隔为1年
  ggtitle("(a) 观测值图")

# 图（b）按年折叠图
p2<-ggplot(df,aes(x=月份,y=销售量,color=年份,group=年份))+
  geom_line(linewidth=0.8)+
  geom_point(aes(fill=年份),shape=21,color="grey50",size=2)+
  ylim(100,330)+
  theme(legend.position="inside",legend.justification=c("left","top"),
        legend.background=element_blank())+
  guides(fill="none",color=guide_legend(nrow=3,title=NULL))+
  ggtitle("(b) 按年折叠图")

# 图（c）成分分解图
df.ts<-ts(df$销售量,start=2020,frequency=12) # 定义时间序列对象
compose<-decompose(df.ts,type="multiplicative") # 使用乘法模型分解序列成分
names(compose)                            # 显示分解名称（为构建数据框）

df3<-data.frame(趋势成分=compose$trend,
    季节成分=compose$seasonal,随机成分=compose$random)%>% # 构建数据框
    mutate(日期=seq(as.Date("2020-01-01"),by="months",length=72))%>%
    melt(id.vars="日期",variable.name="成分",value.name="数值")

p3<-ggplot(df3,aes(x=日期,y=数值,color=成分))+
  geom_line(linewidth=0.8)+
  geom_point(aes(fill=成分),shape=21,color="grey50",size=1.5)+
  scale_x_date(date_breaks="1 year",date_labels="%Y")+
  facet_grid(成分~.,scale="free")+                      # 按成分分面
  theme(legend.position="none")+
  ggtitle("(c) 成分分解图")

# 组合图形
((p1/p2+plot_layout(heights=c(1,1.5)))|p3)+  # p1和p2的行高为1:1.5
   plot_layout(widths=c(1.3,1))              # 列宽为1.3:1





#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####
#####  END
#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####



