

###==================================###
###  【第9章】组合式和交互式图形     ###
###==================================###


#####================================================================#####
#####  9.1  组合式图形
#####================================================================#####

#####—————————————————————————
##### 9.1.1  图形与表格的组合
#####—————————————————————————

#####————————————————————————————————#####
##### 【图9-1】的绘制代码——金字塔图+原始数据表格
#####————————————————————————————————#####
# 图9-1的绘制代码（数据：data9_1）
library(ggstats)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(dplyr)
library(patchwork)

data9_1<-read.csv("C:/mydata/chap09/data9_1.csv")
df<-data9_1%>%
  mutate(年龄组=factor(年龄组,ordered=TRUE,levels=年龄组))%>%# 设置因子顺序
  melt(id.vars="年龄组",variable.name="性别",value.name="人数")
  
p<-ggplot(df) +
  aes(y=年龄组,fill=性别,weight=人数) +
  geom_pyramid()+
  #geom_pyramid_text(size=2)+
  scale_x_continuous(labels=label_percent_abs(),# 设置x轴标签为百分比绝对值
  limits=symmetric_limits)+                       # x轴范围对称
  theme_classic()+theme(legend.position="inside",
        legend.position.inside=c(0.2,0.95),    # 设置图例位置
        legend.key.size=unit(0.5,"cm"),        # 设置图例大小
        legend.background=element_blank())+    # 移除图例整体边框
  guides(fill=guide_legend(nrow=1))+
  labs(x="百分比",title="金字塔图")

# 绘制表格
tab<-data9_1%>%
    mutate(年龄组=factor(年龄组,ordered=TRUE,levels=年龄组))%>%
    arrange(desc(年龄组))%>%
    ggtexttable(rows=NULL,theme=ttheme(base_size=7,"mBlueWhite"))%>%
                        # 删除表格行号、设置字体大小和主体色调（蓝白）
    tab_add_title(text="按年龄和性别分人口数",size=11,padding=unit(0.8,"line"))%>% 
                        # 添加标题
    tab_add_footnote(text="数据来源：中国统计年鉴,2024",size=7,face="italic") # 添加脚注

p+tab+plot_layout(widths=c(2,1))   # 组合图形



#####————————————————————————————————#####
##### 【图9-2】的绘制代码——嵌套条形图+发散条形图+描述统计量表格
#####————————————————————————————————#####
# 图9-2的绘制代码（数据：data3_2）
## 安装包：devtools::install_github("dxsbiocc/gground")
library(ggplot2);library(ggstats);library(ggpubr)
library(reshape2);library(dplyr);library(patchwork)
library(gground)      # 为使用其函数绘制圆角条形图
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")

# 图（a）嵌套条形图
data3_2$支出项目<-factor(data3_2$支出项目,ordered=TRUE,levels=data3_2$支出项目) # 设置类别顺序
labels=c("食品烟酒","衣着","居住","生活用品\n及服务","交通通信", 
                  "教育文\n化娱乐","医疗保健","其他用品\n及服务")
p1<-ggplot(data3_2)+
  geom_round_col(aes(x=天津,y=支出项目),fill="skyblue",width=0.8,
                 radius = grid::unit(1, "mm"))  + # 绘制天津的圆角条形图
  geom_round_col(aes(x=重庆,y=支出项目),fill="red2",width=0.4,
                 radius = grid::unit(1, "mm"))+   # 绘制重庆的圆角条形图
  geom_label(data=data.frame(y=8,x=c(7500,8800),label=c("天津","重庆")),
             aes(x=x,y=y,label=label,fill=label),size=3.5,color="white")+ # 添加标签
  scale_fill_manual(values=c("skyblue","red2"),guide="none")+ # 自定义颜色，删除图例
  scale_y_discrete(labels=labels)+                   # 设置y轴刻度标签
  theme_classic()+
  labs(x="支出金额(元)",y=NULL,title="(a) 嵌套条形图")

# 图（b）发散条形图
df<-data3_2%>%select(支出项目,北京,上海)%>%
    melt(id.vars="支出项目",variable.name="地区",value.name="支出金额")%>% 
    mutate(支出项目=factor(支出项目,ordered=TRUE,levels=data3_2$支出项目))  # 设置类别顺序 
p2<-ggplot(df)+
  aes(y=支出项目,fill=地区,weight=支出金额) +
  geom_diverging()+               # 绘制发散条形图
  #geom_diverging_text(size=1.5)+ # 添加数值标签
  scale_fill_manual(values=c("#f89f68","#4b84b3"))+ # 自定义颜色
  scale_x_continuous(labels=label_number_abs(),# 设置x轴标签为数值绝对值
           limits = symmetric_limits)+         # 设置x轴对称 
  scale_y_discrete(labels=labels,guide=guide_axis(position="right"))+ # 设置y轴在右侧
  theme_classic()+
  theme(legend.position="inside",
        legend.position.inside=c(0.22,0.93),    # 设置图例位置
        legend.key.height=unit(0.52,"cm"))+
  guides(fill=guide_legend(ncol=2,title=NULL))+  # 图例排成2列，去掉标题
  labs(x="支出金额 (元)",y=NULL,title="(b) 发散条形图")
  
# 绘制表格
tab<-data3_2%>%
  melt(id.vars="支出项目",variable.name="地区",value.name="支出金额")%>%
  group_by(地区)%>%summarise(总支出=sum(支出金额),
            平均支出=mean(支出金额),中位数=median(支出金额),
            标准差=sd(支出金额),最大值=max(支出金额),最小值=min(支出金额),
            极差=max(支出金额)-min(支出金额),
            变异系数=sd(支出金额)/mean(支出金额))%>% # 按地区分组计算统计量
  ggtexttable(rows=NULL,theme=ttheme(base_size=10,"mOrange"))

(p1+theme(plot.margin=unit(c(0,20,0,0), "pt"))+p2)/
   tab+plot_layout(heights=c(2,1))    # 组合图形，行高为2:1


#####————————————————————————————————#####
##### 【图9-3】的绘制代码——条形图、饼图+描述统计量表格
#####————————————————————————————————#####
# 图9-3的绘制代码（数据：data3_2）
library(ggplot2)
library(dplyr)
library(reshape2) 
library(gground)
library(ggpubr)
library(ggsci)

data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")

# 绘制条形图
df<-data3_2%>%
  mutate(支出项目=factor(支出项目,ordered=TRUE,levels=支出项目))%>% # 设置类别顺序
  melt(variable.name="地区",value.name="支出金额")

bar<-ggplot(df,aes(x=地区,y=支出金额,fill=支出项目))+
    geom_round_col(width=0.8,color="grey80",position="dodge")+ # 绘制圆角条形图
    scale_fill_npg()+                              # 设置调色板
    ylim(0,25000)+theme_test()+
    theme(legend.position="bottom",legend.key.height=unit(0.4,"cm"))+
    labs(x="支出项目",y="支出金额 (元)")

# 绘制环形图
 data3_2$支出项目<-factor(data3_2$支出项目,ordered=TRUE,levels=data3_2$支出项目)
p1<-ggplot(data3_2,aes(x=1,y=北京,fill=支出项目))+
   geom_col(width=0.2,position="stack",color="grey90")+   # 绘制堆叠条形图（宽度为0.2）
   scale_fill_npg()+                                   # 设置调色板
   coord_polar(theta="y")+                             # 转换成极坐标
   theme_void()+theme(legend.position="none")+
   xlim(0.7,1.1)   # 设置x轴范围
p2<-p1+aes(x=1,y=天津,fill=支出项目)
p3<-p1+aes(x=1,y=上海,fill=支出项目)
p4<-p1+aes(x=1,y=重庆,fill=支出项目)

# 绘制表格
tab<-data3_2%>%
     melt(variable.name="地区",value.name="支出金额")%>%
     group_by(地区)%>%summarise(总支出=sum(支出金额),平均支出=mean(支出金额))%>%
     ggtexttable(rows=NULL,theme=ttheme(base_size=7,"light"))

# 组合图形
bar+annotation_custom(grob=ggplotGrob(p1),
     xmin=0.5,xmax=1.5,ymin=17500,ymax=27000)+   # 嵌入北京的环形图
   annotation_custom(grob=ggplotGrob(p2),
     xmin=1.5,xmax=2.5,ymin=7500,ymax=17000)+    # 嵌入天津的环形图
   annotation_custom(grob=ggplotGrob(p3),
     xmin=2.5,xmax=3.5,ymin=17500,ymax=27000)+   # 嵌入上海的环形图
   annotation_custom(grob=ggplotGrob(p4),
     xmin=3.5,xmax=4.5,ymin=7500,ymax=17000)+    # 嵌入重庆的环形图
   annotation_custom(ggplotGrob(tab),xmin=3.4,ymin=16000)  # 嵌入表格


#####————————————————————————————————#####
##### 【图9-4】的绘制代码——核密度图等+描述统计量表格
#####————————————————————————————————#####
# 图9-4的绘制代码（数据：data5_1）
library(ggplot2);library(ggpubr);library(dplyr)
library(ggfittext)
library(patchwork)

# 处理数据
df<-read.csv("C:/mydata/chap05/data5_1.csv")
levels=c("优","良","轻度污染","中度污染","重度污染","严重污染")
df$质量等级<-factor(df[,3],ordered=TRUE,levels=levels)    # 设置类别顺序
cols=c("green","yellow","orange","red","purple","maroon") # 设置颜色向量

# 绘制核密度图形+描述统计量表格
tab<-df%>%
   group_by(质量等级)%>%
   summarise(mean=mean(AQI),
             cv=sd(AQI)/mean(AQI),
             skew=e1071::skewness(AQI))%>% # 按质量等级分组计算统计量
   select(-质量等级)%>%round(2)%>%         # 结果保留2位小数
   mutate(质量等级=levels,.before=1)%>%    # 在第1列前插入质量等级一列
  ggtexttable(row=NULL,                         # 绘制表格，去掉行号
  theme=ttheme(base_size=7,
  tbody.style=tbody_style(fill=cols,size=9)))   # 设置表体颜色和字体大小

density.p<-ggplot(df)+aes(x=AQI,fill=质量等级)+ # 绘制核密度图
  geom_density(color="gray50",alpha=0.5)+ylim(0,0.055)+
  scale_fill_manual(values=cols)+
  theme(axis.text.y=element_text(angle=90,hjust=0.6),
        legend.position="inside",legend.position.inside=c(0.74,0.31),
        legend.key.size=unit(0.4,"cm"), legend.key.height=unit(0.4,"cm"), 
        legend.background=element_blank())+
  guides(fill=guide_legend(ncol=3,title=NULL))+         # 删除图例标题
  annotation_custom(ggplotGrob(tab),xmin=210,ymin=0.015,ymax=0.063)+# 设置表格插入位置
  ggtitle("核密度图")+
  theme(plot.title=element_text(size=13,hjust=0.02,vjust=1.2,
        margin=margin(t=10,b=-30)))# 设置标题位置

# 绘制条形图+误差线
dfb<-df%>%
  group_by(质量等级)%>%
  summarise(均值=mean(AQI),标准差=sd(AQI))%>%
            arrange(desc(均值))%>%
            mutate(质量等级=factor(质量等级,ordered=TRUE,levels=质量等级))

bar<-ggplot(dfb,aes(x=质量等级,y=均值,fill=质量等级,
            label=质量等级,alpha=0.5))+
  geom_col(width=0.8,color="grey80")+
  geom_errorbar(aes(ymin=均值-标准差,ymax=均值+标准差),
                width=0.3,color="blue3",linewidth=0.8)+
  geom_point(size=1.5,shape=21,fill="blue3")+   # 添加均值点
  geom_bar_text(position="dodge",place="left",color="grey5",
                min.size=1,alpha=1)+ # 设置标签和位置
  scale_fill_manual(values=rev(cols))+
  guides(alpha="none",fill="none",y="none")+
  coord_flip()+labs(x=NULL,y="AQI的均值")+
  ggtitle("误差条形图")+
  theme(plot.title=element_text(size=13,hjust=0.95,vjust=1.2,
        margin=margin(t=10,b=-30)))

# 绘制Q-Q图
qq<-ggplot(df,aes(sample=AQI))+
  stat_qq(color="steelblue")+stat_qq_line(linewidth=1,color="red")+
  labs(x="theoretical",y="sample")+
  theme(axis.text.y=element_text(size=7,angle=90,hjust=0.5))+
  guides(y=guide_axis(check.overlap=TRUE))+  # 删除y轴刻度重叠标签
  ggtitle("Q-Q图")+
  theme(plot.title=element_text(size=13,hjust=0.05,vjust=1.2,
        margin=margin(t=10,b=-30)))

# 绘制面积图
dfa<-df%>%select(日期,AQI)%>%mutate(日期=as.Date(日期))
area<-ggplot(dfa,aes(x=日期,y=AQI))+
  geom_area(fill="steelblue3")+
  scale_x_date(expand=c(0,0),date_breaks="1 month",date_labels="%b")+
  theme(axis.text.y=element_text(size=7,angle=90,hjust=0.5))+
  ggtitle("面积图")+
  theme(plot.title=element_text(size=13,hjust=0.98,vjust=1.2,
        margin=margin(t=10,b=-30)))

# 组合图形
(density.p+bar+theme(plot.margin=unit(c(0,0,0,-20),"pt"))+ # 设置bar的边距
   plot_layout(widths=c(2,1)))/                 # density.p和bar宽度为2:1
   ((qq+area)+plot_layout(widths=c(1,3)))+      # qq和area宽度为1:3
   plot_layout(heights=c(2,1))                  # 行高为2:1


#####————————————————————————————————#####
##### 【图9-5】的绘制代码——组图+边际图+描述统计量表格
#####————————————————————————————————#####
# 图9-5的绘制代码（数据：data7_1）
library (ggplot2)
library(reshape2)
library(dplyr)
library(ggalt)
library(ggpubr)
library(gground)            # 为使用其函数绘制圆角条形图 
library(aplot)              # 为使用其函数组合图形

# 图（a）矩形热图
# 处理数据
data7_1<-read.csv("C:/mydata/chap07/data7_1.csv")
df1<-data7_1%>%
  mutate(地区=factor(地区,ordered=TRUE,levels=地区))%>%
  melt(id.vars=c("地区","地带划分","区域划分"),
       variable.name="支出项目",value.name="支出金额")

p1<-ggplot(df1)+aes(x=支出项目,y=地区,fill=支出金额)+
   geom_tile(color="grey",linetype=1,linewidth=0.2)+ # 设置格子边框颜色、线型和线宽
   scale_x_discrete(label=c("食品\n烟酒","衣着","居住","生活用\n品及服务",
       "交通\n通信", "教育文\n化娱乐","医疗\n保健","其他用\n品及服务"))+ # 设置x轴标签
   scale_fill_continuous(low="#FEE5D9",high="#EF3B2C")+  # 设置颜色
   theme_test()+
   theme(legend.position="none",axis.text.y=element_text(size=7))

# 图（b）条形图
df2<-data.frame(项目总支出=colSums(data7_1[,-c(1,2,3)]))%>% # 构建带有列合计的数据框
     mutate(支出项目=c("食品烟酒","衣着","居住","生活用品及服务",
           "交通通信","教育文化娱乐","医疗保健","其他用品及服务")) # 插入支出项目列
p2<-ggplot(df2,aes(x=支出项目,y=项目总支出))+
   geom_round_col(aes(fill=支出项目),color="grey",width=0.9)+
   geom_text(aes(label=项目总支出),position=position_stack(0.5),vjust=0.5,size=2,angle=0)+ # 垂直调整标签位置
   theme_classic()+
   theme(axis.title.x=element_blank(),                # 移除x轴标题
         axis.text.x=element_blank(),                 # 移除x轴刻标签
         axis.text.y=element_text(size=7),            # 设置y轴标签字体大小
         axis.ticks.x=element_blank())+               # 移除x轴刻度线
   guides(fill="none")

# 图（c）棒棒糖图
df3<-data7_1%>%
  mutate(地区总支出=rowSums(data7_1[,-c(1,2,3)]),   # 插入行合计
         地区=factor(地区,ordered=TRUE,levels=地区))

p3<-ggplot(df3)+aes(x=地区,y=地区总支出)+ 
  geom_lollipop(color="steelblue",point.size=2)+
  coord_flip()+
  guides(color="none")+                               # 删除color产生的图例
  scale_y_continuous(limits=c(0,55000),
     breaks=c(0,20000,40000))+# 设置y轴值域和刻度线位置
  theme_classic()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),                  # 移除x轴刻标签     
        axis.ticks.y=element_blank())                 # 移除x轴刻度线

# 图（d）绘制表格
tab<-data7_1%>%
    melt(id.vars=c("地区","地带划分","区域划分"),
                   variable.name="支出项目",value.name="支出金额")%>%
    group_by(地带划分)%>%summarise(总支出=sum(支出金额),
       平均支出=mean(支出金额),中位数=median(支出金额),
       标准差=sd(支出金额),
       极差=max(支出金额)-min(支出金额),
       变异系数=sd(支出金额)/mean(支出金额))%>% # 按地带划分分组计算统计量
    ggtexttable(rows=NULL,theme=ttheme(base_size=7,"mBlue"))

# 组合图形
p1%>%insert_top(p2,height=0.4)%>%   # 在顶部插入p2，高度为0.4
    insert_right(p3,width=0.3)%>%   # 在右侧插入p3，宽度为0.3
    insert_bottom(tab,height=0.3)   # 在底部插入tab，高度为0.3




#####——————————————————————————
##### 9.1.2  图形与文本的组合
#####——————————————————————————

#####————————————————————————————————#####
##### 【图9-6】的绘制代码——箱线图+小提琴图+变量注释
#####————————————————————————————————#####
# 图9-6的绘制代码（数据：data5_1）
library(ggplot2)
library(dplyr)
library(lubridate)
library(patchwork)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%mutate(日期=as.Date(日期),月份=factor(month(日期,label=TRUE,abbr=TRUE)))       

# 图（a）PM2.5的箱线图+注释文本
boxplot.p<-ggplot(df,aes(x=月份,y=PM2.5,fill=月份))+
  geom_boxplot(notch=TRUE,linewidth=0.5)+
  scale_fill_brewer(palette="Paired")+
  geom_hline(yintercept=mean(df$PM2.5)+3*sd(df$PM2.5),linetype=2,
             color="blue2",linewidth=0.5)+ # 添加PM2.5的均值＋3sd线
  geom_hline(yintercept=median(df$PM2.5)+3*mad(df$PM2.5),linetype=6,
             color="red2",linewidth=0.5)+ # 添加PM2.5的中位数＋3mad线
  annotate("text",x=7,y=315,label="mean ＋ 3SD:",size=4,color="blue3")+  # 添加注释文本
  annotate("text",x=7,y=260,label="median ＋ 3MAD:",size=4,color="red3")+  # 添加注释文本
  guides(fill="none")+ggtitle("(a) PM2.5的箱线图+变量注释")+
  labs(caption="  注释：\n 
● PM2.5指空气动力学当量直径小于2.5微米的颗粒物。\n
● 其在空气中的含量浓度越高代表空气污染越严重。\n
● 它对空气质量和人体健康等有重要影响。")+# 添加注释文本（默认右下角）
  theme(plot.caption=element_text(hjust=0,size=11,color="darkblue"))# 设置注释文本位置（左下角）、字体大小和颜色

# 图（b）PM10的小提琴图+注释文本
violin.p<-ggplot(df,aes(x=月份,y=PM10,fill=月份))+
  geom_violin(scale="width",linewidth=0.4,trim=FALSE)+
  geom_boxplot(size=0.3,width=0.2,fill="white")+  # 添加箱线图
  scale_fill_brewer(palette="Paired")+
  guides(fill="none")+ggtitle("(b) PM10的小提琴图+变量注释")+
  labs(caption="  注释：\n
○ PM10是可吸入颗粒物(inhalable-particles)的缩写。\n
○ 空气动力学当量直径≤10微米的颗粒物称为可吸入颗粒物。\n
○ 通常来自被风扬起的尘土。")+
  theme(plot.caption=element_text(hjust=0,size=11,color="darkblue"))

boxplot.p+violin.p # 组合图形


#####————————————————————————————————#####
##### 【图9-7】的绘制代码——帕累托图、瀑布图与注释文本的组合
#####————————————————————————————————#####
# 图9-7的绘制代码（数据：data3_2）
library(ggplot2);library(ggpubr);library(dplyr);library(RColorBrewer)
library(patchwork)
library(waterfalls) # 为使用waterfall函数绘制瀑布图

# 处理数据
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")
df<-data3_2%>%select(支出项目,北京)%>%    # 选择绘图数据
         rename(支出金额=北京)%>%         # 将北京命名为支出金额
  arrange(desc(支出金额))%>%              # 将支出金额降序排序
  mutate(累积百分比=round(cumsum(支出金额)/sum(支出金额)*100,1), # 添加累积百分比列
        支出项目=factor(支出项目,ordered=TRUE,levels=支出项目))

# 图（a）帕累托图
labels=c("居住","食品\n烟酒","交通\n通信","医疗\n保健",
      "教育文\n化娱乐","生活用品\n及服务","衣着","其他用品\n及服务")
mytheme<-theme(axis.text.y=element_text(angle=90,hjust=0.5,vjust=0.5),legend.position="none")
palette<-rev(brewer.pal(8,"Reds"))                      # 设置调色板

p<-ggplot(df)+aes(x=支出项目,y=支出金额)+                
  geom_col(width=0.8,color="grey50",fill=palette)+      # 绘制条形图
  scale_x_discrete(labels=labels)+                      # 设置x轴标签
  geom_text(aes(x=支出项目,y=支出金额,label=支出金额,vjust=-0.5),size=3,color="gray50")+
  mytheme+labs(x=NULL,y="支出金额(元)",title="(a) 帕累托图") 

p1<-p+geom_line(aes(x=as.numeric(支出项目),y=累积百分比*max(支出金额/100)))+# 绘制累积百分比曲线
  geom_point(aes(x=as.numeric(支出项目),y=累积百分比*max(支出金额/100)),
             size=2.5,shape=23,fill="white")+    # 绘制点
  geom_text(aes(label=累积百分比,x=支出项目,y=1*累积百分比*max(支出金额/100),
    hjust=0.6,vjust=-0.7),size=2.5,colour="blue3")+# 添加百分比数值标签
  scale_y_continuous(sec.axis=sec_axis(~./max(df$支出金额/100),name="百分比(%)"))+# 添加第2个y轴
  annotate("text",x=7,y=16000,label="累积百分比曲线",size=3.5)# 添加注释文本

# 绘制注释文本（text_p1）
text1<-paste( "\n","帕累托图(Pareto-plot)\n",
  "●以意大利经济学家V.Pareto的名字命名。\n",
  "●按数值降序绘制的条形图并添加累积数值百分比曲线。\n",
  "●可视为单变量条形图的变种。",sep="")# 写入文本
text_p1<-ggparagraph(text1,size=10,face="bold",color="darkred")

# 图（b）瀑布图
p2<-waterfall(.data=df,
   rect_text_labels=paste(df$支出金额),             # 设置矩形标签
   fill_colours=rev(brewer.pal(8,"GnBu")),          # 设置矩形填充颜色
calc_total=TRUE,total_rect_border="grey50",total_rect_color="lightskyblue",# 显示总和矩形并设置边框和填充颜色
   total_rect_text=paste("总支出","\n",sum(df$支出金额)), # 设置总和矩形的文本标签
   rect_width=1,rect_text_size=0.9,total_rect_text_color="black",
   rect_border="grey50",fill_by_sign=FALSE)+# 设置矩形宽度、边框颜色、文本标签大小
   scale_x_discrete(labels=c(labels,"Total"))+
   mytheme+xlab("支出项目")+ggtitle("(b) 瀑布图")

# 绘制注释文本（text_p2）
text2<-paste( "\n","瀑布图(waterfall-chart)\n",
  "●由麦肯锡顾问公司创建的一种图形，因形似瀑布流水而得名。\n",
  "●用于展示多个子类对总和的贡献，反映局部与整体的关系。\n",
  "●可视为单变量条形图的变种。",sep="")# 写入文本
text_p2<-ggparagraph(text2,size=10,face="bold",color="darkblue")

(p1+text_p1+plot_layout(heights=c(3,1))|  # 组合图形
   p2+text_p2+plot_layout(heights=c(3,1)))


#####————————————————————————————————#####
##### 【图9-8】的绘制代码——堆叠条形图与李克特量表注释文本的组合
#####————————————————————————————————#####
# 图9-8的绘制代码（数据：data9_2）
library(ggplot2)
library(ggstats)
library(ggpubr)
library(dplyr)
library(patchwork)
data9_2<-read.csv("C:/mydata/chap09/data9_2.csv")

# 图（a）全部学生
p1<-gglikert(data9_2,include=3:8,    # 选择第3~8列绘图（可根据需要选择绘图项目）
  labels_size=2,labels_accuracy=0.1,# 设置数值标签字体大小和精度（保留1位小数）
   totals_include_center=FALSE)+ # 默认将总计添加到图的每一侧（不含中间项）
   theme(legend.key.size=unit(0.4,"cm"))+
   ggtitle("(a) 全部学生")

# 图（b）按性别分组
p2<-gglikert(data9_2,include=3:8,labels_size=2,
   labels_accuracy=1,            # 取整数
   totals_include_center=TRUE,   # 每侧总计将加中间项比例的一半
   facet_rows=vars(性别))+                 # 按组分组
   scale_fill_brewer(palette="RdYlBu")+   # 自定义调色板
   theme(legend.key.size=unit(0.4,"cm"))+
   ggtitle("(b) 按性别分组")

# 绘制注释文本
text<-paste( "\n","李克特量表\n",
 "⭕李克特量表(Likert-scale)是一种最常用的评分加总式量表，同一构念的项目用加总方式计分。\n",
 "⭕它由美国社会心理学家李克特（R.A,Likert）于1932年在原有的总加量表基础上改进而成。\n",
 "⭕该量表由一组陈述组成，每一陈述有非常同意、同意、不一定、不同意、非常不同意五种回答，\n",
 "分别记为5，4，3，2，1。每个被调查者的态度总分就是他对各道题的回答所得分数的加总。\n",
 "⭕这一总分可说明他的态度强弱或他在这一量表上的不同状态。\n",
 "⭕李克特量表因容易设计在社会学和市场研究中有广泛应用。",sep="")# 写入文本
text_p<-ggparagraph(text,size=8,color="darkblue")

free(p1,side="l")/text_p+                     # p1与text_p左对齐
   theme(plot.margin=unit(c(0,20,0,0),"pt"))+ # 设置p1的右边距
   plot_layout(heights=c(4,1))|free(p2,side="l")



#####—————————————————————————
##### 9.1.3  嵌入局部放大子图
#####—————————————————————————
#####————————————————————————————————#####
##### 【图9-9】的绘制代码——创建并嵌入局部放大子图
#####————————————————————————————————#####
# 图9-9的绘制代码（数据：data5_1）
# 使用ggmagnify包的geom_magnify函数创建插入局部放大图形
library(ggplot2)
library(ggmagnify)
library(patchwork)

df<-read.csv("C:/mydata/chap05/data5_1.csv")

# 图（a）AQI与PM2.5的散点图
p1<-ggplot(data=df,aes(x=AQI,y=PM10))+
  geom_point(aes(fill=AQI),shape=21,size=2)+
  #stat_smooth(method=lm,color="red",fill="deepskyblue",linewidth=0.8)+
  scale_fill_distiller(palette="Spectral")+
  theme_test()+
  theme(legend.position="none")+
  ggtitle("(a) AQI与PM0的散点图")

# 插入局部放大图形
p11<-p1+geom_magnify(from=c(xmin=20,xmax=50,ymin=5,ymax=50),# 设置放大区域的坐标
       to=c(xmin=40,xmax=220,ymin=280,ymax=450), # 设置插入区域的坐标
       shape="rect",   # 设置子图形状为矩形（默认），可选椭圆（ellipse）和轮廓（outline）
       proj="facing",        # 设置投影线为正对(默认)
       colour="steelblue",   # 设置投影线颜色
       linewidth=0.5,        # 设置投影线宽度
       axes="xy")            # 插图中绘制坐标轴

# 图（b）AQI的折线图
p2<-ggplot(data=df,aes(x=1:365,y=AQI))+
  geom_line(linewidth=0.3)+
  theme_test()+
  theme(legend.position="none")+
  xlab("Time")+ggtitle("(b) AQI的折线图")

# 插入局部放大图形
p22<-p2+geom_magnify(from=c(xmin=110,xmax=140,ymin=47,ymax=190),# 设置放大区域的坐标
       to=c(xmin=90,xmax=260,ymin=280,ymax=430), # 设置插入区域的坐标
       shape="rect",   # 设置子图形状为矩形（默认），可选椭圆（ellipse）和轮廓（outline）
       proj="facing",        # 设置投影线为正对(默认)
       colour="steelblue",   # 设置投影线颜色
       linewidth=0.5,        # 设置投影线宽度
       axes="xy")            # 插图中绘制坐标轴

p11+p22         # 组合图形


#####————————————————————————————————#####
##### 【图9-10】的绘制代码——创建并组合局部放大子图
#####————————————————————————————————#####
# 图9-10的绘制代码（数据：data5_1）
library(ggplot2)
library(ggforce)
library(patchwork)

df<-read.csv("C:/mydata/chap05/data5_1.csv")

# 图（a）放大指定的数据范围
p1<-ggplot(data=df,aes(x=AQI,y=PM10))+
  geom_point(aes(fill=质量等级),shape=21,size=2)+  # 绘制按质量等级分组的散点图
  theme_test()+
  guides(fill="none")+
  facet_zoom(xlim=c(20,80),ylim=c(5,100),# 设置放大区域的数据范围
      horizontal=TRUE,       # 水平摆放缩放面板
      zoom.size=0.5,         # 缩放面板的大小为完整数据面板的0.5
      split=FALSE)+          # 不单独显示每个轴缩放
  ggtitle("(a1) 放大指定数据范围的散点图")

p2<-p1+facet_zoom(x=质量等级=="优",  # 放大空气质量为优时的散点图
      ylim=c(5,55),horizontal=FALSE,
      zoom.size=1,split=FALSE)+
      ggtitle("(a2) 放大质量等级为'优'的散点图")

# 图（b）AQI的折线图
p3<-ggplot(data=df,aes(x=1:365,y=AQI))+
  geom_line(linewidth=0.5,color="steelblue")+
  facet_zoom(xlim=c(110,140),ylim=c(47,190),
             horizontal=TRUE,zoom.size=0.5,split=FALSE)+
  theme_test()+xlab("Time")+ggtitle("(b1) 放大x轴和y轴数据范围的折线图")

p4<-p3+facet_zoom(xlim=c(110,140),zoom.size=0.5,split=FALSE)+
    ggtitle("(b2) 放大x轴数据范围的折线图")

(p1+p2)/(p3+p4)         # 组合图形


#####———————————————————————
##### 9.1.4  为图形添加图片
#####———————————————————————
#####————————————————————————————————#####
##### 【图9-11】的绘制代码——绘制带有背景图片的图形
#####————————————————————————————————#####
# 图9-11的绘制代码（数据：data5_1）
library(png)
library(ggplot2)
library(ggpubr)
library(patchwork)

data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
img<-readPNG("C:/mydata/chap09/001.png")              # 读入图片

# 绘制直方图
p1<-ggplot(data5_1,aes(x=AQI))+                       # 绘制图形对象
  ggtitle("(a) 直方图")+                              # 设置标题
  background_image(img)+                              # 添加背景图片
  geom_histogram(fill="deepskyblue",alpha=0.5,color="white")

# 绘制小提琴图
df<-reshape2::melt(data5_1[,c(1,2,4,8,5,9)],variable.name="指标",value.name="指标值") # 融合为长格式
img<-readPNG("C:/mydata/chap09/002.png")             # 读入图片
p2<-ggplot(df, aes(x=指标,y=指标值))+
  ggtitle("(b) 小提琴图")+
  background_image(img)+
  geom_violin(aes(fill=指标),scale="width",alpha=0.5,color="white")+
  fill_palette("jco")+guides(fill="none")

p1+p2                             # 组合图形


#####————————————————————————————————#####
##### 【图9-12】的绘制代码——填充图片
#####————————————————————————————————#####
# 图9-12的绘制代码（数据：data3_1）
library(ggplot2)
library(ggpattern)
library(patchwork)
df<-read.csv("C:/mydata/chap03/data3_1.csv")

# 提供填充图片地址（文件路径）
img <- c("C:/mydata/chap09/003.png",  
         "C:/mydata/chap09/004.png",
         "C:/mydata/chap09/005.png")

# 图(a) 普通条形图
p1<-ggplot(df, aes(x=满意度))+
  geom_bar_pattern(aes(pattern_filename=满意度),  # 设置图像文件名 
      pattern="image",                   # 设置填充图案
      pattern_type="tile",               # 设置填充类型
      fill="white",                      # 设置填充颜色
      colour="grey50",                   # 设置边线颜色
      pattern_scale=0.4)+                # 设置图像缩放
   scale_pattern_filename_discrete(choices = img)+
   labs(x="满意度",title = "(a) 普通条形图")+
   theme_test()+theme(legend.position='none') 
 
# 图(b)极坐标条形图
p2<-p1+coord_polar(theta="x")+
   labs(x="满意度",title = "(b) 极坐标条形图")

 p1+p2                    # 组合图形    



#####================================================================#####
#####  9.2  交互式图形  
#####================================================================#####

#####————————————————————————————————#####
##### 【图9-13】的绘制代码——plot_ly绘制的漏斗图
#####————————————————————————————————#####
# 图9-13的绘制代码（数据：data3_2）
library(plotly)
library(dplyr)
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")

# 处理数据
df1<-data3_2%>%
  select(支出项目,北京)%>%arrange(desc(北京))%>%  # 将支出金额降序排列
  mutate(支出项目=factor(支出项目,ordered=TRUE,levels=支出项目))# 设置类别顺序
df2<-data3_2%>%
  select(支出项目,上海)%>%
  arrange(desc(上海))%>%
  mutate(支出项目=factor(支出项目,ordered=TRUE,levels=支出项目))

# 图（a）北京
p1<-df1%>%plot_ly()%>%add_trace(
      type="funnel",                       # 设置绘图类型为漏斗图
      y=df1$支出项目,                      # 设置y轴
      x=df1$北京,                          # 设置x轴
      textinfo="value+percent total")      # 显示数值和百分比标签
  ## layout(title="2022年北京和上海居民人均消费支出的漏斗图")   # 添加标题

# 图（b）上海
p2<-df2%>%plot_ly()%>%add_trace(
      type="funnel",
      y=df2$支出项目,
      x=df2$上海,
      textinfo="value+percent total")

subplot(p1, p2)   # 组合图形


#####————————————————————————————————#####
##### 【图9-14】的绘制代码——例5-1的箱线图
#####————————————————————————————————#####
# 图9-14的绘制代码（数据：data5_1）
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(patchwork)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%mutate(日期=as.Date(日期),月份=factor(month(日期,label=TRUE,abbr=TRUE)))       

# 图（b）PM10的小提琴图
p2<-ggplot(df,aes(x=月份,y=PM10,fill=月份))+
  geom_violin(scale="width",linewidth=0.4,trim=FALSE)+
  geom_boxplot(size=0.3,width=0.2,fill="white")+  # 添加箱线图
  scale_fill_brewer(palette="Paired")+
  guides(fill="none")+ggtitle("(b) PM10的小提琴图")

ggplotly(p2)


#####————————————————————————————————#####
##### 【图9-15】的绘制代码——（图6-9（c）+图6-10（b））
#####————————————————————————————————#####
# 图6-9的绘制代码（图6-9（c）+图6-10（b））
library(ggplot2)
library(viridis)     # 色盲友好的颜色配色包
library(patchwork)

# 构建数据框
set.seed(1234)
n=5000
df<-data.frame(x=c(rnorm(n,10,5),rnorm(n,20,6),rnorm(n,30,5)),
               y=c(rnorm(n,20,5),rnorm(n,8,5),rnorm(n,30,5)))

# 图6-9（c）散点图+密度等高线
p<-ggplot(data=df,aes(x=x,y=y))+theme_test()
p3<-p+geom_point(color="grey20")+                 # 绘制散点图
    geom_density_2d()                             # 添加等高线

# 图6-10（b）bins=20
p<-ggplot(df,aes(x=x,y=y))+
   theme_test()+theme(legend.position="bottom")
p2<-p+geom_hex(bins=20,linewidth=0.3,color="black")+   # 六边形分箱
    scale_fill_viridis_c(option="H")                   # 选择配色方案

subplot(p3,p2)   # 组合图形


#####————————————————————————————————#####
##### 【图9-16】的绘制代码——时间序列交互图
#####————————————————————————————————#####
# 图9-16的绘制代码——时间序列交互图（数据：data5_1）
library(plotly)
library(ggplot2);library(reshape2);library(forecast);library(dplyr)
library(ggsci)

# 处理数据
data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
df<-data5_1%>%
  select(c(日期,AQI,PM2.5,PM10,二氧化硫,臭氧浓度))%>%  # 选择绘图变量
  mutate(日期=as.Date(日期))%>%              # 修改日期变量
  melt(id.vars="日期",variable.name="指标",value.name="指标值")%>%
  group_by(指标)%>%
  mutate(ma=ma(指标值,order=30,centre=TRUE)) # 按指标分组计算30日移动平均

# 绘制移动平均折线图
p<-ggplot(df,aes(x=日期,y=ma,color=指标))+       # 设置x轴、y轴和线的颜色
  geom_line(linewidth=0.5)+                      # 绘制折线图
  scale_color_npg()+                             # Nature的配色
  scale_x_date(expand=c(0,0),date_breaks="1 month",date_labels="%b")# x轴间隔为1个月
  
ggplotly(p)   # 绘制交互图


##### 创建可视化仪表盘
#####————————————————————————————————#####
##### 【图9-17和图9-18】的绘制代码——GWalkR的交互图
#####————————————————————————————————#####
# 图9-17和图9-18的绘制代码（数据：data5_1）
library(GWalkR)

data5_1<-read.csv("C:/mydata/chap05/data5_1.csv")
gwalkr(data5_1)









#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####
#####  END
#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####




