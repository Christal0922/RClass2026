
###================================================###
###  【第3章】比较数值大小和差异                   ###
###================================================###


#####================================================================#####
#####  3.1  用条形比较
#####================================================================#####

#####—————————————————————
##### 3.1.1  单变量条形图
#####—————————————————————
#####————————————————————————————————#####
##### 【图3-1】的绘制代码——单变量条形图
#####————————————————————————————————#####
# 图3-1的绘制代码
library(ggplot2)
library(patchwork)
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv") 

# 设置图形主题（可根据需设置或省略）
mytheme<-theme(plot.title=element_text(size="11"), # 设置主标题字体大小
   axis.title=element_text(size=10),           # 设置坐标轴标题字体大小
   axis.text=element_text(size=9),             # 设置坐标轴刻度标签字体大小
   legend.position="none")                     # 删除图例

# 图（a）性别的水平条形图
p1<-ggplot(data3_1,aes(x=性别,fill=性别))+ # 设置x轴和填充变量
  geom_bar(width=0.8)+          # 绘制条形图并设置条的宽度
  ylim(0,1250)+                 # 设置y轴范围
  coord_flip()+                 # 坐标轴互换（水平摆放条）
  mytheme+                      # 使用设置的主题          
  ylab("人数")+                 # 设置y轴标题
  ggtitle("(a) 水平条形图")     # 添加主标题（默认不绘制）

# 图（b）网购原因的垂直条形图
p2<-ggplot(data3_1,aes(x=网购原因,fill=网购原因))+
  geom_bar(width=0.8)+mytheme+ylab("人数")+
  ggtitle("(b) 垂直条形图")

# 图（c）满意度的垂直条形图
p3<-ggplot(data3_1,aes(x=满意度,fill=满意度))+geom_bar(width=0.8)+
  scale_fill_manual(values=c("red2","grey","grey"))+  # 自定义颜色
  scale_x_discrete(limits=c("不满意","中立","满意"))+ # 修改类别顺序
  mytheme+
  ylab("人数")+ggtitle("(c) 垂直条形图(修改类别顺序)")

p1+p2+p3                # 组合图形


#####————————————————————————————————#####
##### 【图3-2】的绘制代码——添加频数标签
#####————————————————————————————————#####
# 图3-2的绘制代码（以满意度为例）
library(ggplot2)
library(dplyr)            # 为了使用管道符%>%
library(patchwork)

# 处理数据
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv")
df<-data3_1%>%select(满意度)%>%table()%>%     # 选择绘图变量并生成频数表
  as.data.frame()%>%                          # 将频数表转化成数据框
  mutate(人数=Freq,百分比=人数/sum(人数)*100) # 修改Freq名称并添加百分比列

# 绘制条形图
palette<-RColorBrewer::brewer.pal(3,"Set2")         # 设置调色板
p1<-ggplot(df,aes(x=满意度,y=人数))+
  geom_bar(stat="identity",width=0.8,fill=palette)+ # 设置条宽和填充颜色
  geom_text(aes(label=人数,vjust=-0.5))+  # 垂直调整标签位置
  ylim(0,1.1*max(df$人数))+               # 设置y轴范围
  ggtitle("(a) 添加频数标签")

p2<-p1+geom_text(aes(label=paste(format(百分比,nsmall=1),"%")),vjust=1.5)+  # 添加百分比标签
  scale_x_discrete(limits=c("满意","中立","不满意"))+ # 修改类别顺序
  ggtitle("(b) 添加频数和频数百分比标签")

p1+p2+plot_layout(axes ="collect_y")  # 组合图形，删除重复的y轴


#####————————————————————————————————#####
##### 【图3-3】的绘制代码——图形分面
#####————————————————————————————————#####
# 图3-3的绘制代码
library(ggplot2)
library(tidytext)
library(dplyr)
library(reshape2)  
library(patchwork)
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")

# 图（a）按按地区分面、支出项目排序
df1<-data3_2%>%
  melt(variable.name="地区",value.name="支出金额")%>% # 融合成长格式
  mutate(支出项目=factor(支出项目,ordered=TRUE,levels=data3_2[,1]))
                                             # 设置类别顺序
p1<-ggplot(df1,aes(x=支出项目,y=支出金额,fill=地区))+
   geom_col(width=0.7,show.legend=FALSE)+    # 不显示图例
   coord_flip()+                             # 坐标轴互换
   facet_grid(.~地区)+                       # 按地区1行分面
   labs(x="支出项目",y="支出金额 (元)",title="(a) 按地区分面,按支出项目排序")

# 图（b）按地区分面、按支出金额排序
df2<-data3_2%>%melt(variable.name="地区",value.name="支出金额")%>%
   mutate(支出项目=reorder_within(支出项目,支出金额,地区))
                                          # 按支出金额对支出项目重新排序
p2<-ggplot(df2,aes(x=支出项目,y=支出金额,fill=地区))+ 
   geom_col(width=0.7,show.legend=FALSE)+ 
   coord_flip()+ 
   scale_x_reordered()+                       # 分面之前对x轴（列）重新排序
   facet_wrap(.~地区,ncol=2,scales="free")+   # 按2列分面，自由设置坐标轴
   labs(x="支出项目",y="支出金额 (元)",title="(b) 按地区分面,按支出金额排序") 

# 组合图形
p1/p2+plot_layout(heights=c(1,2))   # 组合图形,行高比为1:2



#####————————————————————
##### 3.1.2  多变量条形图
#####————————————————————
#####————————————————————————————————#####
##### 【图3-4】的绘制代码——双变量条形图——展示绝对值
#####————————————————————————————————#####
# 图3-4的绘制代码
library(ggplot2)
library(dplyr)
library(patchwork)

# 处理数据
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv")
df1<-data3_1%>%select(性别,满意度)%>%
table()%>%as.data.frame()%>%rename(人数=Freq)   # 生成列联表并转化成数据框
df2<-data3_1%>%select(网购原因,满意度)%>%
table()%>%as.data.frame()%>%rename(人数=Freq)

## 注：as.data.frame函数与count函数等价。即table()%>%as.data.frame()=count

# 图（a1）垂直并列条形图
p<-ggplot(df1,aes(x=满意度,y=人数,fill=性别))+
  geom_col(width=0.8,    # 设置条形宽度
  position="dodge",      # 绘制并列条形图
  color="gray50")+       # 设置条形图的边框颜色
  scale_fill_brewer(palette="Set2")     # 设置填充颜色

p1<-p+geom_text(aes(label=人数),position=position_dodge(0.8),
     vjust=-0.5,size=3)+          # 设置标签垂直位置和字体大小
  ylim(0,1.1*max(df1$人数))+      # 设置y轴范围
  ggtitle("(a1) 垂直并列")

# 图（a2） 水平并列条形图
p2<-p+
  geom_text(aes(label=人数),position=position_dodge(0.8),size=3,hjust=1.5)+
  coord_flip()+
  ggtitle("(a2) 水平并列")

# 图（b1） 垂直堆叠条形图
p3<-ggplot(df2,aes(x=满意度,y=人数,fill=网购原因))+
  geom_col(width=0.7,color="gray50")+ # 绘制堆叠条形图（默认）
  geom_text(aes(label=人数),position=position_stack(0.5),size=3)+
  scale_fill_brewer(palette="Set2")+
  ggtitle("(b1) 垂直堆叠")

# 图（b2） 水平堆叠条形图
p4<-p3+coord_flip()+ggtitle("(b2) 水平堆叠")

# 组合图形

# 组合图形
((p1+p2)+plot_layout(guides="collect"))/    # p1和p2共享图例（默认在右侧）
  ((p3+p4)+plot_layout(guides="collect"))   # p3和p4共享图例
  

#####————————————————————————————————#####
##### 【图3-5】的绘制代码——多变量条形图——x 轴交互分类条形图
#####————————————————————————————————#####
# 图3-5的绘制代码
library(ggplot2);library(dplyr);library(patchwork)

# 处理数据
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv")
df<-data3_1%>%ftable()%>%as.data.frame()%>%   # 生成频数表并转换成数据框
   rename(人数=Freq)                          # 将Freq重新命名为人数

# 图（a）x 轴交互分类的并列条形图
p1<-ggplot(df,aes(x=interaction(性别,满意度),y=人数,fill=网购原因))+
  geom_col(width=0.7,       # 设置条形间距
     position="dodge",      # 绘制并列条形图
     color="gray50")+       # 为条形图添加灰色边框
  geom_text(aes(label=人数),position=position_dodge(0.9),size=2.5,
  color="black",hjust=-0.2,vjust=0.5)+
                            # 设置标签字体大小、颜色和垂直位置调整
  scale_fill_brewer(palette="Set2")+      # 设置调色板
  ylim(0,1.1*max(df$人数))+               # 设置y轴范围
  coord_flip()+                           # 坐标轴互换
  ggtitle("(a) x 轴交互分类的并列条形图")

# 图（b） x 轴交互分类的堆叠条形图
p2<-ggplot(df,aes(x=interaction(性别,满意度),y=人数,fill=网购原因))+
  geom_col(width=0.7,position="stack",# 绘制堆叠条形图（默认）
   color="gray50")+
   geom_text(aes(label=人数),position=position_stack(0.5),
size=2.5,color="black")+  # 垂直调整标签位置
  scale_fill_brewer(palette="Set2")+
  coord_flip()+
  ggtitle("(b) x 轴交互分类的堆叠条形图")

((p1+p2)+plot_layout(guides="collect",axes ="collect_y"))
                               # 组合图形p1和p2并共享图例，删除重复的y轴


#####————————————————————————————————#####
##### 【图3-6】的绘制代码——不等宽条形图
#####————————————————————————————————#####
# 图3-6的绘制代码
library(ggiraphExtra)
require(ggplot2)
library(patchwork)
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv")

# 图（a）不等宽并列条形图
p1<-ggSpine(data=data3_1,aes(x=满意度,fill=网购原因),
  position="dodge",palette="Blues",labelsize=2.5)+    # 绘制并列条形图
     theme(legend.position="inside",                  # 设置图例在内部
       legend.position.inside=c(0.64,0.96),           # 设置内部图例位置
       legend.text=element_text(size="7"),            # 设置图例字体大小
       legend.background=element_blank(),             # 移除图例整体边框
       legend.key.height=unit(0.4,"cm"))+             # 设置图例键高度
  guides(fill=guide_legend(nrow=1,title=NULL))+ # 图例排成1行，移除图例标题
  labs(y="人数",title="(a) 不等宽并列条形图")        # 设置y轴标题和主标题

# 图（b）不等宽堆叠条形图
p2<-ggSpine(data=data3_1,aes(x=满意度,fill=网购原因),
 position="stack",palette="Reds",labelsize=3,reverse=TRUE)+# 绘制堆叠条形图
 ggtitle("(b) 不等宽堆叠条形图")

p1+p2             # 组合图形


#####————————————————————————————————#####
##### 【图3-7】的绘制代码——例3-1——普通百分比条形图
#####————————————————————————————————#####
# 图3-7的绘制代码
library(ggplot2)
library(ggstats)
library(dplyr)
library(patchwork)

# 处理数据
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv") 
df<-data3_1%>%table()%>%as.data.frame()%>% # 生成频数表并转换成数据框
       rename(人数=Freq)                   # 将Freq命名为人数

# 图（a）性别与满意度（占组内的百分比）
p1<-ggplot(df)+
   aes(x=满意度,fill=性别, 
       weight=人数,             # 使用人数变量作为权重   
       by=满意度)+              # 按满意度分组计算每个组内的百分比
   geom_prop_bar(position="fill") + 
                                # 绘制堆叠的百分比条形图(省略为并列条形图)
   geom_prop_text(stat="prop",  # 添加文本标签,使用 stat_prop函数计算百分比
   size=3,position=position_fill(0.5))+  # 设置标签字体大小和位置（中间）
   scale_fill_brewer(palette="Set2")+    # 设置调色板
   scale_y_continuous(labels=scales::percent)+
                               # 将y轴标签设置为百分比（默认为比例）
   labs(y="百分比",title="(a) 性别与满意度(组内百分比)")

# 图（b）性别与满意度（占总数的百分比）
p2<-p1+aes(by=1)+                     # 设置by=1计算占总数的百分比
   labs(title="(b) 性别与满意度(总数百分比)")

# 图（c）网购原因与满意度（占组内的百分比）
p3<-ggplot(df)+aes(x=满意度,fill=网购原因,weight=人数, by=满意度) +
   geom_prop_bar(position="fill")+    # 堆叠的百分比条形图
   geom_prop_text(stat="prop",size=3,position=position_fill(0.5))+
   scale_fill_brewer(palette="Set3")+ 
   scale_y_continuous(labels=scales::percent)+
   labs(y="百分比",title="(c) 网购原因与满意度(组内百分比)")

# 图（d）按性别分面（占总数百分比）
p4<-p3+aes(by=1)+facet_grid(cols=vars(性别))+  # 按性别（列变量）分面
  labs(y=NULL,title="(d) 按性别分面(总数百分比)")

# 组合图形
(p1+p2+plot_layout(axis_titles="collect_y"))/  # 删除重复的y轴标题 
  (free(p3,side="t")+p4)+                # p3与p4的顶部（top）对齐
  plot_layout(guides="collect")&theme(legend.position="bottom") # 共享图例（底部）


#####————————————————————————————————#####
##### 【图3-8】的绘制代码——例3-2——普通百分比条形图
#####————————————————————————————————#####
# 图3-8的绘制代码
library(ggplot2)
library(ggalluvial)
library(reshape2)
library(dplyr)
library(patchwork)
library(plyr)     # 为使用ddply函数
library(ggsci)    # 用于ggplot2的科学杂志和科幻主题调色板

# 处理数据
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")
df<-data3_2%>%
  mutate(支出项目=factor(支出项目,ordered=TRUE,levels=支出项目))%>%
                                                      # 设置类别顺序
  melt(id.vars="支出项目",variable.name="地区",value.name="支出金额")%>%
  ddply("地区",transform,percent=支出金额/sum(支出金额)*100)
                                                      # 计算各项支出百分比

# 图（a）普通百分比条形图
p1<-ggplot(df)+aes(x=地区,y=percent,fill=支出项目)+
  geom_bar(stat="identity",width=0.7,color="grey60")+
  scale_fill_npg()+                                  # 设置调色板
  labs(y="百分比(%)",title="(a) 普通百分比条形图")+  # 添加标签和标题
  theme_classic()+
  theme(legend.key.height=unit(0.5,"cm"),
        axis.line=element_line(arrow=arrow(length=unit(0.15,"cm")),linewidth=0.5))# 设置坐标轴箭头。设置type="closed"绘制封闭箭头

# 图（b）同类连线百分比条形图
p2<-p1+aes(x=地区,y=percent,fill=支出项目,
                  stratum=支出项目,alluvium=支出项目)+  # 建立映射关系
  geom_col(width=0.7,color="white",linewidth=0.5)+
  geom_flow(width=0.7,alpha=0.3,knot.pos=0,
        color="white",linewidth=2)+ # knot.pos控制连线的曲直，0为直线
  guides(fill="none")+labs(y="百分比(%)",title="(b) 同类连线百分比条形图")  # 添加标签和标题

# 组合图形
p1+p2+plot_layout(guides="collect",axes="collect_y")& # 组合图形并共享图例，删除重复的y轴
    theme(legend.position="bottom")


#####————————————————————————————————#####
##### 【图3-9】的绘制代码——不等宽百分比条形图
#####————————————————————————————————#####
# 图3-9的绘制代码
library("ggiraphExtra")
require(ggplot2)
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv")
ggSpine(data=data3_1,aes(x=满意度,fill=网购原因,facet=性别), # 按性别分面
  palette="Reds",labelsize=3,reverse=TRUE)               # 反转调色板颜色



#####—————————————————————————————
##### 3.1.3  极坐标条形图和玫瑰图
#####—————————————————————————————
#####————————————————————————————————#####
##### 【图3-10】的绘制代码——普通条形图到极坐标条形图的转换
#####————————————————————————————————#####
# 图3-10的绘制代码
library(ggplot2)
library(dplyr)
library(patchwork)

# 处理数据
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv")
df1<-data3_1%>%select(满意度)%>%table()%>%as.data.frame()%>%
  dplyr::rename(人数=Freq)                # 重新命名Freq
df2<-data3_1%>%select(性别,满意度)%>%table()%>%
  as.data.frame()%>%dplyr::rename(人数=Freq)

# 图（a）单变量条形图和极坐标条形图
p1<-ggplot(df1,aes(x=满意度,y=人数,fill=满意度))+
  geom_col(width=0.8,color="gray30")+  # 设置条形图的宽度
  scale_fill_brewer(palette="Reds")+   # 设置调色板
  guides(fill="none")+
  theme(plot.title=element_text(size=12))+# 设置主标题字体大小
  labs(y="人数",title="(a1) 单变量条形图")
p2<-p1+coord_polar()+ggtitle("(a2) 单变量极坐标条形图")  # 默认绘图起为0度（start=0）

# 图（b）双变量堆叠条形图和极坐标条形图
p3<-ggplot(df2,aes(x=满意度,y=人数,fill=性别))+
  geom_col(width=0.8,color="gray30")+
  scale_fill_brewer(palette="Blues")+
  theme(plot.title=element_text(size=12),
        legend.key.height=unit(0.5,"cm"))+
  labs(y="人数",title="(b1) 双变量堆叠条形图")
p4<-p3+coord_polar()+ggtitle("(b2) 双变量极坐标条形图")

(p1+p2)/(p3+p4+plot_layout(guides="collect"))&
    theme(legend.position="bottom")    # 组合图形，图p3和p4共享图例


#####————————————————————————————————#####
##### 【图3-11】的绘制代码——极坐标条形图（例3-2）
#####————————————————————————————————#####
# 图3-11的绘制代码
library(ggplot2)
library(dplyr)
library(patchwork)

# 处理数据
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")
df<-data3_2%>%select(支出项目,北京)%>%dplyr::rename(支出金额=北京)%>%
  arrange(支出金额)%>%   
                  # 选择数据并将北京重新命名为支出金额，按升序排序支出金额
  mutate(支出项目=factor(支出项目,ordered=TRUE,levels=支出项目))

# 图（a）将角度映射到变量x(theta="x")
p<-ggplot(df,aes(x=支出项目,y=支出金额,fill=支出项目))+
   geom_col(width=0.8)+                     # 绘制条形图
   scale_fill_brewer(palette="Spectral")    # 设置调色板

p1<-p+geom_text(aes(label=支出项目,y=15000),# 设置x轴标签及其位置
   size=2,angle=90, hjust=1)+         # 设置标签字体大小、角度和位置调整
   theme(legend.position="none",            # 删除图例
         axis.text.x=element_blank(),       # 删除x轴标签   
         axis.ticks.x=element_blank())+     # 删除x轴刻度线
   coord_radial(inner.radius=0.1,           # 转换成极坐标并设置内径
                rotate.angle=TRUE,expand=FALSE)+ # 旋转角度，不进行扩展
  ylim(0,20000)+                            # 设置y轴范围
  ggtitle("(a) 角度映射到变量 x (theta='x')")

# 图（b）将角度映射到变量y(theta="y")
p2<-p+theme(legend.position="none",         # 删除图例
      axis.text.y=element_blank(),          # 删除x轴标签   
      axis.ticks.y=element_blank())+        # 删除x轴刻度线
coord_radial(theta="y",start=-0.25,expand=TRUE)+
geom_text(aes(x=支出项目,y=0,label=支出项目),hjust=1,size=2.5)+
                                            # 添加文本标签
  ggtitle("(b) 角度映射到变量 y (theta='y')")

p1+p2         # 组合图形


#####————————————————————————————————#####
##### 【图3-12】的绘制代码——极坐标条形图
#####————————————————————————————————#####
# 图3-12的绘制代码
library(ggplot2)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(patchwork)

# 处理数据
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")
df<-data3_2%>%
  melt(id.vars="支出项目",variable.name="地区",value.name="支出金额")%>% 
                                                          # 融合数据
  mutate(支出项目=factor(支出项目,ordered=TRUE,levels=data3_2[,1])) 
                                                          # 设置类别顺序

# 图（a）极坐标堆叠条形图，x轴为支出项目 
p1<-ggplot(df,aes(x=reorder(支出项目,支出金额),y=支出金额,fill=地区))+
                                                # 按支出金额对支出项目排序
  geom_col(position="stack",width=0.6,color="gray50")+
  coord_radial(theta="x",start=0,inner.radius=0.05)+
  scale_fill_brewer(palette="Reds")+
  theme(legend.key.size=unit(0.4,"cm"),legend.key.height=unit(0.4,"cm"),
        axis.text.x=element_text(angle=seq(-20,-340,length.out=8),size=8))+# 设置标签角度
  labs(x="支出项目",title="(a) 极坐标堆叠条形图",subtitle="x 轴为支出项目") 

# 图（b）极坐标并列条形图，x轴为地区
p2<-ggplot(df,aes(x=地区,y=支出金额,fill=支出项目))+
  geom_col(width=0.8,position="dodge",color="gray50")+
  coord_radial(theta="x",start=0,inner.radius=0.05)+
  scale_fill_brewer(palette="Blues")+            # 设置调色板
  theme(legend.key.size=unit(0.4,"cm"),legend.key.height=unit(0.4,"cm"),
        axis.text.x=element_text(angle=seq(-40,-320,length.out=4),size=8))+# 设置标签角度
  labs(x="地区",title="(b) 极坐标并列条形图",subtitle="x 轴为地区")

p1+p2+plot_layout(guides="collect") # 组合图形并共享图例


#####————————————————————————————————#####
##### 【图3-13】的绘制代码——极坐标条形图——theta="y"
#####————————————————————————————————#####
# 图3-13的绘制代码
library(ggplot2)
library(dplyr)
library(reshape2)
library(patchwork)

# 处理数据
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")
df<-data3_2%>%
  melt(id.vars="支出项目",variable.name="地区",value.name="支出金额")%>%
  mutate(支出项目=factor(支出项目,ordered=TRUE,levels=data3_2[,1]))
                                                         # 设置类别顺序
mytheme<-theme(axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12),
        legend.background=element_blank(),
        legend.key.height=unit(0.4,"cm"),
        legend.key.size=unit(0.4,"cm"))

# 图（a）x轴为支出项目的极坐标堆叠条形图(theta='y')
p1<-ggplot(df,aes(x=reorder(支出项目,支出金额),y=支出金额,fill=地区))+
  geom_col(position="stack",width=0.7,color="gray40")+
  coord_radial(theta="y",start=-0.25)+
  scale_fill_brewer(palette="Reds")+           # 设置调色板
  geom_text(data=df,aes(x=支出项目,y=0,label=支出项目),
            hjust=1.05,size=2,show.legend=FALSE)+
  mytheme+
  labs(x=NULL,title="(a) x轴为支出项目(theta=y)")

# 图（b）x轴为地区的极坐标堆叠条形图(theta='y')
p2<-ggplot(df,aes(x=reorder(地区,支出金额),y=支出金额,fill=支出项目))+
  geom_col(position="stack",width=0.7,color="gray40")+
  coord_radial(theta="y",start=-0.25)+
  scale_fill_brewer(palette="Blues")+
  geom_text(data=df,aes(x=地区,y=0,label=地区),
            hjust=1.2,size=2,show.legend=FALSE)+
  mytheme+
  labs(x=NULL,title="(b) x轴为地区(theta=y)")

p1+p2+plot_layout(guides="collect") # 组合图形并共享图例


#####————————————————————————————————#####
##### 【图3-14】的绘制代码——南丁格尔玫瑰图
#####————————————————————————————————#####
# 图3-14的绘制代码（北京的南丁格尔玫瑰图）
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(patchwork)

# 处理数据
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")
df1<-data3_2%>%select(支出项目,北京)%>%
  dplyr::rename(支出金额=北京)%>% # 选择数据并将北京重新命名为支出金额
  mutate(支出项目=factor(支出项目,ordered=TRUE,levels=支出项目))
df2<-data3_2%>%
  select(支出项目,北京)%>%
  dplyr::rename(支出金额=北京)%>%
  arrange(desc(支出金额))%>%                  # 按降序排序支出金额
  mutate(支出项目=factor(支出项目,ordered=TRUE,levels=支出项目))

# 图（a）按支出项目原始顺序排序
palette1<-brewer.pal(8,"Set3")               # 设置离散型调色板
p1<-ggplot(df1,aes(x=支出项目,y=支出金额,fill=支出项目))+           
  geom_col(width=1,colour="grey20",fill=palette1)+# 绘制条形图
  ylim(0,20000)+
  coord_radial(theta="x",start=0,expand=FALSE)+   # 转化成极坐标图
  theme(axis.text.x=element_text(size=8,angle=seq(-20,-340,length.out=8)))+
  ggtitle("(a) 按支出项目原始顺序排序")

# 图（b）按支出金额降序排序
palette2<-brewer.pal(8,"Spectral")
p2<-p1%+%df2+
  geom_col(width=1,colour="grey20",fill=palette2)+
  ggtitle("(b) 按支出金额降序排序")

p1+p2    # 组合图形


#####————————————————————————————————#####
##### 【图3-15】的绘制代码——北京、天津、上海、重庆的南丁格尔玫瑰图
#####————————————————————————————————#####
# 图3-15的绘制代码
library(ggplot2);library(reshape2);library(dplyr)
library(patchwork)

# 处理数据
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")
df<-data3_2%>%
  melt(id.vars="支出项目",variable.name="地区",value.name="支出金额")%>%
  mutate(支出项目=factor(支出项目,ordered=TRUE,levels=data3_2$支出项目))

# 图（a）x轴为地区
p1<-ggplot(df,aes(x=地区,y=支出金额,fill=支出项目))+
   geom_col(position="stack",width=1,color="gray30")+
   coord_radial(theta="x",start=0,expand=FALSE)+
   scale_fill_brewer(palette="Blues")+
  theme(legend.position="bottom",
        legend.key.size=unit(0.5,"cm"),
        legend.key.height=unit(0.4,"cm"),
        axis.text.x=element_text(size=9,angle=seq(-40,-320,length.out=4)))+
   guides(fill=guide_legend(nrow=2,title=NULL))+ggtitle("(a) x 轴为地区")

# 图（b）x轴为支出项目
p2<-ggplot(df,aes(x=支出项目,y=支出金额,fill=地区))+
  geom_col(position="stack",width=1,color="gray30")+
  coord_radial(theta="x",start=0,expand=FALSE)+
  scale_fill_brewer(palette="Reds")+  # 设置调色板
  theme(legend.position="bottom",legend.key.size=unit(0.5,"cm"),
        legend.key.height=unit(0.4,"cm"),
        axis.text.x=element_text(size=9,angle=seq(-20,-340,length.out=8)))+
  guides(fill=guide_legend(nrow=2,title=NULL))+
  ggtitle("(b) x 轴为支出项目")

p1+theme(plot.margin=unit(c(0,20,0,0), "pt"))+p2   # 设置p1的边距



#####================================================================#####
#####  3.2  用矩形比较
#####================================================================#####

#####————————————————————————————————#####
##### 【图3-16】的绘制代码——马赛克图
#####————————————————————————————————#####
# 图3-16的绘制代码
library(ggplot2)
library(ggmosaic)
library(ggsci)
library(patchwork)
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv") 

mytheme<-theme_test()+theme(legend.position="none",
axis.text.x=element_text(size=7,angle=30,hjust=0.5,vjust=0.5),# 设置x轴标签
axis.text.y=element_text(size=7,angle=90,hjust=0.5,vjust=0.5))# 设置y轴标签

# 图（a）性别为alpha(透明度区分)
p1<-ggplot(data=data3_1) +
  geom_mosaic(aes(x=product(网购原因,满意度), # 设置绘图变量
  fill=满意度,alpha=性别))+      
  scale_alpha_manual(values=c(0.4,0.8))+      # 自定义颜色
  mytheme+
  ggtitle("(a) 性别为alpha")

# 图（b）性别为条件变量
p2<-ggplot(data=data3_1)+
  geom_mosaic(aes(x=product(网购原因), 
  fill=满意度,conds= product(性别)))+ 
  scale_fill_npg()+                           # 设置调色板
  mytheme+
  ggtitle("(b) 性别为条件变量")

# 图（c）按性别分面
p3<-p2+facet_grid(性别~.)+                    # 按性别分面
  mytheme+ggtitle("(c) 按性别分面")

(p1/p2)|p3     # 组合图形


#####————————————————————————————————#####
##### 【图3-17】的绘制代码——矩形树状图
#####————————————————————————————————#####
# 图3-17的绘制代码
library(ggplot2)
library(treemapify)
library(dplyr)
library(reshape2)
library(patchwork)

data3_2<-read.csv("C:/mydata/chap03/data3_2.csv") 
df1<-melt(data3_2,variable.name="地区",value.name="支出金额")# 融合数据

# 图（a）4个地区的8项支出(亚组：地区)
p1<-ggplot(df1,aes(area=支出金额,fill=地区,    # 设置区域变量和填充变量
           subgroup=地区,label=支出项目)) +    # 设置亚组和标签
 geom_treemap(color="white")+                  # 设置区域分割线颜色
 geom_treemap_subgroup_border(color="white",size=5)+
                                               # 设置亚组边线颜色和线宽
 geom_treemap_text(place="middle",min.size=2)+ # 设置标签位置和最小字体大小
 scale_fill_brewer(palette="Set2")+            # 设置调色板
 theme(legend.position="bottom")+              # 设置图例位置
 ggtitle("(a) 4个地区的8项支出","(亚组：地区)")# 设置标题

# 图（b）消费者的网购情况调查(亚组：满意度和性别)
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv")
df2<-data3_1%>%table()%>%as.data.frame()%>%    # 生成频数表并转化成数据框
    rename(人数=Freq)   # 修改Freq名称
p2<-ggplot(df2,aes(area=人数,fill=满意度,
          subgroup=满意度,subgroup2=性别,label=网购原因))+ # 设置亚组和标签
  geom_treemap(color="white")+                        # 设置区域分割线颜色
  geom_treemap_subgroup_border(color="white",size=6)+ # 设置亚组边线颜色和线宽
  geom_treemap_subgroup2_border(color="white",size=3)+ # 设置亚组2边线颜色和线宽
  geom_treemap_text(place="middle",size=12)+   # 设置标签位置和字体大小
  geom_treemap_subgroup2_text(place="topleft",fontface="bold.italic",
            size=12,color="white")+# 设置亚组2标签位置、字体、大小和颜色
  scale_fill_brewer(palette="Set2")+                     
  theme(legend.position="bottom")+
  ggtitle("(b) 消费者的网购情况调查","(亚组：满意度和性别)")

p1+p2            # 组合图形



#####================================================================#####
#####  3.3  用圆或点比较
#####================================================================#####

#####————————————————————————————————#####
##### 【图3-18】的绘制代码——圆堆图
#####————————————————————————————————#####
# 图3-18的绘制代码
## 安装包：remotes::install_github("EvaMaeRey/ggcirclepack") # 安装R包
library(ggcirclepack)
library(ggplot2)
library(dplyr)
library(patchwork)

# 数据处理
data3_3<-read.csv("C:/mydata/chap03/data3_3.csv",check.name=FALSE)
df<-data3_3[,c(1,2,3,8)]                           # 选择绘图变量
colnames(df)[4]<-"GDP2023"                         # 重新命名第4列

p1<-ggplot(df)+
  aes(id=地区,area=GDP2023,fill=地区)+ # 设置id变量、填充面积变量和填充变量
  geom_circlepack(show.legend=FALSE,color="grey")+ # 设置边线颜色
  #scale_fill_manual(values=rainbow(31))+          # 自定义填充颜色
  geom_circlepack_text(show.legend=FALSE)+         # 绘制标签，不显示图例
  coord_equal()+                        # 固定坐标轴比例（相等，即1??1）
  labs(title="(a) 各地区的地区生产总值")

p2<-ggplot(df)+aes(id=区域划分,fill=区域划分)+     # 设置填充变量
  geom_circlepack(show.legend=F,color="grey")+
  geom_circlepack_text(show.legend=F,size=5)+      # 设置标签字体大小
  coord_equal()+
  labs(title="(b) 各区域的地区生产总值")  

p3<-p1+aes(fill=地带划分)+
  facet_grid(rows=vars(地带划分)) +      # 按地带划分分面
  labs(title="(c) 按地带划分分面")

(p1/p2)|free(p3,side="l")               # 组合图形，p3左对齐


#####————————————————————————————————#####
##### 【图3-19】的绘制代码—气泡图
#####————————————————————————————————#####
# 图3-19的绘制代码
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(patchwork)

# 处理数据
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv")
df<-data3_1%>%ftable()%>%as.data.frame()%>% # 生成列联表并转化成数据框
    dplyr::rename(人数=Freq)                # 重新命名Freq

# 绘制点图
palette<-rev(brewer.pal(11,"Spectral"))    # 设置调色板
p1<-ggplot(df,aes(x=满意度,y=网购原因,fill=人数))+
   geom_point(aes(size=人数),shape=21)+
   scale_size(range=c(1,10),breaks=c(50,85,115,145,175))+ # 设置点的大小范围和分割点
   scale_fill_gradientn(colors=palette)+
   labs(x=NULL,y=NULL,title="(a) 满意度和网购原因")

p2<-p1+facet_grid(性别~.)+                 # 按性别分面
   labs(title="(b) 按性别分面")

# 组合图形并共享图例
p1+p2+plot_layout(widths=c(1.2,1))+       # 设置宽度为1.2：1
plot_layout(guides="collect")&theme(legend.position="bottom")


#####————————————————————————————————#####
##### 【图3-20】的绘制代码——例3-3（数据：data3_2）
#####————————————————————————————————#####
# 图3-20的绘制代码
library(ggplot2)
library(reshape2)
library(dplyr)
library(RColorBrewer)
library(patchwork)

# 处理数据
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")
df<-data3_2%>%
  mutate(支出项目=factor(支出项目,ordered=TRUE,levels=支出项目))%>%
  melt(variable.name="地区",value.name="支出金额")
labs=c("食品\n烟酒","衣着","居住","生活用\n品及服务",
     "交通\n通信", "教育文\n化娱乐","医疗\n保健","其他用\n品及服务") # 设置标签

# 绘制点图
palette<-rev(brewer.pal(11,"Spectral"))    # 设置调色板
p1<-ggplot(df,aes(x=支出项目,y=地区,size=支出金额,fill=支出金额))+
    geom_point(shape=21)+                  # 绘制点
    scale_size(range=c(1,10),breaks=c(4000,10000,16000))+ # 设置点的大小范围和分割点
    scale_fill_gradientn(colors=palette)+
    scale_x_discrete(labels=labs)+         # 设置x轴标签
    ggstats::geom_stripped_cols()+         # 添加列的交替背景色
    theme_test()+
    theme(legend.key.size=unit(0.5,"cm"),  # 设置图例大小
    legend.key.height=unit(0.5,"cm"),
    legend.key.width=unit(1,"cm"))+        # 设置键宽度
    labs(x=NULL,y=NULL,title="(a) 直角坐标")

p2<-p1+coord_polar()+
    scale_y_discrete(expand=c(0.08,0.3))+ # 设置y轴扩展范围
    labs(title="(b) 极坐标")

# 组合图形并共享图例
p1+p2+plot_layout(guides="collect")&theme(legend.position="bottom")



#####================================================================#####
#####  3.4  用颜色饱和度比较
#####================================================================#####

#####————————————————————————————————#####
##### 【图3-21】的绘制代码—热图
#####————————————————————————————————#####
# 3-21的绘制代码
library(ggplot2)
library(dplyr)
library(patchwork)

# 处理数据
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv")
df1<-data3_1%>%select(网购原因,满意度)%>%ftable()%>%
  as.data.frame()%>%dplyr::rename("频数"=Freq)
df2<-data3_1%>%ftable()%>%as.data.frame()%>%dplyr::rename("频数"=Freq)

# 绘制图形p1、p2、p3
p1<-ggplot(df1)+aes(x=满意度,y=网购原因,fill=频数)+
  geom_tile(color="grey50",linetype=1,linewidth=0.3)+# 设置格子边框颜色、线型和线宽
  geom_text(aes(label=频数),size=2.5,color="black")+ # 设置标签字体大小和颜色
  scale_fill_continuous(low="#FEE5D9",high="#EF3B2C")+ # 设置颜色
  coord_fixed()+                                    # 设置x轴与y轴比例相等
  ggtitle("(a) 矩形热图")
p2<-p1+coord_polar()+labs(y=NULL,title="(b) 极坐标热图")# 转换成极坐标热图

p3<-ggplot(df2)+aes(x=满意度,y=网购原因,fill=频数)+
  geom_tile(color="grey50",linetype=1,linewidth=0.3)+
  geom_text(aes(label=频数),size=2.5,color="black")+
  scale_fill_continuous(low="#EFF3FF",high="#2171B5")+
  coord_polar()+
  facet_grid(.~性别)+      # 按性别分面
  ggtitle("(c) 按性别分面")

# 组合图形
(p1+p2+plot_layout(guides="collect"))/p3    # 组合图形,p1和p2共享图例


#####————————————————————————————————#####
##### 【图3-22】的绘制代码——热图（数据：data3_2）
#####————————————————————————————————#####
# 图3-22的绘制代码
library(ggplot2)
library(reshape2)
library(dplyr)
library(patchwork)

# 处理数据
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")
df<-data3_2%>%
  mutate(支出项目=factor(支出项目,ordered=TRUE,levels=支出项目))%>%
  melt(id.vars="支出项目",variable.name="地区",value.name="支出金额")
labs=c("食品\n烟酒","衣着","居住","生活用\n品及服务",
     "交通\n通信", "教育文\n化娱乐","医疗\n保健","其他用\n品及服务") # 设置标签

# 图（a）x轴为地区的矩形热图
p1<-ggplot(df)+aes(x=地区,y=支出项目,fill=支出金额)+
  geom_tile(color="grey50",linetype=1,linewidth=0.3)+    # 设置格子边框颜色、线型和线宽
  geom_text(aes(label=支出金额),size=4,color="black")+   # 设置标签字体大小和颜色
  scale_y_discrete(label=labs)+                          # 设置y轴标签  
  scale_fill_continuous(low="#FEE5D9",high="#EF3B2C")+   # 设置颜色
  ggtitle("(a) x轴为地区的矩形热图")

# 图（b）x轴为支出项目的极坐标热图
p2<-ggplot(df)+aes(x=支出项目,y=地区,fill=支出金额)+
  geom_tile(color="grey50",linetype=1,linewidth=0.3)+
  scale_x_discrete(label=labs)+           # 设置x轴标签  
  scale_fill_continuous(low="#FEE5D9",high="#EF3B2C")+
  coord_polar()+
  ggtitle("(b) x轴为支出项目的极坐标热图")

# 组合图形并共享图例
p1+p2+plot_layout(guides="collect")


#####————————————————————————————————#####
##### 【图3-23】的绘制代码——热图极坐标分组热图
#####————————————————————————————————#####
# 图3-23的绘制代码
## 安装包：devtools::install_github("junjunlab/ggcirclize")
library(ggcirclize)
library(ggplot2)
library(reshape2)
library(patchwork)
library(ggsci)      # 用于ggplot2的科学杂志和科幻主题调色板

# 处理数据
d<-read.csv("C:/mydata/chap03/data3_3.csv")
colnames(d)[4:8]<-c("GDP2019","GDP2020","GDP2021","GDP2022","GDP2023") # 重新命名4:8列
df<-melt(d,id.vars=c("地区","地带划分","区域划分"),variable.name="年份",value.name="地区生产总值")

# 绘制分组热图
mytheme<-theme(legend.position="inside",legend.position.inside=c(0.5,-0.22),  # 设置图例位置
        legend.direction="horizontal",        # 设置图例水平摆放
        legend.key.size=unit(0.5,"cm"),       # 设置图例大小
        legend.key.width=unit(1.1,"cm"))      # 设置图例键宽度

p1<-ggcirclize(data=df,mg.t=1.8,mg.b=2.2,mg.r=0.6,mg.l=0.6, # 设置图形边距
      mapping=aes(x=地区,y=年份,fill=地区生产总值,
      sector=地带划分,start=90,end=420))+ # 设置分组扇区
  geom_tracktile(add.yaxis=FALSE,         # 不显示y轴标签
  strip.label.pos="bottom",               # 设置分组标签位置
  scales="free_x")+                       # 自由设置x轴
  #scale_fill_gradient2(low="#FEE5D9",high="#EF3B2C")+  # 设置颜色
  scale_fill_material("red")+             # 设置调色板
  annotate("text",x=0.11,y=0.76,
   label="2023年\n2022年\n2021年\n2020年\n2019年",size=2.5,color="red4")+# 添加y轴标签
  annotate("text",x=0.05,y=1.4,label="(a) 按地带划分分组",size=4.5)+# 添加标题
  mytheme

p2<-ggcirclize(data=df,mg.t=1.8,mg.b=2.2,mg.r=0.6,mg.l=0.6, # 设置图形边距
      mapping=aes(x=地区,y=年份,fill=地区生产总值,
      sector=区域划分,start=90,end=420))+ # 设置分组扇区
  geom_tracktile(add.yaxis=FALSE,         # 不显示y轴标签
  strip.label.pos="bottom",               # 设置分组标签位置
  scales="free_x")+                       # 自由设置x轴
  #scale_fill_gradient2(low="#EFF3FF",high="#2171B5")+  # 设置颜色
  scale_fill_material("blue")+
  annotate("text",x=0.14,y=0.76,
   label="2023年\n2022年\n2021年\n2020年\n2019年",size=2.5,color="blue4")+
  annotate("text",x=0.05,y=1.4,label="(b) 按区域划分分组",size=4.5)+
  mytheme

p1+p2      # 组合图形


#####================================================================#####
#####  3.5  其他比较方法
#####================================================================#####

#####————————————————————————————————#####
##### 【图3-24】的绘制代码—克利夫兰点图
#####————————————————————————————————#####
# 图3-24的绘制代码
library(ggplot2)
library(dplyr)
library(patchwork)
library(scales)    # 为使用scientific函数表示科学计数

# 数据处理
data3_3<-read.csv("C:/mydata/chap03/data3_3.csv",check.name=FALSE)
d<-data3_3[,c(1,2,8)]                                # 选择绘图变量
colnames(d)[3]<-"GDP2023"                            # 重新命名第3列
df<-d%>%
  mutate(地区=factor(地区,ordered=TRUE,levels=地区),  # 将地区设为有序类别
         地带划分=factor(地带划分,ordered=TRUE,levels=c("东部地带","中部地带","西部地带"))) # 将地带设为有序类别

# 图（a）按按地区生产总值升序排序
p1<-ggplot(df)+aes(x=reorder(地区,GDP2023),y=GDP2023)+ # 按地区生产总值升序对地区重新排序
  geom_point(size=2,color="red2")+         # 设置点的大小和颜色
  coord_flip()+                            # 坐标轴互换（水平摆放）
  labs(x="地区",y="地区生产总值",title="(a) 按地区生产总值排序")

# 图（b）按地区生产总值排序，调整 y 轴起点值
cols=ifelse(df$GDP2023>=mean(df$GDP2023),"red","blue") 
                   # 设置颜色，地区生产总值大于等于均值为红色，否则为蓝色
p_size=ifelse(df$GDP2023>100000,4,2) 
                   # 设置点的大小，地区生产总值大于100000为4，否则为2
l_width=ifelse(df$GDP2023>100000,1.2,0.6)
                   # 设置线宽，地区生产总值大于100000??2，否则为0.6
p2<-ggplot(df)+aes(x=reorder(地区,GDP2023),y=GDP2023)+
  geom_point(size=p_size,color=cols)+
  geom_segment(aes(x=地区,xend=地区,y=mean(GDP2023),yend=GDP2023),
               linewidth=l_width,color=cols)+  # 设置y轴起点为GDP2023均值
  geom_hline(yintercept=mean(df$GDP2023),color="grey50",linewidth=0.5)+ # 添加y轴均值线
  annotate("text",x=17,y=(mean(df$GDP2023)+50000),
  label=paste("地区生产总值均值\n",scientific(mean(df$GDP2023),digits=3)),
               size=3.5,fontface="bold",color="green4")+    # 添加均值标签
  coord_flip()+
  guides(y="none")+               # 删除y轴
  labs(x=NULL,y="地区生产总值",title="(b) 调整 y 轴起点")

# 图（c）按地带划分分组
p3<-ggplot(df)+aes(x=reorder(地区,GDP2023),y=GDP2023,color=地带划分)+
  geom_point(size=2)+
  geom_segment(aes(x=地区,xend=地区,y=0,yend=GDP2023),linewidth=0.6)+
  scale_color_brewer(palette="Set1")+  # 自定义颜色
  facet_grid(地带划分~.,scale="free")+ # 按地带划分分面,自由设置坐标轴
  guides(color="none")+
  coord_flip()+labs(x=NULL,y="地区生产总值",title="(c) 按地带划分分组")

p1+p2+p3+plot_layout(axes="collect_x") # 组合图形并删除重复的x轴标题


#####————————————————————————————————#####
##### 【图3-25】的绘制代码—棒棒糖图
#####————————————————————————————————#####
# 图3-25的绘制代码
library(ggplot2)
library(ggalt)
library(dplyr)
library(patchwork)

# 数据处理
data3_3<-read.csv("C:/mydata/chap03/data3_3.csv",check.name=FALSE)
d<-data3_3[,c(1,8)]                   # 选择绘图变量
colnames(d)<-c("地区","GDP2023")      # 重新命名列名
df<-d%>%arrange(desc(GDP2023))%>%     # 按地区生产总值降序排序
    mutate(地区=factor(地区,ordered=TRUE,levels=地区))

# 绘制棒棒糖图
p1<-ggplot(df)+aes(x=地区,y=GDP2023)+ 
  geom_lollipop(aes(color=地区),point.size=1.5)+
  theme(axis.text.x=element_text(angle=90))+  # x轴标签旋转90度
  geom_hline(yintercept=mean(df$GDP2023),color="red3",linetype=6,linewidth=0.5)+ # 添加y轴均值线
  guides(color="none")+                       # 删除color产生的图例
  labs(x="地区",y="地区生产总值",title="(a) 棒棒糖图")

p2<-p1+coord_polar()+
  theme(axis.text.x=element_text(angle=0))+   # x轴标签不旋转
  labs(title="(b) 极坐标棒棒糖图")

p1+p2          # 组合图形

# 注： 设置horizontal=TRUE可以水平摆放 


#####————————————————————————————————#####
##### 【图3-26】的绘制代码—哑铃图
#####————————————————————————————————#####
# 图3-26的绘制代码
# 哑铃图——比较多个样本两个不同时间点的数值差异或变化，也可以比较两个不同变量的差异（变量有可比性）
# 也可以比较两个配对样本的数据差异。如20个消费者对不同电商的评分差异，两套试卷得分的差异，等等
# 两个数据用不同颜色的点表示，两个点用直线连接

library(ggplot2)
library(reshape2)
library(dplyr)
library(patchwork)

# 图（a）按地区生产总值排序
data3_3<-read.csv("C:/mydata/chap03/data3_3.csv",check.name=FALSE)
df<-data3_3[,c(1,4,8)]
colnames(df)[2:3]<-c("GDP2019","GDP2023") # 重新命名2:3列
df1<-df%>%arrange(desc(GDP2023))%>% 
   melt(id.vars="地区",variable.name="年份",value.name="地区生产总值")

mytheme<-theme(legend.position="inside",
               legend.position.inside=c(0.8,0.1),
               legend.background=element_blank())
p1<-ggplot(df1,aes(x=地区生产总值,y=reorder(地区,地区生产总值),fill=年份))+
  geom_line(aes(group=地区),linewidth=0.8,color="grey50")+
  geom_point(shape=21,size=2)+
  mytheme+
  guides(fill=guide_legend(nrow=2,title=NULL))+# 图例摆放2行，去掉标题
  ylab("地区")+
  ggtitle("(a) 按2023年地区生产总值排序")

# 图（b）按地区原始顺序排序
df2<-df%>%
  mutate(地区=factor(地区,ordered=TRUE,levels=地区))%>%
  melt(id.vars="地区",variable.name="年份",value.name="地区生产总值")
p2<-ggplot(df2,aes(x=地区生产总值,y=地区,fill=年份))+
  geom_line(aes(group=地区),linewidth=0.8,color="grey50")+
  geom_point(shape=21,size=2)+
  mytheme+
  guides(fill=guide_legend(nrow=2,title=NULL))+
  ggtitle("(b) 按地区原始顺序排序")

p1+p2       # 组合图形


#####————————————————————————————————#####
##### 【图3-27】的绘制代码——词云图
#####—————————————————————data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCC———————————#####
# 图3-27的绘制代码
library(wordcloud2)
library(ggwordcloud)
library(patchwork)
df<-read.csv("C:/mydata/chap03/data3_4.csv")

# 图（a）星形
set.seed(12)               # 设置随机数种子以再现图形
p1<-ggplot(df,aes(label=词,size=词频,
  color=factor(sample.int(10,nrow(df),replace=TRUE)))) +# 设置颜色
  geom_text_wordcloud(shape="star",area_corr=TRUE) +# 设置形状(默认为圆形）
  scale_radius(range=c(1,25),limits=c(0,NA)) +      # 设置半径缩放范围
  theme_bw()+                                       # 设置主题
  ggtitle("(a) 星形")

# 图（b）心形
set.seed(12)
p2<-ggplot(df,aes(label=词,size=词频,color=词频)) + # 设置颜色为词频
  geom_text_wordcloud(mask=png::readPNG(system.file("extdata/hearth.png",
      package="ggwordcloud",mustWork=TRUE)),rm_outside=TRUE) + # 使用函数提供的掩码
  scale_radius(range=c(0,12),limits=c(0,NA))+
  scale_color_gradient(low="red3",high="red")+ # 设置颜色
  theme_bw()+
  ggtitle("(b) 心形")

p1+p2         # 组合图形


#####————————————————————————————————#####
##### 【图3-28】的绘制代码——词云图（wordcloud2）
#####————————————————————————————————#####
# 图3-28的绘制代码
library(wordcloud2)
data3_4<-read.csv("C:/mydata/chap03/data3_4.csv")

# 图（a）圆形（默认）+主题WCtheme(class=1)
set.seed(1)
wordcloud2(data=data3_4,shape="circle",size=0.6)+WCtheme(class=1)

# 图（b）主题WCtheme(1)+WCtheme(2)
wc<-wordcloud2(data=data3_4,shape="circle",size=0.5,
  color=ifelse(data3_4[,2]>500,"red","deepskyblue"),
                                      # 词频大于500用红色，否则用深天蓝色
  backgroundColor="black")            # 设置背景颜色
wc+WCtheme(class=1)                   # 主题WCtheme(1)
wc+WCtheme(class=1)+WCtheme(class=2)  # 主题WCtheme(1)+WCtheme(2)



#####================================================================#####
#####  3.6  添加推断信息
#####================================================================#####

#####————————————————————————————————#####
##### 【图3-29】的绘制代码——绘制误差条形图
#####————————————————————————————————#####
# 图3-29的绘制代码（数据：data2_1）
library(ggplot2)
library(reshape2)     # 为使用melt函数融合数据
library(dplyr)        # 为使用select函数和管道符%>%
library(ggfittext)    # 为使用geom_bar_text添加文本标签
library(patchwork)
data2_1<-read.csv("C:/mydata/chap02/data2_1.csv")

# 处理数据
df<-data2_1%>%select(性别,专业,Python语言,R语言)%>% # 调整变量顺序 
   melt(variable.name="课程",value.name="分数")     # 融合数据为长格式

# 计算统计量并返回数据框
df1<-df%>%      
   group_by(课程)%>%          # 按课程分组计算统计量并创建一个新数据框
   dplyr::summarise(mean=mean(分数), # 计算均值（mean）
      sd=sd(分数),            # 计算标准差（sd）
      df=n()-1,               # 使用dplyr包中的n()函数计算自由度（df）
      se=sd/sqrt(df+1))       # 计算标准误（se）

df3<-df%>%      
   group_by(课程,性别)%>%  # 按课程和性别分组计算统计量并创建一个新数据框
    dplyr::summarise(mean=mean(分数),sd=sd(分数),df=n()-1,
      se=sd/sqrt(df+1),
      E=qt(0.975,df)*sd/sqrt(df+1))  # 计算估计误差（t分布，E）

df4<-df%>%      
   group_by(课程,专业)%>%   # 按课程和专业分组计算统计量并创建一个新数据框
    dplyr::summarise(mean=mean(分数),sd=sd(分数),df=n()-1,
      se=sd/sqrt(df+1),E=qt(0.975,df)*sd/sqrt(df+1))%>%
      group_by(专业)%>%mutate(mm=cumsum(mean))%>%  # 按专业分组计算累积平均（mm）并添加到数据框
      mutate(课程=factor(课程,levels=c("R语言","Python语言")))# 修改课程为有序类别以保证误差线可以对应其正确位置

# 绘制误差条形图
p<-ggplot(df1,aes(x=课程,y=mean,fill=课程))+
  geom_col(color="grey40",width=0.6)+
  theme(legend.position="none")
p1<-p+geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),
            width=0.3,color="blue3",linewidth=1.2)+
  geom_point(size=3,shape=21,fill="yellow")+  # 添加均值点
  ggtitle("(a) 均值 ± 1个标准差","分析样本数据的离散程度")

p2<-p+geom_errorbar(aes(ymin=mean-se,ymax=mean+se),width=0.3)+
   ggtitle("(b) 均值 ± 1个标准误","分析样本均值的离散程度")

p3<-ggplot(df3,aes(x=课程,y=mean,fill=性别,label=性别))+
  geom_col(color="grey40",position="dodge")+
  geom_errorbar(aes(ymin=mean-E,ymax=mean+E),
        width=0.3,position=position_dodge(0.9))+
  geom_bar_text(position="dodge",place="center",color="grey30",min.size=1)+ 
                                                   # 设置标签和位置
   guides(fill="none")+
   ggtitle("(c) 均值 ± 估计误差(按性别并列分组)","推断总体均值的置信区间")

p4<-ggplot(df4,aes(x=专业,y=mean,fill=课程,label=课程))+
   geom_col(width=0.8,position="stack",color="grey40")+
   geom_errorbar(aes(ymin=mm-E,ymax=mm+E),width=0.3)+
   guides(fill="none")+
   geom_bar_text(position="stack",place="center",color="grey30",angle=45)+
                                                 # 设置标签位置和角度
   ggtitle("(d) 均值 ± 估计误差(按课程堆叠分组)","推断总体均值的置信区间")

(p1+p2)/(p3+p4)     # 组合图形


#####————————————————————————————————#####
##### 【图3-30】的绘制代码——检验变量间关系
#####————————————————————————————————#####
# 图3-30的绘制代码
library(ggplot2)
library(dplyr)
library(ggfittext)
library(patchwork)
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv")

# 图（a）不同满意度的拟合优度检验
df1<-data3_1%>%select(满意度)%>%table()%>%
  as.data.frame()%>%dplyr::rename(人数=Freq)  # 生成列联表并转化成数据框
p1<-(data3_1%>%select(满意度)%>%table()%>%chisq.test())$p.value
                                              # 提取检验的p值
g1<-ggplot(df1,aes(x=满意度,y=人数,fill=满意度))+
  geom_col(width=0.8,position="dodge",color="gray50")+
  ylim(0,1.05*max(df1$人数))+                                 # 设置y轴范围
  annotate("text",x=3,y=810,label=paste("p =",round(p1,16)))+ # 添加p值
  guides(fill="none")+ggtitle("(a) 拟合优度检验","(满意度)")

# 图（b）不同满意度的男女人数拟合优度检验
d<-data3_1%>%select(性别,满意度)
p21<-(d%>%filter(满意度=="不满意")%>%select(性别)%>%
  table()%>% # 筛选出不满意时的性别变量并生成频数表
     chisq.test())$p.value       # 进行卡方检验并提取p值
p22<-(d%>%filter(满意度=="满意")%>%select(性别)%>%table()%>%
     chisq.test())$p.value
p23<-(d%>%filter(满意度=="中立")%>%select(性别)%>%table()%>%
     chisq.test())$p.value

# 绘制条形图
df2<-d%>%table()%>%as.data.frame()%>%dplyr::rename(人数=Freq)
g2<-ggplot(df2,aes(x=满意度,y=人数,fill=性别,label=性别))+
  geom_col(width=0.8,position="dodge",color="gray50")+
  geom_bar_text(position="dodge",place="center",size=10)+ # 设置标签和位置  
  ylim(0,1.15*max(df2$人数))+
  annotate("text",x=1,y=480,label=paste("p =",round(p21,3)))+  # 添加p值
  annotate("text",x=2,y=400,label=paste("p =",round(p22,19)))+
  annotate("text",x=3,y=400,label=paste("p =",round(p23,3)))+
  guides(fill="none")+
  ggtitle("(b) 拟合优度检验","(不同满意度的男女人数)")  

# 图（c）网购原因与满意度的卡方独立性检验
df3<-data3_1%>%select(网购原因,满意度)%>%
  table()%>%as.data.frame()%>%dplyr::rename(人数=Freq)
c3<-chisq.test(table(data3_1$网购原因,data3_1$满意度))  # 独立性检验
g3<-ggplot(df3,aes(x=满意度,y=人数,fill=网购原因,label=网购原因))+
  geom_bar(stat="identity",width=0.8,color="gray50")+
  geom_bar_text(position="stack",place="center",size=10)+
  ylim(0,820)+
  annotate("text",x=1.9,y=800,label=expression(chi^2))+ # 添加文本
  annotate("text",x=2.3,y=800,label=paste(" =",round(c3$statistic,2)))+ 
                                                   # 添加卡方统计量的值
  annotate("text",x=3.1,y=800,label=paste("p =",round(c3$p.value,3)))+        # 添加p值
  guides(fill="none")+ggtitle("(c) 独立性检验","(网购原因与满意度)")

# 图（d）性别与满意度的卡方独立性检验
df4<-data3_1%>%select(性别,满意度)%>%
  table()%>%as.data.frame()%>%dplyr::rename(人数=Freq)
c4<-chisq.test(table(data3_1$性别,data3_1$满意度))  # 独立性检验
cont<-DescTools::ContCoef(table(data3_1$性别,data3_1$满意度)) # 使用DescTools包的ContCoef函数计算列联系数（cont）
g4<-ggplot(df4,aes(x=满意度,y=人数,fill=性别,label=性别))+
  geom_col(position="dodge",width=0.8,color="gray50")+
  geom_bar_text(position="dodge",place="center",size=10)+
  ylim(0,1.15*max(df4$人数))+
  annotate("text",x=1.1,y=490,label=expression(chi^2))+
  annotate("text",x=1.5,y=490,label=paste(" =",round(c4$statistic,2)))+
  annotate("text",x=2.25,y=490,label=paste("p =",round(c4$p.value,8)))+ 
  annotate("text",x=3.1,y=490,label=paste("cont =",round(cont,2)))+ 
                                                      # 添加列联系数
  guides(fill="none")+ggtitle("(d) 独立性检验","(性别与满意度)")

(g1+g2)/(g3+labs(caption="注：检验方法可参阅贾俊平著《统计学—基于R》(第6版)，中国人民大学出版社，2025。")+
  theme(plot.caption=element_text(size=12,color="darkblue",hjust=0))+g4)  # 组合图形并在图g3底部左下角添加注释文本


#####————————————————————————————————#####
##### 【图3-31】的绘图代码——类别变量的独立性检验
#####————————————————————————————————#####
# 图3-31的绘制代码
library(ggstatsplot)
library(ggplot2)
library(patchwork)
df<-read.csv("C:/mydata/chap03/data3_1.csv")

# 图（a）性别与满意度的百分比条形图及其检验
p1<-ggbarstats(df,x=性别,y=满意度,type="nonparametric", # 设置检验类型
    bf.message=FALSE,                      # 去掉支持原假设的贝叶斯信息
    ggtheme=theme_grey(),title ="(a) 性别与满意度")+
    theme(legend.position="bottom")


# 图（b）网购原因与满意度的百分比条形图及其检验
p2<-ggbarstats(df,x=网购原因,y=满意度,type="nonparametric",bf.message=FALSE,
    ggtheme=theme_grey(),
    title ="(b) 网购原因与满意度")+
    theme(legend.position="bottom")

p1+p2    # 组合图形 








#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####
#####  END
#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####


