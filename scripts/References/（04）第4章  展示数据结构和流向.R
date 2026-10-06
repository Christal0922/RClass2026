

###======================================================###
###  【第4章】展示数据结构和流向                         ###
###======================================================###


#####================================================================#####
#####  4.1  展示单层结构
#####================================================================#####

#####————————————————————————————————#####
##### 【图4-1】的绘制代码——单层结构——ggplot2包绘制的饼图
#####————————————————————————————————#####
# 图4-1的绘制代码（数据：data3_1）
library(ggplot2)
library(dplyr)
library(patchwork)

# 处理数据
d<-read.csv("C:/mydata/chap03/data3_1.csv") 
df1<-table(d$性别)%>%
  as.data.frame()%>%
  dplyr::rename(性别=Var1,人数=Freq)%>%
  mutate(label=paste(性别,"\n",round(人数/sum(人数)*100,2),"%")) # 创建带有百分比的标签新列
df2<-table(d$网购原因)%>%
  as.data.frame()%>%
  dplyr::rename(网购原因=Var1,人数=Freq)%>%
  mutate(label=paste(网购原因,"\n",round(人数/sum(人数)*100,2),"%"))

df3<-table(d$满意度)%>%
  as.data.frame()%>%
  dplyr::rename(满意度=Var1,人数=Freq)%>%
  mutate(label=paste(满意度,"\n",round(人数/sum(人数)*100,2),"%"))

# 绘制饼图
p1<-ggplot(df1,aes(x=1,y=人数,fill=性别))+
   geom_col(width=1,position="stack",color="white")+# 绘制堆叠条形图
   coord_polar(theta="y")+                          # 转换成极坐标
   geom_text(aes(label=label),position=position_stack(vjust=0.5),size=3,color="black")+     # 添加标签并设置位置
   theme_void()+guides(fill="none")+
   theme(plot.title=element_text(hjust=0.5))+       # 设置标题位置（居中）
   ggtitle("(a) 性别")

p2<-p1%+%df2+aes(x=1,y=人数,fill=网购原因)+ggtitle("(b) 网购原因")
p3<-p1%+%df3+aes(x=1,y=人数,fill=满意度)+ggtitle("(c) 满意度")

p1+p2+p3     # 组合图形


#####————————————————————————————————#####
##### 【图4-2】的绘制代码——分面饼图
#####————————————————————————————————#####
# 图4-2的绘制代码（数据：data3_2）
library(ggplot2)
library(dplyr)
library(reshape2)
library(ggsci)
library(plyr)     # 为使用ddply函数
library(ggh4x)    # 为使用geom_text_aimed函数添加标签

# 处理数据
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")
df<-data3_2%>%
  mutate(支出项目=factor(支出项目,ordered=TRUE,levels=支出项目))%>%
                                                 # 设置类别顺序
  melt(variable.name="地区",value.name="支出金额")%>%
  ddply("地区",transform,支出百分比=round(支出金额/sum(支出金额)*100,1))
                                                 # 按地区分组计算百分比

# 绘制分面饼图
p<-ggplot(df,aes(x="地区",y=支出百分比,fill=支出项目))+
   geom_col(color="grey90")+
   scale_fill_npg()+
   labs(x=NULL,y=NULL)+
   geom_text_aimed(aes(label=paste(支出百分比,"%")), # 添加百分比标签
      position=position_stack(vjust=0.5),hjust=-0.1,size=2.5)+# 设置标签
   theme_void()+
   theme(legend.key.size=unit(0.4,"cm"))+    # 设置图例大小
   facet_wrap(~地区,ncol=2)                  # 按地区2列分面
p+coord_polar(theta="y")


#####————————————————————————————————#####
##### 【图4-3】的绘制代码——扇形图
#####————————————————————————————————#####
# 图4-3的绘制代码（数据：data3_2）
library(ggtricks)
library(ggplot2)
library(ggsci)
library(patchwork)

# 处理数据
df<-read.csv("C:/mydata/chap03/data3_2.csv")
df$支出项目<-factor(df$支出项目,ordered=TRUE,levels=df$支出项目) # 设置类别顺序

# 图（a）北京(初始~终止角度：20~140)
mytheme<-theme_void()+
         theme(plot.title=element_text(size=12,hjust=0.5),
               legend.key.height=unit(0.5,"cm"))
p1<-ggplot(df)+
  geom_slice(aes(cat=支出项目,val=北京,fill=支出项目),color="white",# cal=类别；val=值
  init_angle=20,slice_angle=140)+  # 初始角度=20，切片（终止）角度=140
  coord_equal()+                   # 设置x轴和y轴比例相等
  scale_fill_npg()+
  mytheme+
  ggtitle("(a) 北京\n(初始~终止角度：20~140)")

# 图（b）天津(初始~终止角度：0~90)
p2<-ggplot(df)+
  geom_slice(aes(cat=支出项目,val=天津,fill=支出项目),color="white",
             init_angle=0,slice_angle=90)+
  coord_equal()+
  scale_fill_npg()+
  mytheme+
  ggtitle("(b) 天津\n(初始~终止角度：0~90)")

# 图（c）上海(初始~终止角度：0~180)
p3<-ggplot(df)+
  geom_slice(aes(cat=支出项目,val=上海,fill=支出项目),color="white",
             init_angle=0,slice_angle=180)+
  coord_equal()+
  scale_fill_npg()+
  mytheme+
  ggtitle("(c) 上海\n(初始~终止角度：0~180)")

# 图（d）重庆(初始~终止角度：90~90)
p4<-ggplot(df)+
  geom_slice(aes(cat=支出项目,val=重庆,fill=支出项目),color="white",
             init_angle=90,slice_angle=90)+
  coord_equal()+
  scale_fill_npg()+
  mytheme+
  ggtitle("(d) 重庆\n(初始~终止角度：90~90)")

p1+p2+p3+p4+plot_layout(guides="collect")&     # 组合图形并共享图例
   theme(legend.position="bottom")


#####————————————————————————————————#####
##### 【图4-4】的绘制代码——ggplot2绘制环形图
#####————————————————————————————————#####
# 图4-4的绘制代码（数据：data3_2）
library(ggplot2)
library(dplyr)
library(ggh4x)    # 为使用geom_text_aimed函数添加标签
library(ggsci)
library(patchwork)

# 处理数据
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")
df<-data3_2%>%
  mutate(北京百分比=round(北京/sum(北京)*100,1),   # 添加支出百分
  天津百分比=round(天津/sum(天津)*100,1),
  上海百分比=round(上海/sum(上海)*100,1),
  重庆百分比=round(重庆/sum(重庆)*100,1),
  支出项目=factor(支出项目,ordered=TRUE,levels=支出项目)) # 设置类别顺序

# 绘制环形图
# 图（a）北京
p<-ggplot(df,aes(x=1,y=北京百分比,fill=支出项目))+
  geom_col(width=0.2,position="stack",color="grey90")+ # 绘制堆叠条形图
  scale_fill_npg()+                                    # 设置调色板
  coord_polar(theta="y")+                              # 转换成极坐标
  theme_void()+theme(legend.position="none")+
  xlim(0.7,1.1)+   # 设置x轴范围
  theme_void()+theme(legend.key.height=unit(0.5,"cm"))
p1<-p+geom_text_aimed(aes(label=paste(北京百分比,"%")),   # 添加百分比标签
     position=position_stack(vjust=0.5),hjust=0.5,size=3)+# 设置标签位置
  annotate("text",x=0.7,y=0.5,label="北京",size=5)        # 添加标题

# 图（b）天津
p2<-p+aes(x=1,y=天津百分比,fill=支出项目)+
  geom_text_aimed(aes(label=paste(天津百分比,"%")),  # 添加百分比标签
      position=position_stack(vjust=0.5),hjust=0.5,size=3)+# 设置标签
  annotate("text",x=0.7,y=0.5,label="天津",size=5)

# 图（c）上海
p3<-p+aes(x=1,y=上海百分比,fill=支出项目)+
  geom_text_aimed(aes(label=paste(上海百分比,"%")),  # 添加百分比标签
      position=position_stack(vjust=0.5),hjust=0.5,size=3)+# 设置标签
  annotate("text",x=0.7,y=0.5,label="上海",size=5)

# 图（d）重庆
p4<-p+aes(x=1,y=重庆百分比,fill=支出项目)+
  geom_text_aimed(aes(label=paste(重庆百分比,"%")) , # 添加百分比标签
      position=position_stack(vjust=0.5),hjust=0.5,size=3)+# 设置标签
  annotate("text",x=0.7,y=0.5,label="重庆",size=5)

# 组合图形
p1+p2+p3+p4+plot_layout(ncol=2,guides="collect")  # 组合图形并共享图例


#####————————————————————————————————#####
##### 【图4-5】的绘制代码—弧形图(arc chart)——ggtricks包
#####————————————————————————————————#####
# 图4-5的绘制代码（数据：data3_2）
library(ggtricks)
library(ggplot2)
library(ggsci)
library(patchwork)

# 处理数据
df<-read.csv("C:/mydata/chap03/data3_2.csv")
df$支出项目<-factor(df$支出项目,ordered=TRUE,levels=df$支出项目)

# 图（a）北京
mytheme<-theme_void()+
         theme(plot.title=element_text(hjust=0.5),
               legend.key.height=unit(0.4,"cm"))
p1<-ggplot(df)+
  geom_donut_slice(aes(cat=支出项目,val=北京,fill=支出项目),
  r1=1,r2=0.6,          # 设置外半径（r1）和内半径（r2）
  slice_angle=130,      # 设置弧形角度=130（可设置其他任意角度）
  slice_position="top", # 设置切片位置为顶部（可选left,right,bottom）
  link_with_origin=F)+  # 不与原点连线
  coord_equal()+        # 设置x轴和y轴比例相等
  scale_fill_npg()+     # 设置调色板
  mytheme+
  ggtitle("(a) 北京")+
  annotate("text",x=0,y=0.4,label="跨度130",size=4)  # 添加标签

# 图（b）天津
p2<-ggplot(df)+
  geom_donut_slice(aes(cat=支出项目,val=天津,fill=支出项目),
      r1=1,r2=0.6,slice_angle=130,slice_position="top")+
  coord_equal()+
  scale_fill_npg()+mytheme+
  ggtitle("(b) 天津")+
  annotate("text",x=0,y=0.4,label="跨度130",size=4)# 添加标签

# 图（a）上海
p3<-ggplot(df)+
  geom_donut_slice(aes(cat=支出项目,val=上海,fill=支出项目),
      r1=1,r2=0.5,slice_angle=180)+
  coord_equal()+
  scale_fill_npg()+
  mytheme+
  ggtitle("(c) 上海")+
  annotate("text",x=0,y=0.2,label="跨度180",size=4)  # 添加标签

# 图（b）重庆
p4<-ggplot(df)+geom_donut_slice(aes(cat=支出项目,val=重庆,fill=支出项目),
  r1=1,r2=0.5,slice_angle=180)+
  coord_equal()+scale_fill_npg()+mytheme+ggtitle("(d) 重庆")+
  annotate("text",x=0,y=0.2,label="跨度180",size=4)  # 添加标签

p1+p2+p3+p4+   # 组合图形并共享图例
  plot_layout(guides="collect")&
   theme(legend.position="bottom")



#####================================================================#####
#####  4.2  展示多层结构
#####================================================================#####

#####————————————————————————————————#####
##### 【图4-6】的绘制代码—双层结构—饼环图
#####————————————————————————————————#####
# 图4-6的绘制代码（数据：data3_1）
library(ggplot2)
library(webr)
df<-read.csv("C:/mydata/chap03/data3_1.csv")

# 图（a）饼环图
p1<-PieDonut(df,aes(性别,满意度),
     r0=0,r1=0.7,r2=1,  # 饼图半径的起点为0，终点为0.7，环半径的终点为1
     pieLabelSize=2.5,donutLabelSize=2.5,# 设置饼和环的标签字体大小
     title="(a) 饼环图")

# 图（b）嵌套环形图
p2<-PieDonut(df,aes(网购原因,满意度),
     r0=0.4,r1=0.7,r2=1,  # 饼图半径的起点为0.4，终点为0.7，环半径的终点为1
     pieLabelSize=3,donutLabelSize=3,
     title="(b) 嵌套环形图")


#####————————————————————————————————#####
##### 【图4-7】的绘制代码—双层结构—饼环图—炸开
#####————————————————————————————————#####
# 图4-7的绘制代码（数据：data3_1）
library(ggplot2)
library(webr)
df<-read.csv("C:/mydata/chap03/data3_1.csv")

# 图（a）饼的"方便"部分炸开
p1<-PieDonut(df,aes(性别,满意度),
     r0=0,r1=0.7,r2=1,  # 饼图半径的起点为0，终点为0.7，环半径的终点为1
     pieLabelSize=2.5,donutLabelSize=2.5,# 设置饼和换的标签字体大小
     explode=1,explodePie=TRUE,          # 选择炸开的部分
     title="(a) 饼的男性部分炸开")

# （b）外环中的"满意"部分炸开
p2<-PieDonut(df,aes(网购原因,满意度),
     r0=0.4,r1=0.7,r2=1,
     pieLabelSize=3,donutLabelSize=3,
     selected=c(2,5,8),explodeDonut=TRUE,# 选择炸开的部分
     title="(b) 外环的满意部分炸开")


#####————————————————————————————————#####
##### 【图4-8】的绘制代码—多层结构——旭日图(sunburst chart)
#####————————————————————————————————#####
# 图4-8的绘制代码（数据：data3_1,data3_3）
library(WeightedTreemaps)
library(dplyr)
library(RColorBrewer)

# 图（a）性别、网购原因和满意度的旭日图
# 处理数据
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv")
df1<-data3_1%>%ftable()%>%
  as.data.frame()%>%rename(人数=Freq)

# 使用sunburstTreemap函数创建数据列表
data1<-sunburstTreemap(data=df1,
       levels=c("性别","网购原因","满意度"),# 设置列名称（名字的顺序必须与等级相对应，从大到小）
       cell_size="人数")                  # 设置用于控制单元格大小的列名称

# 使用drawTreemap函数绘制旭日图
drawTreemap(data1,label_size=0.8,label_color="black", # 设置标签字体大小和颜色
     border_size=1.5,                                 # 设置分割线宽度
     color_type="both", # 设置着色类型（默认color_type="categorical", 可选"cell_size","both"或 "custom_color" ）
     color_palette=brewer.pal(8,"Set3"),        # 设置颜色
     legend=TRUE,legend_position="left",legend_siz=0.14, # 设置图例位置和字体大小
     title="(a) 性别、网购原因和满意度的旭日图",# 设置标题
     title_size=1.4,title_color="black",        # 设置标题字体大小和颜色
     layout=c(1,2),position=c(1,1),add=TRUE)    # 图形布局       

# 图（b）31个地区地区生产总值的旭日图
data3_3<-read.csv("C:/mydata/chap03/data3_3.csv",check.name=F)
df2<-data3_3[,c(1,2,3,8)] 
colnames(df2)[4]<-"地区生产总值"                # 重新命名第4列

# 使用sunburstTreemap函数创建数据列表
data2<-sunburstTreemap(data=df2,levels=c("地带划分","区域划分","地区"),cell_size="地区生产总值",
   diameter_inner=0.3,diameter_outer=0.8)      # 设置最小内径和最大外径
 
# 使用drawTreemap函数绘制旭日图
drawTreemap(data2,label_size=0.4,label_color="black",border_size=1.5,
    color_palette=rev(brewer.pal(9,"YlOrRd")[-1]),       # 设置颜色
    legend=TRUE,legend_position="right",legend_siz=0.15,
    title="(b) 31个地区地区生产总值的旭日图",title_size=1.4,title_color="black",
    layout=c(1,2),position=c(1,2),add=TRUE)


#####————————————————————————————————#####
##### 【图4-9】的绘制代码——voronoi图
#####————————————————————————————————#####
# 图4-9的绘制代码（数据：data3_3）
library(WeightedTreemaps)
library(dplyr)

# 处理数据
data3_3<-read.csv("C:/mydata/chap03/data3_3.csv",check.name=FALSE)
df<-data3_3[,c(1,2,3,8)]             # 选择绘图变量
colnames(df)[4]<-"地区生产总值"      # 重新命名第4列

# 图（a）东部地带
# 使用voronoiTreemap函数创建数据列表
data1<-filter(df,地带划分=="东部地带")%>%  # 选出东部地带
  voronoiTreemap(levels=c("地区"),         # 设置列名称（名字的顺序必须与等级相对应，从大到小）
  cell_size="地区生产总值",# 设置用于控制单元格大小的列名称
  shape="circle",          # 设置树图的初始形状（默认shape="rectangle"）。目前支持的“矩形”（rectangle）、“圆角矩形（rounded_rect）”、“圆形（circle）”或“六边形（hexagon）”
  positioning="regular",   # 设置父单元中子单元起始坐标的算法
  seed=123)                # 设置随机数种子以重现图形
# 使用drawTreemap函数绘制图形
drawTreemap(data1,label_size=0.4,         # 设置标签字体大小
  label_color="black", border_size=1.5,   # 设置分割线宽度
  color_type="both",  # 设置着色类型（默认color_type="categorical", 可选"cell_size","both"或 "custom_color" ）
  title="(a) 东部地带(单层结构)",title_size=1,title_color="black", # 设置标题
  layout=c(2,3),position=c(1,1),add=TRUE) # 2行3列的图形布局（本图为第1行的第1幅图）

# 图（b）中部地带
data2<-filter(df,地带划分=="中部地带")%>%
  voronoiTreemap(levels=c("地区"),cell_size="地区生产总值",shape="circle",
  positioning="regular",seed=123)
drawTreemap(data2,label_size=0.4,label_color="black", border_size=1.5,
  color_type="both",
  title="(b) 中部地带(单层结构)",title_size=1,title_color="black",
  layout=c(2,3),position=c(1,2),add=TRUE)

# 图（c）西部地带
data3<-filter(df,地带划分=="西部地带")%>%
  voronoiTreemap(levels=c("区域划分","地区"),
                 cell_size="地区生产总值",
                 shape="circle",
                 positioning="regular",seed=123)
drawTreemap(data3,label_size=0.4,label_color="black", border_size=3,
  title="(c) 西部地带(单层结构)",title_size=1,title_color="black",
  color_type="both",
  layout=c(2,3),position=c(1,3),add=TRUE) 

# 图（d）31个地区
data4<-voronoiTreemap(data=df,levels=c("地区"),
  cell_size="地区生产总值",shape="rounded_rect",
  positioning="regular",seed=123)
drawTreemap(data4,label_size=0.4,label_color="black", border_size=1.5,
  color_type="both",
  title="(d) 31个地区(单层结构)",title_size=1,title_color="black",
  layout=c(2,3),position=c(2,1),add=TRUE)

# 图（e）按区域划分分组（双层结构）
data5<-voronoiTreemap(data=df,levels=c("区域划分","地区"),
  cell_size="地区生产总值",shape="rounded_rect",
  positioning="regular",seed=123)
drawTreemap(data5,label_size=0.4,label_color="black", border_size=3,
  title="(e) 按区域划分分组 (双层结构)",title_size=1,title_color="black",
  color_type="both",
  legend=TRUE,legend_position="left",legend_size=0.16,
  layout=c(2,3),position=c(2,2),add=TRUE) 

# 图（f）按地带划分分组（双层结构）
data6<-voronoiTreemap(data=df,levels=c("地带划分","地区"),
  cell_size="地区生产总值",shape="rounded_rect",
  positioning="regular",seed=123)
drawTreemap(data6,label_size=0.4,label_color="black", border_size=3,
  color_type="both",
  title="(f) 按地带划分分组 (双层结构)",title_size=1,title_color="black",
  legend=TRUE,legend_position="left",legend_size=0.16,
  layout=c(2,3),position=c(2,3),add=TRUE)



#####================================================================#####
#####  4.3  展示数据流向
#####================================================================#####

#####————————————————————————————————#####
##### 【图4-10】的绘制代码——桑基图
#####————————————————————————————————#####
# 图4-10的绘制代码（数据：data3_2）
library(ggplot2)
library(reshape2)
library(dplyr)
library(patchwork)
library(ggalluvial)                 # 为使用"alluvium"函数
library(ggforce)                    # 为使用gather_set_data函数

# 构建绘制桑基图的数据格式
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")

df1<-melt(data3_2,variable.name="地区",value.name="支出金额")
data1<-gather_set_data(df1,1:2)      # 生成绘制桑基图的数据

df2<-df1[,c("地区","支出项目","支出金额")]  # 调整变量顺序
data2<-gather_set_data(df2,1:2) 

# 图（a）默认类别顺序
#mycolor <- colorRampPalette(brewer.pal(9,'YlGnBu'))(13)
p1<-ggplot(data1,aes(x=x,y=支出金额,
    stratum=y,alluvium=id,fill=y,label=y))+
    geom_flow(alpha=0.5)+    # 设置连接条带颜色的透明度
    geom_stratum(alpha=0.5,color="white")+ # 设置类别矩形和边框颜色     
    geom_text(stat="stratum", size=2.5)  + # 设置类别标签字体大小
    #scale_fill_manual(values= mycolor)+
    scale_x_discrete(limits=c("支出项目","地区"),# 数值x轴刻度标签
       expand=c(0.1,0.1))+                       # 设置x轴扩展范围
    theme(legend.position="none",
      panel.grid=element_blank(),
      axis.text.y=element_text(angle=90,hjust=1,vjust=1))+
    labs(x=NULL,title="(a) 默认类别顺序")

# 图（b）调整类别顺序
p2<-p1%+%data2+
    scale_x_discrete(limits=c("地区","支出项目"),expand=c(0.1,0.1))+
    labs(title="(b) 调整类别顺序")

p1+p2           # 组合图形


#####————————————————————————————————#####
##### 【图4-11】的绘制代码——桑基图（ggplot2包）
#####————————————————————————————————#####
# 图4-11的绘制代码（数据：data3_1）
library(ggplot2)
library(ggalluvial)
library(ggforce)
library(dplyr)
library(patchwork)

# 处理数据
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv")
df<-data3_1%>%table()%>%reshape2::melt()  # 生成频数分布表并融合数据
data1<-gather_set_data(df,1:3)            # 构建绘制桑基图的数据格式
data2<-gather_set_data(df,2:3)

# 图（a）性别、网购原因和满意度
p1<-ggplot(data1,aes(x=x,y=value,stratum=y,alluvium=id,fill=y,label=y))+
  geom_flow()+geom_stratum()+
  geom_text(stat="stratum",angle=90,size=3)+
  scale_x_discrete(expand=c(0.02,0.02))+       # 设置x轴扩展范围
  scale_y_discrete(expand=c(0.02,0.02))+       # 设置y轴扩展范围
  theme(axis.text.y=element_text(angle=90,hjust=1),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(), # 调整y轴标签角度
        legend.position="none")+            
  labs(x="类别",y="人数",title="(a) 性别、网购原因和满意度")

# 图（b）按性别分面
p2<-p1%+%data2+
  facet_grid(性别~.,scales="free")+  # 按性别分面
  labs(title="(b) 按性别分面")

p1+p2+plot_layout(widths=c(1.5,1))   # 组合图形,列宽为1.5:1


#####————————————————————————————————#####
##### 【图4-12】的绘制代码——桑基图（networkD3包）
#####————————————————————————————————#####
# 图4-12的绘制代码（数据：data3_2）
library(tidyverse)
library(networkD3)
library(RColorBrewer)

# 处理数据
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")
df<-reshape2::melt(data3_2,variable.name="地区",value.name="支出金额")
colnames(df)<-c("target","source","value")     #  重新命名列名

nodes<-data.frame(name=c(as.character(df$source),
  as.character(df$target))%>% unique())        # 设置节点
df$IDsource=match(df$source,nodes$name)-1 
df$IDtarget=match(df$target,nodes$name)-1
# palette<-RColorBrewer::brewer.pal(8,"Set2")  # 设置调色板
ColourScal='d3.scaleOrdinal() .range(["#66C2A5","#FC8D62","#8DA0CB",
  "#E78AC3","#A6D854","#FFD92F","#E5C494","#B3B3B3"])'# 设置颜色向量

# 绘制桑基图ColourScal
sankeyNetwork(Links = df, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "value", NodeID = "name", 
              LinkGroup = 'source', # 颜色分组
              sinksRight=FALSE, colourScale=ColourScal, nodeWidth=30,   
              fontSize=13, nodePadding=20)


#####————————————————————————————————#####
##### 【图4-13】的绘制代码——和弦图（chord diagram）
#####————————————————————————————————#####
# 图4-13的绘制代码（数据：data3_2）
library(circlize)

# 图（a）默认扇区起始位置
data3_2<-read.csv("C:/mydata/chap03/data3_2.csv")
mat<-as.matrix(data3_2[,2:5])
rownames(mat)=data3_2[,1]# 将数据框转换成矩阵

par( mfrow=c(1,2),mai=c(0.1,0.1,0.3,0.1),cex=0.6,font.main=1)
set.seed(112)      # 数值随机数种子
myorder=c("食品烟酒","其他用品及服务","居住","生活用品及服务","交通通信","衣着","医疗保健","教育文化娱乐",
           "北京","天津","上海","重庆")# 设置标签顺序

chordDiagram(mat,order=myorder,
   annotationTrackHeight=mm_h(c(4,2)), # 设置注释值的轨道高度（距离外圆）
   grid.border="red",  # 设置外围圆弧边框的颜色
   transparency=0.6,   # 设置连接条带颜色的透明度
   big.gap=10,         # 设置大类（本例为地区和支出项目两类）扇形之间的间隔 
   small.gap=5,        # 设置小类（大类内的子类）扇形之间的间隔
   link.border="grey85")  # 设置网格边框的颜色
   title("(a) 默认扇区起始位置",cex.main=2)
   circos.clear()      # 重置圆形布局参数

# 图（b）设置扇区起始位置并突出显示链接
set.seed(112)
mycol=function(x) ifelse(x>10000,"red","grey90") # 设置颜色（数值>1000为红色，其他为灰色）
circos.par(start.degree=90,clock.wise=TRUE) # 调整扇区起始位置（角度），顺时针排序
chordDiagram(mat,order=myorder,
   col=mycol,
   annotationTrackHeight=mm_h(c(4,2)),
   grid.border="red",
   transparency=0.5,
   big.gap=10,
   small.gap=5,link.border="grey85")
   title("(b) 设置扇区起始位置并突出显示链接",cex.main=2)
   circos.clear()


#####————————————————————————————————#####
##### 【图4-14】的绘制代码——DescTools包的PlotCirc函数绘制的和弦图
#####————————————————————————————————#####
# 图4-14的绘制代码（数据：data3_1）
library(circlize)
library(dplyr)

# 处理数据
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv")
# 处理数据
data3_1<-read.csv("C:/mydata/chap03/data3_1.csv")
df1<-data3_1%>%select(性别,满意度)%>%ftable()%>%as.data.frame()
df2<-data3_1%>%select(网购原因,满意度)%>%ftable()%>%as.data.frame()

# 图（a）性别与满意度
par( mfrow=c(1,2),mai=c(0.1,0.1,0.3,0.1),cex=0.8,font.main=1)
set.seed(126)             # 数值随机数种子
chordDiagram(df1,
   annotationTrackHeight=mm_h(c(4,2)), # 设置注释值的轨道高度（距离外圆）
   transparency=0.7)
   title("(a) 性别与满意度",cex.main=1.4)
   circos.clear()

# 图（b）网购原因与满意度
set.seed(115)
chordDiagram(df2,
   annotationTrackHeight=mm_h(c(4,2)),
   transparency=0.6)
   title("(b) 网购原因与满意度",cex.main=1.4)
   circos.clear()






#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####
#####  END
#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####



