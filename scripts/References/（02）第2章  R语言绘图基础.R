

###==================================###
###  【第2章】R 语言绘图基础         ###
###==================================###


#####================================================================#####
#####  2.1  graphics简介
#####================================================================#####

#####————————————————————————————————#####
##### 【图2-1】的绘制代码——plot函数的简单应用
#####————————————————————————————————#####
# 图2-1的绘制代码
data2_1 <- read.csv("C:/mydata/chap02/data2_1.csv")  # 加载数据
attach(data2_1)    # 绑定数据框
par(mfrow = c(1,3), mai = c(0.7, 0.7, 0.4, 0.1), cex = 0.8, font.main = 1)
plot(factor(性别), xlab = "性别", ylab = "人数", 
     col = "skyblue", main = "(a) 条形图")
plot(R语言, Python语言, col = "red", pch = 19, main = "(b) 散点图")
plot(R语言~factor(专业), xlab = "专业", col = "green", main = "(c) 箱线图")


#####————————————————————————————————#####
##### 【图2-2】的绘制代码——在已有的图上增加新的元素
#####————————————————————————————————#####
# 图2-2的绘制代码
## 构建数据框
set.seed(1)                                       # 设置随机数种子
x <- rnorm(200)  
y <- 1 + 2*x + rnorm(200)
d <- data.frame(x, y)                             # 组织成数据框

# 绘制图形
par(mai = c(0.7, 0.7, 0.4, 0.4), cex = 0.8)      # 设置图形参数
plot(d, xlab = "x = 自变量", ylab = "y = 因变量")# 绘制散点图
grid(col = "grey60")                             # 添加网格线
axis(side = 4, col.ticks = "blue", lty = 1)      # 在第4个边添加坐标轴
polygon(d[chull(d),], lty = 6, lwd = 1, col = "lightyellow")  # 添加多边形并填充底色
points(d)                                               # 重新绘制散点图
points(mean(x), mean(y), pch = 19, cex = 5, col = 2)    # 添加均值点
abline(v = mean(x), h = mean(y), lty = 2, col = "gray30") 
                                                # 添加垂直和水平均值线
abline(lm(y ~ x), lwd = 2, col = 2)                     # 添加回归直线
lines(lowess(y ~ x,f=1/6), col = 4, lwd = 2, lty = 6)   # 添加拟合曲线
title("散点图及拟合直线和曲线\n并为图形增加新的元素", 
      cex.main = 1, font.main = 4)   # 增加标题并折行,使用斜体字
segments(-0.8, 0, -1.6, 3.3, lty = 6, col = "blue")     # 添加线段
arrows(0.45, -2.2, -0.8, -0.6, code = 2,
       angle = 25, length = 0.06, col = 2)     # 添加带箭头的线段
text(-2.2, 3.5, labels = expression("拟合的曲线"),
     adj = c(-0.1, 0.02), col = "blue4") 
                                               # 添加注释文本
rect(0.4, -1.6, 1.6, -3.5, col = "pink", border = "grey60")   # 添加矩形

mtext(expression(italic(hat(y)) == italic(hat(beta)[0] + hat(beta)[1]*x)),
                  cex = 0.9, side = 1, line = -4.5, adj = 0.72) 
                                               # 添加注释表达式
legend("topleft", 
       legend = c("拟合的直线", "拟合的曲线"), 
       lty = c(1, 6), col = c(2, 4), cex = 0.8, 
       fill = c("red", "blue"), box.col = "grey60",
       ncol = 1, inset = 0.02)                 # 添加图例
box(col = 4, lwd = 2)                          # 添加边框


#####————————————————————————————————#####
##### 【图2-4】的绘制代码——R的绘图颜色——查看R的调色板
#####————————————————————————————————#####
# 图2-4的绘制代码
library(RColorBrewer)
layout(matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2), 
       widths = c(1, 1))       # 页面布局
par(mai=c(0.1, 0.35, 0.2, 0.1),
    cex = 0.7, cex.main = 1.3, font.main = 1)  # 图形参数设置
display.brewer.all(type = 'seq')               # 展示连续型部分
title(main = "(a) 单色连续型部分")             # 添加标题
display.brewer.all(type = "qual")              # 展示离散型部分
title(main = "(b) 多色离散型部分")
display.brewer.all(type = "div")               # 展示极端型部分
title(main = "(c) 双色极端型部分")


#####————————————————————————————————#####
##### 【图2-5】的绘制代码——创建自己的调色板
#####————————————————————————————————#####
# 图2-5的绘制代码
library(RColorBrewer)

# 设置调色板
palette1 <- brewer.pal(7, "Reds")            # 7种颜色的红色连续型调色板
palette2 <- brewer.pal(7, "Set1")            # 7种颜色的离散型调色板
palette3 <- brewer.pal(7, "RdBu")            # 7种颜色的红蓝色极端型调色板
palette4 <- rev(brewer.pal(7, "Greens"))     # 调色板颜色反转
palette5 <- brewer.pal(8, "Spectral")[-1]    # 去掉第1种颜色，使用其余7种

# 绘制条形图
par(mfrow = c(1,5), mai = c(0.3, 0.3, 0.3, 0.1), cex = 0.6, font.main = 1)
labs<-LETTERS[ 1:7]                       # 设置字母标签向量
barplot(1:7,names = labs,col = palette1, main = "(a) 单色连续型调色板")
barplot(1:7,names = labs,col = palette2, main = "(b) 多色离散型调色板")
barplot(1:7,names = labs,col = palette3, main = "(c) 双色极端型调色板")
barplot(1:7,names = labs,col = palette4, main = "(d) 调色板颜色反转")
barplot(1:7,names = labs,col = palette5, main = "(e) 去掉第1种颜色")


#####————————————————————————————————#####
##### 【图2-6】的绘制代码——获取16进制的颜色代码
#####————————————————————————————————#####
library(RColorBrewer)
library(scales)  # 使用show_col函数显示颜色代码
show_col(brewer.pal(10, "RdBu"), ncol = 5, cex_label = 0.8)
scales:: show_col(brewer.pal(10, "RdBu"), ncol = 10, cex_label = 0.7)


#####————————————————————————————————#####
##### 【图2-7】的绘制代码——图形布局：layout函数
#####————————————————————————————————#####
# 图2-7的绘制代码
# 图（a）2行2列的图形矩阵，第2行为1个图
layout(matrix(c(1, 2, 3, 3), 
       nrow = 2, ncol = 2, byrow = TRUE), heights = c(2, 1))# 页面布局
                                                  
layout.show(3)                                    # 预览布局

# 图（b）2行3列的图形矩阵，第2行为3幅图
layout(matrix(c(1, 1, 1, 2, 3, 4), nrow = 2, ncol = 3, byrow = TRUE),
       widths = c(3: 1), heights = c(2, 1))
layout.show(4)

# 图（c）3行3列的图形矩阵，第2行为2个图
layout(matrix(c(1, 2, 3, 4, 5, 5, 6, 7, 8), 3, 3, byrow = TRUE),
       widths = c(2: 1), heights = c(1: 1))
layout.show(8)


#####————————————————————————————————#####
##### 【图2-8】的绘制代码——8幅图的布局：layout函数
#####————————————————————————————————#####
# 图2-8的绘制代码
set.seed(12); n=100; x<-rnorm(n); y<-rexp(n)
layout(matrix(c(1, 1, 2, 3, 4, 4, 5, 5, 6, 7, 7, 8), 3, 4, 
       byrow = TRUE),widths=c(1:1),heights=c(1:1))
par(mai = c(0.3, 0.3, 0.3, 0.1), cex.main = 1.2, font.main = 1)
barplot(runif(8, 1, 8), names = LETTERS[1:8],col = 2:7,  
        main = "(a) 条形图")
pie(1:12, col = rainbow(6), labels = "", border = NA, 
    main = "(b) 饼图")
qqnorm(y, col = 1:7, pch = 19, xlab = "", ylab = "",
       main = "(c) Q-Q图")
plot(x, y, pch = 21, bg = c(2, 3, 4), cex = 1.2, 
     xlab = "", ylab = "", main = "(d) 散点图")
plot(rnorm(25), rnorm(25), cex = (y + 2), col = 2:4, 
     lwd = 2, xlab = "", ylab = "", main = "(e) 气泡图")
hist(rnorm(1000), col = 3, xlab = "", ylab = "",
     main = "(f) 直方图")
plot(density(y), col = 4, lwd = 1, xlab = "", ylab = "",
     main="(g) 核密度图")
polygon(density(y), col = "gold", border = "blue")
boxplot(x, col = 2, main = "(h) 箱线图")


#####————————————————————————————————#####
##### 【图2-9】的绘制代码——同时打开多个绘图窗口
#####————————————————————————————————#####
# 图2-9的绘制代码
x <- rnorm(1000)                      # 生成1000个标准正态分布的随机数
hist(x, col = 3, xlab = "", ylab = "", main = "直方图")       # 绘制直方图
dev.new()                                         # 打开一个新的绘图窗口
plot(density(x), xlab = "", ylab = "", main = "核密度图")  # 绘制核密度图

dev.off()       



#####================================================================#####
#####  2.2  ggplot2简介
#####================================================================#####

#####————————————————————————————————#####
##### 【图2-10】的绘制代码——ggplot2的绘图语法
#####————————————————————————————————#####
# 图2-10的绘制代码
library(ggplot2)           # 加载包
library(reshape2)          # 为使用melt函数融合数据
library(patchwork)         # 为使用其算法组合图形
data2_1 <- read.csv("C:/mydata/chap02/data2_1.csv") # 加载数据

# 将数据融合为长格式
df <- melt(data2_1, id.vars = c("性别", "专业"),
           variable.name = "课程", 
           value.name = "分数")

# 绘制图形
p1<-ggplot(data = df, mapping = aes(x = 性别, fill = 性别)) +  # 设置x轴，按性别填充颜色
     geom_bar() +                        # 绘制条形图
     theme(legend.position = "none")+    # 设置主题（删除图例） 
     labs(y="人数",title="(a) 条形图")   # 设置y轴标题和主标题

p2 <- ggplot(data = df , mapping =  aes(x = 分数)) +
     geom_histogram(binwidth = 5, fill = "lightgreen", color = "gray60") +
     ggtitle("(b) 直方图")              # 设置主标题

p3<-ggplot(data = data2_1, mapping= aes(x = R语言, y = Python语言, fill = 性别)) +
   geom_point(size = 2, shape = 21) +            # 绘制散点图
   theme(legend.position="inside",legend.justification=c("left","top"),
       legend.background=element_blank())+       # 移除图例整体边框
   guides(fill=guide_legend(nrow=1,title=NULL))+ # 图例排成1行,去掉图例标题
   ggtitle("(c) 散点图")

p4<-ggplot(data = df, mapping = aes(x = 课程, y = 分数, fill = 性别)) +
     geom_boxplot() +                # 绘制箱线图
     facet_wrap( ~性别) +            # 按性别分面
     theme(legend.position = "none")+
     ggtitle("(d) 分面箱线图")

# 组合图形
p1 + p2 + free(p3, side = "t") + p4   # p3与p4顶部对齐


#####————————————————————————————————#####
##### 【图2-11】的绘制代码——设置坐标轴
#####————————————————————————————————#####
# 图2-11的绘制代码
library(ggplot2)
library(patchwork)
library(reshape2)

# 将数据融合成长格式
data2_1 <- read.csv("C:/mydata/chap02/data2_1.csv")
df<-melt(data2_1, id.vars = c("性别", "专业"), 
         variable.name = "课程", value.name = "分数")

# 图（a）修改类别轴标签顺序
p1<-ggplot(data = df) + aes(x = 课程, y = 分数, fill = 课程) +
  geom_boxplot() +                                      # 绘制箱线图
  scale_x_discrete(limits = c("Python语言", "R语言")) +# 修改类别轴标签顺序
  theme(legend.position="none") +    # 删除图例
  ggtitle("(a) 修改类别轴标签顺序\n默认顺序R语言、Python语言")

# 图（b）坐标轴互换，并反转x轴项目的顺序
p2<-ggplot(data = df) + aes(x = 课程, y = 分数, fill = 课程) +
  geom_boxplot() +
  coord_flip() +              # 坐标轴互换（或者设置y=分数,x=课程）
  ylim(54, 101) +             # 设置y轴值域（数值范围）
  theme(axis.text.y = element_text(size = 9, angle = 90,
                                   hjust = 0.5, vjust = 0.5)) +
                                # 设置y轴标签角度，并进行水平和垂直位置调整
  scale_x_discrete(limits = rev(levels(df$课程))) + # 反转类别轴项目顺序
  theme(legend.position="none") +   # 删除图例
  ggtitle("(b) 坐标轴互换\n反转x轴项目顺序、标签旋转90度")

# 图（c）移除y轴刻度线和标签，删除x轴和y轴次网格线
p3<-ggplot(data = df) + aes(x = 分数, color = 课程) +
  geom_density(linewidth = 1) +   # 绘制核密度图
  xlim(50,105) +                  # 设置x轴值域（数值范围）
  ylim(0,0.07) +                  # 设置y轴值域（数值范围）
  theme(axis.title.y = element_blank(),          # 移除y轴标题
        axis.ticks.y = element_blank(),          # 移除y轴刻度线
        panel.grid.minor.x = element_blank(),    # 移除x轴次网格线
        panel.grid.minor.y = element_blank(),    # 移除y轴次网格线
        legend.position = "inside", 
        legend.justification = c("right", "top"),
        legend.key.size = unit(0.4, "cm"),          # 设置图例键大小
        legend.background = element_blank()) +      # 移除图例整体边框
  guides(color = guide_legend(nrow = 2,title = NULL)) + # 图例排成2行,去掉图例标题
  ggtitle("(c) 移除y轴刻度线和y轴标题\n移除x轴和y轴次网格线")

# 图（d）移除所有刻度线，刻度标签旋转90度
p4<-ggplot(data = df) + aes(x = 分数, color = 课程) +
  geom_density(linewidth = 1) +          # 绘制核密度图
  scale_x_continuous(limits = c(50,100),
                       breaks = c(50, 55, 60, 65, 70, 75, 
                                  80, 85, 90, 95, 100)) +
                                         # 设置x轴值域和刻度线位置
  scale_y_continuous(limits = c(0, 0.07),
   breaks = c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07)) +
                                         # 设置y轴值域和刻度线位置
   theme(axis.ticks = element_blank(),   # 移除所有刻度线
    axis.line = element_line(color = "blue", linewidth = 1.5),
                                         # 添加坐标轴直线
    axis.text.x = element_text(size = 9, angle = 90, hjust = 1, vjust = 1), 
                                         # 设置x轴标签角度
    legend.position = "inside", 
    legend.justification = c("right", "top"),
    legend.key.size = unit(0.4, "cm"),  # 设置图例键大小
    legend.background = element_blank()) +    # 移除图例整体边框
  guides(color = guide_legend(nrow = 2, title = NULL)) + # 图例排成2行,去掉图例标题
  labs(y="密度",title="(d) 移除所有刻度线\nx轴刻度标签旋转90度")

p1 + p2 + p3 + p4+ plot_layout(ncol = 2)       # 按2列组合图形


#####————————————————————————————————#####
##### 【图2-12】的绘制代码——设置图形标题
#####————————————————————————————————#####
# 图2-12的绘制代码
library(ggplot2)
library(reshape2)
library(patchwork)

# 绘制图形
data2_1 <- read.csv("C:/mydata/chap02/data2_1.csv")
df<-melt(data2_1, id.vars = c("性别", "专业"),
         variable.name = "课程", value.name = "分数")
p<-ggplot(data = df) + aes(x = 课程, y = 分数, fill = 性别) +
    geom_boxplot()    # 绘制箱线图

# 设置主标题
p1<-p + ggtitle("(a) 这里是主标题(默认设置)")   # 添加主标题

p2<-p + ggtitle("(b) 这里是主标题(设置字体大小,粗体字)")+
    theme(plot.title = element_text(size = 10, face = "bold"))
                                                # 设置主标题字体大小

p3<-p + labs(title = ("(c) 这里是主标题(位置居中)\n(标题换行)")) +
                                                # 主标题换行（在\n处换行）
theme(plot.title = element_text(size = 12, hjust = 0.5))
                                                # 调整主标题位置（居中）

p4<-p + ggtitle("(d) 这里是主标题 (蓝色粗斜体)", "(这里是副标题)") +
                                                # 添加副标题
theme(plot.title = element_text(size = 12, face="bold.italic",
                                color = "blue3"))
                                             # 设置主标题为粗斜体字，蓝色

p1 + p2 + p3 + p4       # 组合图形


#####————————————————————————————————#####
##### 【图2-13】的绘制代码——设置图例
#####————————————————————————————————#####
##### 注：内部图例位置设置（新版ggplot3.5.0）
theme(
  legend.position = "inside",
  legend.position.inside = c(.95, .95),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.margin = margin(6, 6, 6, 6)
)
#####———————
# 图2-13的绘制代码
library(ggplot2)
library(reshape2)
library(patchwork)

# 绘制图形
data2_1 <- read.csv("C:/mydata/chap02/data2_1.csv")
df <- melt(data2_1, id.vars = c("性别", "专业"),
         variable.name = "课程", value.name = "分数")
p <- ggplot(data = df) + aes(x = 课程, y = 分数, fill = 性别) +
   geom_boxplot()            # 绘制箱线图

# 设置图例
p1 <- p + ggtitle("(a) 默认图例")
p2 <- p + ggtitle("(b) 移除图例")+
    theme(legend.position = "none")  # 移除图例(或设置guides(fill="none"))
p3 <- p+ggtitle("(c) 设置图例位置、字体、背景和边框颜色")+
    theme(legend.text = element_text(size = 8, color= "blue"), 
                                               # 设置图例字体大小和颜色
          legend.position = "top",             # 设置图例位置（顶部）
          legend.key.height = unit(0.3,"cm"),  # 设置图例键高度
          legend.key.width = unit(0.6,"cm"),   # 设置图例键宽度
          legend.background = element_rect(fill = "lightyellow", 
                                           color = "grey"),
                                              # 设置图例背景色和边框颜色
          legend.key = element_rect(color = "blue", linewidth = 0.25))
                                              # 设置图例键的颜色和线宽
p4 <- p+ggtitle("(d) 设置图例位置、摆放方式和顺序")+
    theme(legend.position = "inside",             # 设置图例位置为内部
          legend.position.inside = c(0.75, 0.9),  # 设置图例位置坐标
          legend.background = element_blank(),    # 移除图例整体边框
          legend.text = element_text(size = 8))+  # 设置图例字体大小
    guides(fill = guide_legend(nrow = 1,title = NULL))+
                                   # 设置图例摆放方式(1行，去掉图例标题)
    scale_fill_discrete(limits = c("女", "男"))   # 修改图例顺序

p1 + p2 + p3 + free(p4, side = "t")    # 组合图形,p4与p3顶部对齐


#####————————————————————————————————#####
##### 【图2-14】的绘制代码——长标签的处理
#####————————————————————————————————#####
# 图2-14的绘制代码
library(ggplot2)
library(patchwork

# 构建数据框（3个专业和3们课程的平均考试分数）
df <- data.frame(
  专业 = c("流行病和卫生统计", "数据科学与大数据技术", "数理统计"),
  课程 = c("Python机器学习原理与实践", "数据建模", "数据科学统计基础"),
  平均分数 = c(76, 88, 82)
)

# 绘制条形图
p <- ggplot(df) + aes(x = 课程, y = 平均分数, fill = 专业) +
  geom_col(width = 0.8, color = "grey50")

# 图（a）默认绘制的标签
p1<-p + theme(legend.background = element_blank(), # 移除图例整体边框
      plot.background=element_rect(fill = "lightyellow")) +
                                                   # 设置图形整体背景色
  ggtitle("(a) 默认绘制的标签")

# 图（b）在适当位置换行
p2 <- p + 
 scale_x_discrete(labels = c("Python\n机器学习\n原理与实践", 
                             "数据建模","数据科学\n统计基础")) + 
                                                  # 将x轴标签换行
 scale_fill_discrete(labels = c("流行病\n和卫生\n统计",
                                "数据科学\n与大数据\n技术", "数理\n统计")) +# 将图例标签换行
 theme(axis.text = element_text(lineheight = 1),  # 设置x轴标签文本的高度
       legend.text = element_text(lineheight = 1),# 设置图例文本高度
       legend.key.height = unit(1, "cm")) +       # 设置图例键高度
 ggtitle("(b) 在适当位置换行")

# 图（c）x轴标签排成两行（或多行）
p3 <- p + 
 theme(legend.text = element_text(lineheight = 1),
                                 legend.key.height = unit(1, "cm")) +
 scale_fill_discrete(labels = c("流行病和\n卫生统计",
                                "数据科学\n与大数据\n技术",
                                "数理统计")) +
 scale_x_discrete(guide = guide_axis(n.dodge = 2))+  # x轴标签排成2行
 ggtitle("(c) x轴标签排成两行")

# 图（d）设置x轴标签角度
p4 <- p+
   theme(axis.text.x = element_text(size = 9, angle = 20,
                                    hjust=1, vjust = 1),# 设置x轴标签角度
        legend.text = element_text(lineheight = 1),
        legend.key.height = unit(1, "cm")) +
 scale_fill_discrete(labels = c("流行病和\n卫生统计",
                                "数据科学\n与大数据\n技术",
                                "数理统计"))+
 ggtitle("(d) 设置x轴标签角度")

p1 + p2 + p3 + p4             # 组合图形


#####————————————————————————————————#####
##### 【图2-15】的绘制代码——其他标签处理
#####————————————————————————————————#####
# 图2-15的绘制代码
library(ggplot2)
library(dplyr)
library(reshape2)
library(patchwork)
library(ggrepel)       # 为使用geom_text_repel函数添加标签
library(ggfittext)     # 为使用geom_bar_text添加标签
library(geomtextpath)  # 为使用geom_textdensity函数添加曲线标签
data2_1 <- read.csv("C:/mydata/chap02/data2_1.csv")

# 图（a）使用geom_text函数添加标签
dot <- ggplot(data = data2_1) + aes(x = R语言, y = Python语言) +
    geom_point(size = 2, shape = 21,
               color = "black", fill = "red2") + # 绘制散点图
    scale_x_continuous(breaks = c(70, 75, 80, 85, 90, 95, 100))
                                           # 设置坐标轴刻度
p1 <- dot + geom_text(aes(label = 专业),nudge_x = 2,size = 2.5) +
                                           # 微调标签的水平偏移量
    ggtitle("(a) geom_text函数添加标签", "(标签有重叠)")

# 图（b）使用geom_text_repel函数添加标签
p2 <- dot + geom_text_repel(aes(label = 专业), color = "black",
                            size = 2.5, max.overlaps = 25)+ 
                                          # 删除重叠点数超过25的标签
    ggtitle("(b) geom_text_repel函数添加标签", "(避免标签重叠)")  

# 图（c）使用geom_bar_text函数添加标签
df1<-data2_1 %>% 
     select(性别, 专业) %>%
     table() %>%
     as.data.frame() %>%
     rename(人数=Freq)
p3<-ggplot(df1) + aes(x = 性别, y = 人数, fill = 专业, label = 专业) +
  geom_col(width = 0.8, color = "grey50", show.legend = FALSE) + 
                                            # 绘制条形图
  geom_bar_text(position = "stack", place = "center",
                color = "grey40", size=15)+ # 设置标签位置、颜色和字体大小
  ggtitle("(c) geom_bar_text函数添加标签", "(填充条形标签)")

# 图（d）使用geom_textdensity函数添加曲线标签
df2 <- data2_1 %>%
       rename(R_language=R语言, Python_language = Python语言) %>% 
                                            # 重新命名变量
    melt(id.vars = c("性别", "专业"), 
         variable.name = "课程", value.name = "分数")
p4 <- ggplot(data = df2) + aes(x = 分数, color = 课程, label = 课程) +
    geom_density(linewidth = 1) +           # 绘制核密度图
    xlim(50, 105) + ylim(0, 0.07) +
    geom_textdensity(size = 3, vjust = -0.6, hjust = 0.33) +
                                 # 设置标签字体大小、垂直和水平位置调整
    guides(color = "none") +     # 删除color产生的图例
    ggtitle("(d) geom_textdensity函数添加标签", "(添加曲线标签)")

 p1 + p2 + p3 + p4                     # 组合图形


#####————————————————————————————————#####
##### 【图2-16】的绘制代码——设置图形主题
#####————————————————————————————————#####
# 图2-16的绘制代码
library(ggplot2)
library(reshape2)
library(patchwork)
data2_1 <- read.csv("C:/mydata/chap02/data2_1.csv")

# 将数据融合成长格式
df <- melt(data2_1, id.vars = c("性别", "专业"),
         variable.name = "课程", value.name = "分数")

# 绘制箱线图
p <- ggplot(data = df) + aes(x = 课程, y = 分数, fill = 性别) +
    geom_boxplot()    # 绘制箱线图
p1 <- p + theme_grey() +
      ggtitle("(a) theme_grey")         # 默认主题
p2 <- p + theme_bw() +
      ggtitle("(b) theme_bw")           # 黑白主题
p3 <- p + theme_test()+ggtitle("(c) theme_test")    # 测试主题
p4 <- p + theme_classic() +
      ggtitle("(d) theme_classic")      # 经典主题
p5 <- p + theme_minimal() +
      ggtitle("(e) theme_minimal")      # 最小主题
p6 <- p + theme_dark() +
      ggtitle("(f) theme_dark")         # 黑暗主题

p1 + p2 + p3 + p4 + p5 + p6    # 组合图形



#####————————————————————————————————#####
##### 【图2-17】的绘制代码——设置图形主题
#####————————————————————————————————#####
# 图2-17的绘制代码（使用图2-16绘制的图形p）
library(ggplot2)
library(ggthemes)
library(reshape2)
library(patchwork)
data2_1 <- read.csv("C:/mydata/chap02/data2_1.csv")

# 将数据融合成长格式
df <- melt(data2_1, id.vars = c("性别", "专业"),
         variable.name = "课程", value.name = "分数")

# 绘制箱线图
p <- ggplot(data = df) + aes(x = 课程, y = 分数, fill = 性别) +
    geom_boxplot()    # 绘制箱线图

p1 <- p + theme_economist() + 
      ggtitle("(a) theme_economist")  # 《经济学人》杂志主题
p2 <- p + theme_excel_new() +
      ggtitle("(b) theme_excel_new")  # Excel默认的新主题
p3 <- p + theme_stata() + 
      ggtitle("(c) theme_stata")      # 基于Stata图形方案的主题

p1 + free(p2, side = "t") + p3        # 组合图形，p2顶部对齐


#####————————————————————————————————#####
##### 【图2-18】的绘制代码——图形注释
#####————————————————————————————————#####


# 图2-18的绘制代码
library(ggplot2)          # 加载包
library(gground)          # 为使用其函数添加圆角矩形

d <- read.csv("C:/mydata/chap02/data2_1.csv")

# 绘制散点图
p <- ggplot(data = d) + aes(x = R语言, y = Python语言) +
    geom_point(size = 3, shape = 21, 
               color = "black", fill = "red2") + # 绘制散点图
    scale_x_continuous(breaks = c(70, 75, 80, 85, 90, 95)) +
                                  # 设置x轴值域和刻度线位置
    stat_smooth(method = "lm")    # 添加线性回归线和置信带

# 添加注释
p+geom_vline(xintercept = mean(d$R语言), linetype = "twodash",
             color = "grey50", linewidth = 0.5) + # 添加x的均值线（垂直线）
  geom_hline(yintercept = mean(d$Python语言), linetype = "twodash",
             color = "grey50", linewidth = 0.5 )+ # 添加y的均值线（水平线）
  geom_point(x = mean(d$R语言), y = mean(d$Python语言), shape = 21,
             size = 6, fill = "yellow") +# 添加均值点

  annotate("text", x = 73.7, y = 81,
      label = paste("相关系数: r = ",round(cor(d$R语言, d$Python语言), 4)),
           size = 5, color = "red3") +   # 添加相关系数
  
  geom_round_rect(xmin = 87, xmax = 97, 
            ymin = 56.5, ymax = 63, 
            fill = "grey85",
            radius = grid::unit(5, "mm")) + # 添加圆角矩形
  
  annotate("text", x = 92, y = 60,
           parse = TRUE, size = 5, color = "red3",
           label = "r==frac(cov(xy),sqrt(var(x)*var(y)))") +  
                                         # 添加相关系数的数学表达式


  annotate("text" ,x = 84, y = 81, label = "回归线:",
           size = 5, color = "blue3") +  # 添加注释文本
  annotate("text", x = 88.8, y = 81.3,
           parse = TRUE, size = 4.5, color = "blue3",
           label = "hat(y) == hat(beta)[0] + hat(beta)[1]*x") +  
                                         # 添加回归方程数学表达式
  annotate("segment", x = 68.5, xend = 79,
           y = 79.8, yend = 79.8,
           color = "red4", linewidth=0.5) + # 添加直线
  annotate("segment", x = 88, xend = 92,
           y = 80, yend = 78, 
           color = "blue", linewidth = 1,
           arrow = arrow(angle = 15, length = unit(0.2, "inches"))) +
                                            # 添加带箭头的直线
  annotate("curve", x = 80, xend = 87,
           y = 65, yend = 61, color = "red3",
           linewidth = 0.8, 
           arrow = arrow(length = unit(0.1, "inches"))) +
                                            # 添加带箭头的曲线
  labs(caption="注释：\n相关系数（correlation coefficient）是度量两个变量之间线性关系强度的统计量，记为r。
 r的值域为[-1,1]，绝对值越趋于1表示两个变量之间的线性关系越强，越趋于0表示关系越弱。
 r大于0表示两个变量之间为正线性相关，小于0为负线性相关，等于0表示不存在线性相关。\n
                      ——摘自贾俊平著《统计学—基于R》(第6版)，中国人民大学出版社，2025。")+       # 添加图形脚注
  theme(plot.caption = element_text(hjust = 0, size = 11, color = "steelblue4"))
                          # 设置脚注文本位置（左下角）、字体大小和颜色


#####————————————————————————————————#####
##### 【图2-19】的绘制代码——图形分面
#####————————————————————————————————#####
# 图2-19的绘制代码
library(ggplot2)
library(reshape2)
library(patchwork)
data2_1 <- read.csv("C:/mydata/chap02/data2_1.csv")


# 绘制分面图形
df <- melt(data2_1, id.vars = c("性别", "专业"),
           variable.name = "课程", value.name = "分数")

p1 <- ggplot(data = df) +
      aes(x = 课程, y = 分数, fill = 性别) +
      geom_boxplot()+
      facet_wrap( ~性别, ncol = 2) +           # 按性别2列分面
      ggtitle("(a) 按性别2列分面")

p2 <- ggplot(data = df) +
      aes(x = 课程, y = 分数, fill = 专业) +
      geom_boxplot() +
      facet_grid(性别~.) +                    # 按性别2行分面
      ggtitle("(b) 按性别2行分面")

p3 <- ggplot(data = df) +
      aes(x = 课程, y = 分数, fill = 性别) +
      geom_boxplot() +
      facet_wrap( ~专业, ncol = 3) +          # 按专业3列分面
      ggtitle("(c) 按专业3列分面")

p4 <- ggplot(data = df) +
      aes(x = 专业, y = 分数, fill = 专业) +
      geom_boxplot() +
      facet_grid(课程 ~ 性别) +              # 按课程（行）和性别（列）分面
      theme(panel.spacing.x = unit(0.2, "lines"),   # 设置子图的x轴间距
            panel.spacing.y = unit(0.1, "lines"),   # 设置子图的y轴间距
            strip.text = element_text(size = 10),   # 设置分面字体大小
            strip.background = element_rect(fill = "skyblue", 
                                            color = "blue4")) +
                                     # 设置分面背景颜色和边框颜色
    ggtitle("(d) 按课程和性别分面")

(free(p1, side = "t") + p2)/(p3 + p4) 
                                    # 组合图形，p1与p2顶部对齐，p3和p4换行


#####————————————————————————————————#####
##### 【图2-20和图2-21】的绘制代码——图形组合
#####————————————————————————————————#####
# 图2-20的绘制代码
library(ggplot2)          # 加载包
library(reshape2)         # 为使用melt函数融合数据
library(patchwork)        # 为使用运算符组合图形
library(gridExtra)        # 为使用函数组合图形

# 将数据融合成长格式
data2_1 <- read.csv("C:/mydata/chap02/data2_1.csv")
df <- melt(data2_1, id.vars = c("性别", "专业"),
           variable.name = "课程", value.name = "分数")

# 设置图形主题（可根据需要设置）
mytheme <- theme(plot.title = element_text(size = 12), # 设置主标题字体大小
   axis.title = element_text(size = 10),      # 设置坐标轴标题字体大小
   axis.text = element_text(size = 9),        # 设置坐标轴刻度标签字体大小
   legend.position = "none")                  # 删除图例

# 绘制图形p1、p2、p3、p4、p5
p <- ggplot(data = df) 
p1 <- p + aes(x = 性别, fill = 性别) +
    geom_bar(width = 0.8) + ylab("人数") +            # 绘制条形图
    mytheme + ggtitle("(a) 条形图")
p2 <- p + aes(x = 分数) +
    geom_histogram(binwidth = 5, fill = "lightgreen", color = "gray50") +
                                                    # 绘制直方图
    mytheme + ggtitle("(b) 直方图")
p3 <- p + aes(x = 专业, y = 分数, fill = 专业) + 
                                                     # 绘制箱线图
    geom_boxplot() +        
    mytheme + ggtitle("(c) 箱线图")
p4 <- p + aes(x = 课程, y = 分数, fill = 课程) +
    geom_violin() +                                  # 绘制小提琴图
    mytheme + ggtitle("(d) 小提琴图")
p5 <- p + aes(x = 分数, color = 课程) +
    geom_density(linewidth = 1) +                    # 绘制核密度图
    xlim(50,105) + ylim(0, 0.07) +
    mytheme + ggtitle("(e) 核密度图")

# 图2-20：使用patchwork包组合图形
# 组合图[G1]：2行3列的组合
g1<-((p2 + p3 + p1) + plot_layout(widths = c(2,1, 1)))/
                                     # p2、p3和p1为1行（3列），列宽为2:1:1
  ((p4 + p5) + plot_layout(widths = c(1, 2))) +  
                                     # p4和p5为1行（2列）,列宽为1:2
   plot_annotation("[G1]  2行3列的组合") # 添加标题

# 组合图[G2]：3行2列的组合
g2<-((p1/p2/p3)|                         # p1、p2和p3为1列（3行）
  (p4/p5) +                              # p4和p5为1列（并与前图并列）
   plot_layout(heights = c(1, 1.5))) +   # p4和p5行高为1:1.5
   plot_annotation("[G2]  3行2列的组合") # 添加标题

cowplot::plot_grid(g1, g2)  
                   # 使用cowplot包中的plot_grid函数对组合后的图形再组合
## 注：使用patchwork包运行g1|g2也可以组合g1和g2








#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####
#####  END
#####————————————————————————————————#####
#####————————————————————————————————#####
#####————————————————————————————————#####





