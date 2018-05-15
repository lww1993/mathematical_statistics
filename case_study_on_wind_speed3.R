########
#读文件#
########
library(xlsx)
turbine_data <- read.xlsx(file = "D:/Alfred20151208/Work/work2017Autumn/MathematicalStatisticswithR2017/theFourthTime/TurbineData.xls",
                         sheetName = "Sheet1", header = TRUE)
head(turbine_data)
dim(turbine_data)
###################
#Weibull分布的拟合#
###################
#输入：形状参数k和the data
#输出：(1/alpha)*sum[(xi^k)*log(xi)]-(1/k)-(1/n)*sum (log(xi)) 
#其中alpha= sum(xi^k).
weibull_shape <- function(k, data){
  numer <- sum((data ^ k) * log(data))
  denom <- sum(data ^ k)
  return(numer / denom - 1 / k - mean(log(data)))
}
#输入：形状参数k和数据
#输出：对(1/n) sum xi^k 开k次方 也就是计算lambda
#其中n是数据的长度
weibull_scale <- function(k, data)
{
  return(mean(data ^ k) ^ (1 / k))
}
wind <- turbine_data$AveSpeed
#uniroot是在lower与upper中寻找相应的零点。
k <- uniroot(f = weibull_shape, data = wind, lower = 1,upper = 5)$root
#利用k与wind计算相应的lambda
lambda <- weibull_scale(k, wind)

#比较直方图与计算的概率密度函数曲线
#画直方图，prob=TRUE,此时得到的直方图面积为1
hist(wind, main = "Distribution of average wind speeds",
    xlab = "meters/sec", prob = TRUE)
#添加weibull分布的密度分布函数
curve(dweibull(x, shape=k, scale=lambda), add = TRUE, col = "blue", lwd = 2)
#比较经验分布函数与计算的分布函数图
#打开新的图形界面
dev.new()
#经验概率分布函数
plot.ecdf(wind,main = "ECDF of wind data")
#Weibull分布的概率分布函数
curve(pweibull(x, k, lambda), add=TRUE, col="blue",lwd=2)
#现在利用卡方检验做分布拟合检验
#检验的目的是判断拟合的分布与这些点的实际分布是否相同
#这个时候， 我们将区间根据分位数分为十份
#原假设是在每个区间中的点数的频率与其概率相等
#详见概率论与数理统计教程 第一版 茆诗松、程依明、濮晓龙 7.4节 分布拟合检验 第356页
#详见概率论与数理统计教程 第二版 茆诗松、程依明、濮晓龙 7.4节 似然比检验与分布拟合检验
#得到相应的0.1,0.2,...,0.9分位数
q <- qweibull(seq(.1, .9, by = .1), shape = k, scale = lambda)
#wind的最大值与最小值
range(wind)
#此时q为直方图不同区间的分隔点
q <- c(0, q, 14)
#breaks代表的是不同区间的分割点
#plot=F代表的是此时我们不绘图，只计算相应的counts,也就是每个小区间所含有的点个数 
count <- hist(wind, breaks = q, plot = F)$counts
#每个区间期望含有的点数
expected <- length(wind) * .1
#计算卡方检验统计量
stats<-sum((count - expected) ^ 2 / expected)
#其近似服从于卡方分布，自由度为10-2-1=7
#计算p值 
pchisq(q = stats, df = 7, lower.tail = FALSE)
#Kolmogorov-Smirnov检验
#作检验
ks.test(unique(wind), y = "pweibull", shape = k, scale = lambda)
