library(multcomp)                
table(cholesterol$trt)
#不同治疗方法的实验次数
boxplot(response~trt,data=cholesterol)
#箱线图上可以很明显看出不同
aggregate(cholesterol$response,by=list(cholesterol$trt),FUN=mean)
#计算不同治疗方法的降低数均值
aggregate(cholesterol$response,by=list(cholesterol$trt),FUN=sd)
#计算不同治疗方法的降低数标准差
fit<-aov(response~trt,data=cholesterol)
#作方差分析
summary(fit)
#方差分析结果
TukeyHSD(fit)
#进行不同治疗方法的两两比较
library(gplots)
plotmeans(response~trt,data=cholesterol,xlab="Treatment",ylab="Response",
main="Mean Plot\n with 95% CI")
#画出不同方法降低数的95%置信区间

