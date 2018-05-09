#以下试图建立women数据集中weight和height的关系
fit1<-lm(weight~height,data=women)
#建立回归方程
summary(fit1)
#给出相应的回归信息
#包括系数估计，系数的t检验，方程的F检验等
women$weight
#原始的体重值
fitted(fit1)
#估计后的体重值
residuals(fit1)
#原始值-估计值
plot(women$height,women$weight,xlab="Height(in inches)",ylab="Weight(in pounds)",type="p")
#画出原始的点
lines(women$height,fitted(fit1))
#画出估计的直线

#从图形上，可以看出，我们可以用二次多项式去拟合
fit2<-lm(weight~height+I(height^2),data=women)
#建立回归方程
summary(fit2)
#给出相应的回归信息
#包括系数估计，系数的t检验，方程的F检验等
women$weight
#原始的体重值
fitted(fit2)
#估计后的体重值
residuals(fit2)
#原始值-估计值
plot(women$height,women$weight,xlab="Height(in inches)",ylab="Weight(in pounds)",type="p")
#画出原始的点
lines(women$height,fitted(fit2))
#画出估计的直线
anova(fit1, fit2)
#进行统计比较

fit3 <- lm(weight ~ poly(height, 3), data = women)
fit4 <- lm(weight ~ poly(height, 4), data = women)
fit5 <- lm(weight ~ poly(height, 5), data = women)

anova(fit1, fit2, fit3, fit4, fit5)
#故可以选择fit3或者fit4
