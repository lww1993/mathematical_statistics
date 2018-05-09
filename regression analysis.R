#������ͼ����women���ݼ���weight��height�Ĺ�ϵ
fit1<-lm(weight~height,data=women)
#�����ع鷽��
summary(fit1)
#������Ӧ�Ļع���Ϣ
#����ϵ�����ƣ�ϵ����t���飬���̵�F�����
women$weight
#ԭʼ������ֵ
fitted(fit1)
#���ƺ������ֵ
residuals(fit1)
#ԭʼֵ-����ֵ
plot(women$height,women$weight,xlab="Height(in inches)",ylab="Weight(in pounds)",type="p")
#����ԭʼ�ĵ�
lines(women$height,fitted(fit1))
#�������Ƶ�ֱ��

#��ͼ���ϣ����Կ��������ǿ����ö��ζ���ʽȥ���
fit2<-lm(weight~height+I(height^2),data=women)
#�����ع鷽��
summary(fit2)
#������Ӧ�Ļع���Ϣ
#����ϵ�����ƣ�ϵ����t���飬���̵�F�����
women$weight
#ԭʼ������ֵ
fitted(fit2)
#���ƺ������ֵ
residuals(fit2)
#ԭʼֵ-����ֵ
plot(women$height,women$weight,xlab="Height(in inches)",ylab="Weight(in pounds)",type="p")
#����ԭʼ�ĵ�
lines(women$height,fitted(fit2))
#�������Ƶ�ֱ��
anova(fit1, fit2)
#����ͳ�ƱȽ�

fit3 <- lm(weight ~ poly(height, 3), data = women)
fit4 <- lm(weight ~ poly(height, 4), data = women)
fit5 <- lm(weight ~ poly(height, 5), data = women)

anova(fit1, fit2, fit3, fit4, fit5)
#�ʿ���ѡ��fit3����fit4