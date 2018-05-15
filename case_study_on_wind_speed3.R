########
#���ļ�#
########
library(xlsx)
turbine_data <- read.xlsx(file = "D:/Alfred20151208/Work/work2017Autumn/MathematicalStatisticswithR2017/theFourthTime/TurbineData.xls",
                         sheetName = "Sheet1", header = TRUE)
head(turbine_data)
dim(turbine_data)
###################
#Weibull�ֲ������#
###################
#���룺��״����k��the data
#�����(1/alpha)*sum[(xi^k)*log(xi)]-(1/k)-(1/n)*sum (log(xi)) 
#����alpha= sum(xi^k).
weibull_shape <- function(k, data){
  numer <- sum((data ^ k) * log(data))
  denom <- sum(data ^ k)
  return(numer / denom - 1 / k - mean(log(data)))
}
#���룺��״����k������
#�������(1/n) sum xi^k ��k�η� Ҳ���Ǽ���lambda
#����n�����ݵĳ���
weibull_scale <- function(k, data)
{
  return(mean(data ^ k) ^ (1 / k))
}
wind <- turbine_data$AveSpeed
#uniroot����lower��upper��Ѱ����Ӧ����㡣
k <- uniroot(f = weibull_shape, data = wind, lower = 1,upper = 5)$root
#����k��wind������Ӧ��lambda
lambda <- weibull_scale(k, wind)

#�Ƚ�ֱ��ͼ�����ĸ����ܶȺ�������
#��ֱ��ͼ��prob=TRUE,��ʱ�õ���ֱ��ͼ���Ϊ1
hist(wind, main = "Distribution of average wind speeds",
    xlab = "meters/sec", prob = TRUE)
#����weibull�ֲ����ܶȷֲ�����
curve(dweibull(x, shape=k, scale=lambda), add = TRUE, col = "blue", lwd = 2)
#�ȽϾ���ֲ����������ķֲ�����ͼ
#���µ�ͼ�ν���
dev.new()
#������ʷֲ�����
plot.ecdf(wind,main = "ECDF of wind data")
#Weibull�ֲ��ĸ��ʷֲ�����
curve(pweibull(x, k, lambda), add=TRUE, col="blue",lwd=2)
#�������ÿ����������ֲ���ϼ���
#�����Ŀ�����ж���ϵķֲ�����Щ���ʵ�ʷֲ��Ƿ���ͬ
#���ʱ�� ���ǽ�������ݷ�λ����Ϊʮ��
#ԭ��������ÿ�������еĵ�����Ƶ������������
#���������������ͳ�ƽ̳� ��һ�� ��ʫ�ɡ�������������� 7.4�� �ֲ���ϼ��� ��356ҳ
#���������������ͳ�ƽ̳� �ڶ��� ��ʫ�ɡ�������������� 7.4�� ��Ȼ�ȼ�����ֲ���ϼ���
#�õ���Ӧ��0.1,0.2,...,0.9��λ��
q <- qweibull(seq(.1, .9, by = .1), shape = k, scale = lambda)
#wind�����ֵ����Сֵ
range(wind)
#��ʱqΪֱ��ͼ��ͬ����ķָ���
q <- c(0, q, 14)
#breaks�������ǲ�ͬ����ķָ��
#plot=F�������Ǵ�ʱ���ǲ���ͼ��ֻ������Ӧ��counts,Ҳ����ÿ��С���������еĵ���� 
count <- hist(wind, breaks = q, plot = F)$counts
#ÿ�������������еĵ���
expected <- length(wind) * .1
#���㿨������ͳ����
stats<-sum((count - expected) ^ 2 / expected)
#����Ʒ����ڿ����ֲ������ɶ�Ϊ10-2-1=7
#����pֵ 
pchisq(q = stats, df = 7, lower.tail = FALSE)
#Kolmogorov-Smirnov����
#������
ks.test(unique(wind), y = "pweibull", shape = k, scale = lambda)