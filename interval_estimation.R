#����1000���������ÿ��100��������N(25,6^2)
mat1 <- matrix(data = 0, ncol = 2, nrow = 1000) #��¼1000�������������˵����Ҷ˵㡣
count <- 0                                      #��¼���еİ�����ֵ25���������
plot(x = c(20, 30), y = c(1, 100), type = "n", xlab = "",ylab = "")
#n����no plotting
for(i in 1:1000){
      x <- rnorm(n = 100, mean = 25, sd = 6)
      #������Ӧ����������
      mean1 <- mean(x)
      par1 <- qnorm(p = 0.975) * 6 / 10
      mat1[i, ] <- c(mean1 - par1, mean1 + par1)
      if((mean1 - par1) < 25 && (mean1 + par1) > 25){
          count <- count + 1
      }
      if(i <= 100){
            segments(x0 = mat1[i, 1], y0 = i, x1 = mat1[i, 2], y1 = i) #���߶�
      }
}
abline(v = 25,col = "red")   #��x=25����һ����ֱ����
count / 1000               #��һ�����а�����ֵ���߶Σ���ռ�������