#产生1000组随机数，每组100个，服从N(25,6^2)
mat1 <- matrix(data = 0, ncol = 2, nrow = 1000) #记录1000个估计区间的左端点与右端点。
count <- 0                                      #记录所有的包含均值25的区间个数
plot(x = c(20, 30), y = c(1, 100), type = "n", xlab = "",ylab = "")
#n代表no plotting
for(i in 1:1000){
      x <- rnorm(n = 100, mean = 25, sd = 6)
      #计算相应的置信区间
      mean1 <- mean(x)
      par1 <- qnorm(p = 0.975) * 6 / 10
      mat1[i, ] <- c(mean1 - par1, mean1 + par1)
      if((mean1 - par1) < 25 && (mean1 + par1) > 25){
          count <- count + 1
      }
      if(i <= 100){
            segments(x0 = mat1[i, 1], y0 = i, x1 = mat1[i, 2], y1 = i) #画线段
      }
}
abline(v = 25,col = "red")   #在x=25处画一条垂直的线
count / 1000               #看一下其中包含均值的线段，所占比例多大