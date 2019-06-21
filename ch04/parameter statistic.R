# 4장. 표본분포 

# 예제 4-1. 표본 평균 x의 분포
m10 <- rep(NA, 1000)      # 결측치 생성 
m40 <- rep(NA, 1000)

set.seed(9)
for(i in 1:1000) {         
  m10[i] <- mean(rnorm(10))     # m10[i]에 10개의 랜덤 숫자의 평균을 대입 (rnorm(10)= 표준정규분포)
  m40[i] <- mean(rnorm(40))     # m40[i]에 10개의 랜덤 숫자의 평균을 대입 (rnorm(40)= 표준정규분포) 
}
m10
options(digits = 4)
c(mean(m10), sd(m10))
c(mean(m40), sd(m40))

par(mfrow=c(1,2))
hist(m10, xlim=c(-1.5,1.5), main = "", xlab = 'x', ylab = '',
     col = 'cyan', border = 'blue')
hist(m40, xlim=c(-1.5,1.5), main = "", xlab = 'x', ylab = '',
     col = 'cyan', border = 'blue')

par(mfrow=c(1,1))

