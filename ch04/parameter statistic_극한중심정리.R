# 4.2 중심극한정리
# 예제 4-2. 정규분포로부터 추출된 표본평균 x의 분포

set.seed(9)
n <- 1000
r.1.mean = rep(NA, n)
r.2.mean = rep(NA, n)

for (i in 1:n) {
  r.1.mean[i] = mean( rnorm(4, mean = 3, sd = 1))      # 평균이 3, 표준편차가 1인 정규분포에서 표본 4개 추출, N(3, 1^2)
  r.2.mean[i] = mean( rnorm(4, mean = 170, sd = 6))    # 평균이 170, 표준편차가 6인 정규분포에서 표본 4개 추출, N(170, 6^2)
}

# 표본 평균들의 분포에서 평균과 표준편차를 구합니다
options(digit = 4)
c(mean(r.1.mean), sd(r.1.mean))
c(mean(r.2.mean), sd(r.2.mean))

par(mfrow=c(1,2))
hist(r.1.mean, prob = TRUE, xlab = '표본평균', ylab = '밀도',
     main = '', col = 'orange', border = 'red')
x1 <- seq(min(r.1.mean), max(r.1.mean), length=1000)
y1 <- dnorm( x = x1, mean = 3, sd = (1/ sqrt(4)))
lines(x1, y1, lty=2, lwd=2, col= 'blue')

hist(r.2.mean, prob = T, xlab = '표본평균', ylab = '밀도',
     main = '', col = 'orange', border = 'red')
x2 <- seq(min(r.2.mean), max(r.2.mean), length = 1000)
y2 <- dnorm(x = x2, mean= 170, sd = (6/sqrt(4)))
lines(x2, y2, lty = 2, lwd = 2, col = 'blue')






# 예제 4-3. 임의의 분포에서 추출된 표본평균 x의 분포
set.seed(9)
t <- 10
p <- 0.1
x <- 0:10
n <- 1000
b.2.mean <- rep(NA, n)  
b.4.mean <- rep(NA, n)
b.32.mean <- rep(NA, n)
b.64.mean <- rep(NA, n)

for (i in 1:n) {
  b.2.mean[i]  <- mean(rbinom(2, size = t, prob = p))
  b.4.mean[i]  <- mean(rbinom(4, size = t, prob = p))
  b.32.mean[i] <- mean(rbinom(32, size = t, prob = p))
  b.64.mean[i] <- mean(rbinom(64, size = t, prob = p))
}

# 표본평균들의 분포에서 평균과 표준편차
options(digit = 4)
c(mean(b.2.mean), sd(b.2.mean))
c(mean(b.4.mean), sd(b.4.mean))
c(mean(b.32.mean), sd(b.32.mean))
c(mean(b.64.mean), sd(b.64.mean))

# B(10, 0,1)의 분산과 표준편차
c(10*0.1*0.9, sqrt(10*0.1*0.9))    # 이항분포의 분산 = Var(X)= np(1-n)

par(mfrow=c(1,3))
hist(b.2.mean, prob = T, xlim = c(0,4), main = '표본크기 : 2',
     col = 'orange', border='red')
x1 <- seq(min(b.2.mean), max(b.2.mean), length=1000)
y1 <- dnorm(x = x1, mean = 1, sd = sqrt(0.9 / sqrt(2)))
lines(x1, y1, lty=2, lwd= 2, col= 'blue')


hist(b.4.mean, prob = T, xlim = c(0,3), main = '표본크기 : 4',
     col = 'orange', border='red')
x2 <- seq(min(b.4.mean), max(b.4.mean), length=1000)
y2 <- dnorm(x = x2, mean = 1, sd = sqrt(0.9) / sqrt(4))
lines(x2, y2, lty=2, lwd= 2, col= 'blue')


hist(b.32.mean, prob = T, xlim = c(0.4,1.7), main = '표본크기 : 32',
     col = 'orange', border='red')
x3 <- seq(min(b.32.mean), max(b.32.mean), length=1000)
y3 <- dnorm(x = x3, mean = 1, sd = sqrt(0.9) / sqrt(32))
lines(x3, y3, lty=2, lwd= 2, col= 'blue')



