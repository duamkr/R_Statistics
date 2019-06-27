# Section 02. 모집단이 세 개 이상
setwd('D:/Workspace/R_Statistics/ch07')
ad <- read.csv('age.data.csv')

str(ad)
ad$score <- ifelse(ad$score == 99, NA, ad$score)
ad$scale <- factor(ad$scale)
ad$sex <- factor(ad$sex)

y1 <- ad$age[ad$scale == "1"]
y2 <- ad$age[ad$scale == "2"]
y3 <- ad$age[ad$scale == "3"]

y1.mean <- mean(y1)
y2.mean <- mean(y2)
y3.mean <- mean(y3)

sse.1 <- sum( (y1 - y1.mean)^2)
sse.2 <- sum( (y2 - y2.mean)^2)
sse.3 <- sum( (y3 - y3.mean)^2)

sse <- sse.1 + sse.2 + sse.3
dfe <- (length(y1)-1) + (length(y2)-1) + (length(y3)-1)

y.mean <- mean(ad$age)

sst.1 <- length(y1) * sum((y1.mean - y.mean)^2)
sst.2 <- length(y2) * sum((y2.mean - y.mean)^2)
sst.3 <- length(y3) * sum((y3.mean - y.mean)^2)

sst <- sst.1 + sst.2+ sst.3

dft <- length(levels(ad$scale)) -1

mst <- sst / dft
mse <- sse / dfe
f.t <- mst / mse

alpha <- 0.05
tol <- qf(1-alpha, 2, 147)     # 95% 지점 값(임계값)

(p.value <- 1-pf(f.t,2,147))

# R을 이용하여 p-value값 구하기

ow <- lm(age ~ scale, data=ad)
anova(ow)

oneway.test(age~scale, data=ad, var.equal = TRUE)
