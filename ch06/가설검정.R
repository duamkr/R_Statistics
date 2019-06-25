library(stringr)
setwd('D:/Workspace/R_Statistics/ch06')

data <- read.csv('2016.6th.csv')
tmp <- subset(data, data$나이==7)
height.p= tmp$X104.키

set.seed(9)
height = height.p[sample(length(height.p), 15)]
height

mean(height)
sd(height)
t.test(height, mu=1220)

# Section02 - 단일모집단의 가설검정(1-sample T)

## 예제 6-1. 단일 모집단의 평균검정
data <- read.table('http://jse.amstat.org/datasets/babyboom.dat.txt')
str(data)
names(data) <- c('time','gender','weight','minutes')
tmp <- subset(data, gender ==1)
weight <- tmp[[3]]

barx <- mean(weight)
s <- sd(weight)
n <- length(weight)
h0 <- 2800
(t.t<- (barx - h0) / (s / sqrt(n)))

alpha <- 0.05
(c.u <- qt(1-alpha, df=n-1))
(p.value <- 1 - pt(t.t, df= n-1))
t.test(weight, mu=2800, alternative = "greater")

#  도표작성 : 그림 6-8
par(mar=c(0,1,1,1))
x <- seq(-3, 3, by=0.001)
y <- dt(x, df=14)
plot(x, y, type="l", axes=F, ylim=c(-0.02, 0.38), main="", xlab="t", ylab="")
abline(h=0)

polygon(c(c.u, x[x > c.u],3), c(0, y[x > c.u], 0), col = 2)
text(c.u, -0.02, expression(t[0.05] == 1.74))       # x축 1.74 라벨
text(1.8, 0.2, expression(alpha == 0.05), cex = 0.8) # 곡선 a = 0.05라벨
arrows(1.8, 0.18, 1.8, 0.09, length = 0.05) # 화살표

polygon(c(t.t, x[x > t.t],3 ), c(0, y[x > t.t], 0), density = 20, angle = 45)    # x축 빗금친 부분
text(t.t, -0.02, paste("t=", round(t.t, 3)), pos=4)
text(2.65, 0.1, expression(plation(P)(T>2.233) == 0.0196), cex= 0.8)
arrows(2.7, 0.08, 2.5, 0.03, length = 0.05)


# 예제 6-2. 모비율 검정: 야구공의 불량률 검정
tmp <- read.table('D:/Workspace/R_Statistics/ch06/restitution.txt', header = T)
rel <- ifelse(tmp$rst < 0.4134 | tmp$rst > 0.4374, 1, 0)

n <- length(rel)
nos <- sum(rel)
sp <- nos / n
hp <- 0.1
(z <- (sp - hp) / sqrt ( (hp*(1-hp))/n))

alpha <- 0.05
(c.u <- qnorm(1-alpha))
( p.value <- 1-pnorm(z))

prop.test(nos, n, p=0.1, alternative = 'greater', correct = FALSE)


# 7장을 위한 준비 - 데이터 프레임 다루기와 데이터 정제하기
data <- read.table('http://jse.amstat.org/datasets/babyboom.dat.txt')

nrow(data)     # 행 개수
ncol(data)     # 열 개수


str(data)      # 데이터 구조 확인
head(data)     # 데이터의 앞부분 확인 (일부)
head(data, n=2)# 앞에서 2개까지의 데이터 확인
tail(data)     # 데이터의 뒷부분 확인 (일부)
tail(data, n=2)# 뒤에서 두개까지의 데이터 확인  

names(data)    # 데이터의 컬럼명 확인
names(data) <- c("time", "gender", "weight", "minutes")    # 데이터 컬럼명 변경

row.names(data)    # 행번호 확인
g1 <- data$gender # data의 gender컬럼의 값 g1에 할당(벡터저장)
str(g1)
g2 <- data[,2]    # data의 2번째 열 값 g2에 할당(벡터저장)
str(g2)
g3 <- data["gender"]     # data의 gender 컬럼값을 data.frame으로 저장
str(g3)
# g1, g2, g3는 data의 gender값을 담지만, g1,g2는 벡터로 저장, g3는 dataframe으로 저장
g4 <- data[[2]]
str(g4)





