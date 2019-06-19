# 2. 모수와 통계량
setwd('D:/Workspace/R_Statistics/ch02')

# 최대값과 최솟값
ranicafe <- read.csv('cafedata.csv', stringsAsFactors = F)
str(ranicafe)
head(ranicafe)
summary(ranicafe)
dim(ranicafe)

ranicafe$Coffees <- as.numeric(ranicafe$Coffees)
sort(ranicafe$Coffees)
sort(ranicafe$Coffees)[1]
sort(ranicafe$Coffees, decreasing=TRUE)
sort(ranicafe$Coffees, decreasing=TRUE)[1]
min(ranicafe$Coffees, na.rm=T)
max(ranicafe$Coffees, na.rm=T)

# 최빈값

stem(ranicafe$Coffees)
hist(ranicafe$Coffees)

# 평균값과 중앙값
## 평균값
rc <- ranicafe$Coffees
weight <- (1 / length(rc) - 1) ;      # na 값이 1개가 있기 때문에 length를 계산할때  -1
sum(rc * weight, na.rm = T)
mean(rc, na.rm=T)

rc[rc == max(rc, na.rm=T)] <- 480     # rc 값중 최대값을 480으로 바꿔라, 특정 값 변경 
mean(rc, na.rm=T)

## 중앙값 구하기

median.idx <- (1 + length(rc) -1) / 2
sort(rc)[median.idx]
median(rc, na.rm = T)

# 표준편차와 사분위수 범위

height <- c(164,166,168,170,172,174,176)
height.m <- mean(height) ; height.m
height.dev <- height - height.m ; height.dev
sum(height.dev) 
height.dev2 <- height.dev ^ 2
sum(height.dev2)
variance <- sum(height.dev2) / length(height)
variance
standard_deviation <- sqrt(variance)
standard_deviation

# 위에서 구한 식의 값과 아래 명령어에 따른 값이 다름
mean(height)
var(height)
sd(height)


#사분위수 구하기 (나열된 값을 4등분 한 값의 위치에 해당 값 25%, 50%, 75%, 100%의 위치)
quantile(rc, na.rm = T)
qs <- quantile(rc, na.rm = T)
qs 
qs[4] - qs[2]          # 3분위수 - 1분위수 = IQR(InterQuantile Range)
IQR(rc, na.rm = T)
bp <- boxplot(rc, main = ' 커피 판매량에 대한 상자도표', axes = F)

# 이상치(Outlier)
boxplot(cars$dist)
qs <- quantile(cars$dist)
qs
iqr <- qs[4] - qs[2]
iqr
upperLimit <- qs[4] + 1.5 * iqr
lowerLimit <- qs[2] - 1.5 * iqr
lowerLimit; upperLimit
cars$dist[cars$dist > upperLimit]
cars$dist[cars$dist < lowerLimit]
