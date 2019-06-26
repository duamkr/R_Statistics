# 7장 여러 모집단의 평균 비교 검정
# 7-1. 모집단이 두개인 경우
# 정규성 테스트
# 기본 명령 shapiro.test()
library(dplyr)
setwd('D:/Workspace/R_Statistics/ch07')
data <- read.table('chapter7.txt',header =T)
boy <- subset(data, gender ==1)
girl <- subset(data, gender ==2)

var.test(data$weight ~ data$gender)

shapiro.test(boy$weight)      # p-value < 0.05 정규성 없음
shapiro.test(girl$weight)     # p-value > 0.05 정규성 있음
qqnorm(girl$weight)
qqline(girl$weight)     # line기준으로 근처에 분포 되어 있으면 정규성을 가진다고 말함

qqnorm(boy$weight)
qqline(boy$weight)


iriss <- subset(iris, Species == 'setosa')
shapiro.test(iriss$Sepal.Length)     # p-value > 0.05 정규성 있음
qqnorm(iriss$Sepal.Length)
qqline(iriss$Sepal.Length)
 
shapiro.test(iriss$Petal.Width)      # p-value < 0.05 정규성 없음
qqnorm(iriss$Petal.Width)
qqline(iriss$Petal.Width)
       


# 등분산성 테스트
var.test(data$weight ~ data$gender)

t.test(data$weight ~ data$gender, mu = 0, alternative = 'less', 
       var.equal=TRUE)        

# 서로 대응인 두 집단의 평균 차이 검정
install.packages("PairedData")
library(PairedData)
data(Anorexia)
Anorexia

data <- Anorexia
str(data)

install.packages("psych")
library(psych)
summary(Anorexia)
describe(Anorexia)

n <- length(data$Prior - data$Post)
m <- mean(data$Prior - data$Post)
s <- sd(data$Prior - data$Post)
(t.t <- m/(s / sqrt(n)))
alpha <- 0.05
qt(alpha, df=16)
pt(t.t, df=16)

t.test(data$Prior, data$Post, Paired=T, alternative = 'less')




p5<- mpg
p5
p5 %>%
  filter(class%in% c('midsize','subcompact')) %>%
  select(hwy, class)

View(data3)
var.test(data3$Price ~ data1$Origin)
