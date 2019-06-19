# 2장 
setwd('D:/Workspace/R_Statistics/ch02')

data <- read.csv('2010년 인구사항.csv', header= F, na.strings = c('.'))
str(data)
data$V1 <- factor(data$V1, levels=c(1,2),
                                   labels = c("남자","여자"))
data$V3 <- factor(data$V3, levels = 1:14,
                  labels = c('가구주','가구주의 배우자','자녀','자녀의 배우자','가구주의 부모',
                             '배우자의 부모','손자녀, 그 배우자','증손자녀, 그 배우자','조부모','형제자매, 그 배우자',
                             '형재자매의 자녀, 그 배우자','부모의 형제자매, 그 배우자','기타 친인척','그외같이 사는사람'))
data$V4 <- factor(data$V4, levels = 1:8,
                  labels= c('안 받았음','초등학교','중학교','고등학교','대학-4년제 미만','대학-4년제 이상','석사과정','박사과정'))
str(data)
head(data)

# 1. 그래프

library(ggplot2)
cars
str(cars)
# jitter() 산점도의 같은값(point 중첩현상)을 표시해줌
#par(mfrow=c(1,2))
plot(cars$speed, cars$dist,
     main = '속도와 제동거리', xlab= '속도(mph)', ylab = '제동거리(ft)',
     pch = 1, col = 'red')    #pch = 포인트의 모양, plot의 기본 틀 기억

plot(jitter(cars$speed), jitter(cars$dist),
     main = '속도와 제동거리', xlab= '속도(mph)', ylab = '제동거리(ft)',
     pch = 1, col = 'red') 


str(Nile)
plot(Nile, main = "Nile강의 연도별 유량 변화", xlab = '연도', ylab = '유량')
plot(Nile, type = 'p', main = "Nile강의 연도별 유량 변화", xlab = '연도', ylab = '유량')     # type = p , 점
plot(Nile, type = 'l', main = "Nile강의 연도별 유량 변화", xlab = '연도', ylab = '유량')     # type = l , 선
plot(Nile, type = 'b', main = "Nile강의 연도별 유량 변화", xlab = '연도', ylab = '유량')     # type = b , 점 + 선

plot.ts(Nile)


# ggplot으로 그래프 그리기( Nile의 data type이 'Time-Series', ggplot은 data.frame data type이여야 하므로 변환과정)
## Time-Series 데이터 프레임으로 변경하기 1
df_Nile <- data.frame(Nile)
year <- c(1871:1970)
df_Nile$year <- year
df_Nile
ggplot(df_Nile, aes(x = year, y = Nile)) + 
  geom_line()

# Time-Series 데이터 프레임으로 변경하기 2
df_Nile2 <- data.frame(date = time(Nile),        # Time-Series는 date단위이기 때문에, time(Nile)로 date칼럼이 자동으로 년도로 들어감
                       flood = as.matrix(Nile))
View(df_Nile2)
ggplot(df_Nile2, aes(x = year, y = flood)) + 
  geom_line()


# 막대그래프
load('data.rda')
tableV5 <- table(data$V5) ;tableV5
View(tableV5)
barplot(tableV5, main = '출생아(남자)별 빈도', xlab = '출생아수', ylab = '빈도')

tableV1.V4 <- table(data$V1, data$V4)
tableV1.V4
barplot(tableV1.V4, legend.text = T, col = c('orange','green'),
        main = '학력에 따른 성별 인원수', xlab = '연령', ylab= '빈도')

# 히스토그램

hist(data$V2, main = '연령별 분포', xlab= '연령', ylab= '빈도')
hist(data$V2, breaks = c(seq(0,90,10)), right = F,
     main = '연령별 분포', xlab= '연령', ylab= '빈도')
## 히스토 그램에 probability = T , 밀도 분포
hist(data$V2, probability= T, breaks = c(seq(0,90,10)), right = F,
     main = '연령별 분포', xlab= '연령', ylab= '밀도')



# 원 도표
pie(table(data$V4), main ='학력수준별 비중', cex = 0.8)
table(data$V4)
