setwd('D:/Workspace/R_Statistics/ch02')
library(ggplot2)
library(dplyr)


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
head(data)dist

# 1. cars 산점도 ggplot2
cars
ggplot(cars, aes(x = speed, y= dist)) +
  geom_jitter(size = 1) +
  ggtitle("Cars dist & speed") + 
  theme(plot.title = element_text(face = 'bold', hjust = 0.5, size= 15, color = 'dark green'))+
  labs(x= "DIST", y="SPEED") + 
  theme(axis.title = element_text(face = 'bold', hjust = 0.5, size= 7.5, color = 'RED'))

# 2. Nail data geom_line

df_Nile <- data.frame(Nile)
year <- c(1871:1970)
df_Nile$year <- year
df_Nile
ggplot(df_Nile, aes(x = year, y = Nile)) + 
  geom_line(color = 'blue') + 
  geom_point(color = 'green') + 
  ggtitle("년도별 Nail강의 수위") + 
  theme(plot.title = element_text(face = 'bold', size = 15, color = 'black', hjust= 0.5)) + 
  theme(axis.title = element_text(size = 12.5, color = 'red', hjust = 0.5)) + 
  labs(x = '연도', y = '수위') 


# 막대그래프
load('data.rda')

t5 <- table(data$V5) ;t5
t6 <- data.frame(t5)
names(t6)
names(t6) <- c('출생아수','빈도')
View(t6)

ggplot(t6, aes(x =출생아수,y= 빈도, fill= 출생아수)) + 
  geom_col(position = 'dodge') +
  ggtitle("출생아(남자)별 빈도") + 
  theme(plot.title = element_text(face = 'bold', size = 15, color = 'black', hjust= 0.5)) + 
  theme(axis.title = element_text(size = 12.5, color = 'black', hjust = 0.5)) 


tableV1.V4 <- table(data$V1, data$V4)
t4 <- data.frame(tableV1.V4) ; t4
names(t4)
names(t4) <- c('성별','학력','빈도')


ggplot(t4, aes(x = 학력, y = 빈도, fill= 성별)) +
  geom_col() +
  ggtitle("학력에 따른 성별 인원수") + 
  theme(plot.title = element_text(face = 'bold', size = 15, color = 'black', hjust= 0.5))

# 히스토그램
h1 <- table(data$V2)
h1 <- data.frame(h1,stringsAsFactors = T)
names(h1) <- c('연령','빈도')
h1$연령 <- as.numeric(h1$연령)
h1$빈도 <- as.numeric(h1$빈도)
str(h1)

ggplot(h1, aes(x= 연령, y= 빈도)) + 
  geom_histogram(bins = 9) +
  scale_x_continuous(breaks = c(seq(0,90,10)))
 
  
breaks = c(seq(0,90,10))

hist(data$V2, main = '연령별 분포', xlab= '연령', ylab= '빈도')
hist(data$V2, breaks = c(seq(0,90,10)), right = F,
     main = '연령별 분포', xlab= '연령', ylab= '빈도')