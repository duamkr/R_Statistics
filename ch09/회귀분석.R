# 9-2. 회귀 분석
# 예제 9-2 아버지와 아들 키 자료로부터 회귀계수 측정
plot(hf$Height, hf$Father)

hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]
View(hf.son)
mean.x <- mean(hf.son$Father)
mean.y <- mean(hf.son$Height)

sxy <- sum((hf.son$Father - mean.x) * (hf.son$Height - mean.y))
sxx <- sum((hf.son$Father - mean.x)^2)

b1 <- sxy /sxx
b0 <- mean.y - b1 * mean.x     # 아들의 키 = 0.448 * 아버지의 키 + 38.259

# R을 이용한 회귀분석(lm())
out <- lm(Height ~ Father, data=hf.son)
anova(out)
summary(out)

par(mfrow=c(2,2))
plot(out)
# 좋은 선형 모델
# 1. 정규성 - 2번째 그림
# 2. 독립성
# 3. 선형성 - 1번째 그림
# 4. 등분산성 - 3번쨰 그림 

par(mfrow=c(1,1))


# Polynomial Regeression
women
View(women)

# 신장에 따른 몸무게
plot(weight~height, data=women)
fit <- lm(weight~height, data = women)
abline(fit, col='red', lwd=2)

summary(fit)
cor.test(women$weight, women$height)

par(mf)
par(mfrow=c(2,2))
plot(fit)

par(mfrow=c(1,1))

# Polynomial Regeression (2차항 추가)
fit2 <- lm(weight~height + I(height^2), data=women)    # 1차 항으로 부족한 부분 = I(height^2) <- 2차항 적용
plot(weight~height, data=women)
lines(women$height, fitted(fit2), col='green', lwd=2)
summary(fit2)

par(mfrow=c(2,2))
plot(fit2)


# Polynomial Regeression (3차항 추가)
fit3 <- lm(weight~height + I(height^2) + I(height^3), data=women)    # 1차 항으로 부족한 부분 = I(height^2) <- 2차항 적용
plot(weight~height, data=women)
lines(women$height, fitted(fit3), col='green', lwd=2)
summary(fit3)

par(mfrow=c(2,2))
plot(fit3)



# 다중 회귀 분석
state.x77
head(state.x77)
states <- as.data.frame(state.x77[,c("Murder","Population",
                                     "Illiteracy","Income", "Frost")])
fit <- lm(Murder ~ Population+Illiteracy+Income+Frost, data=states)
summary(fit)
par(mfrow = c(2,2))
plot(fit)

fit1 <- lm(Murder ~ . , data=states)     # . <- 나머지 변수 전부다 적용
summary(fit1)
# 결과 값중 Income, Frost는 영향력이 없으므로(*이 한개도 없음) 빼고 적용
fit2 <- lm(Murder ~ Population + Illiteracy, data=states)
summary(fit2)

# AIC(Akaike Information Criterion)
AIC(fit1, fit2) # 값이 적을수록 좋은모델 


# Backward stepwise regreesion, Forward stepwise regression( Backward 로 자동으로 모델을 적용시켜 적용할 변수값을 찾는다 *AIC값 기준)
## Backward 방법
step(fit1, direction = 'backward')
## Forward 방법


# 
install.packages('leaps')
library(leaps)

subsets <- regsubsets(Murder~. , data=states,
                      method = 'seqrep', nbest=4)
subsets <- regsubsets(Murder~. , data=states,
                      method = 'exhaustive', nbest=4)


summary(subsets)
par(mfrow=c(1,1))
plot(subsets)



data3 <- read.csv('http://stats.idre.ucla.edu/stat/data/binary.csv')
View(data3)

data$rank <- as.factor(data$rank)
str(data)

train <- data[1:200,]
test <- data[201:400,]
model <- glm(admit ~ gre + gpa + rank, data =data, family = 'binomial')
summary(model)

model2 <- glm(admit ~ gpa + rank, data =data, family = 'binomial')
summary(model2)

AIC(model, model2)
