# Chapter 03 _ 확률

install.packages('prob')
library('prob')

# 1번 동전던지기 경우의 수
tosscoin(1)

# 1번 주사위 던지기  경우의 수
rolldie(1)

# 1~3 두번 뽑기  경우의 수
urnsamples(1:3, size =2)

# 1~3 두번 뽑기  경우의 수(복원 추출), 한번 뽑고 다시 넣음. 
urnsamples(1:3, size =2, replace = T)

# R = 3개, b = 2개 중 2개를 뽑기 경우의 수
urnsamples(c(rep("R",3), rep("B",2)), size =2)

# 동전던지기 2번, makespace = 확률 표시
tosscoin(2, makespace = T)


# 예제 3-2 확률변수의 평균(기댓값)과 분산
x <- c(0,1,2)              # x가 나올 횟수 
px <- c(1/4, 2/4, 1/4)     # x의 확률
EX <- sum(x * px)          # 
EX

x2 <- x ^ 2
x2
EX2 <- sum(x2 * px)       # X제곱의 기댓값
VARX <- EX2 - EX ^ 2 ; VARX     # E(X^2) - E(X)^2
