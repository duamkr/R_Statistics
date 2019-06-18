seq(1,5,2)
seq(from=1,to=5,by=2)    # 보이진 않지만 from, to, by 순의 규칙이 존재,
seq(by=2,to=5,from=1)    # 키워드를 써주면 규칙 순서에 상관없이 출력 가능
seq(2,5,1)               # 키워드를 쓰지 않으면 위처럼 출력 안됨


# seq(from, to, by | length.out)
seq(-3, 3, length.out = 61)     # length.out = from부터 to사이에 생성할 Vector의 개수 지정
seq(-3,3,0.1)

x <- seq(-3,3,0.1)
x[1]
x[1,2,3]     # 여러개의 Vector는 그냥 나열시 출력안됨
x[c(1,2,3)]  # c를 붙여야 함

# factor 질적 자료를 저장하는 자료구조
x <- 1:5
x
factor(x, levels = c(1:4))
factor(x, levels= c(1,2,3,4), labels = c('a','b','c','d'))
factor(x, levels= c(1,2,3,4), labels = c('a','b','c','d'), ordered = TRUE)

week <- c(1:7) ; x1
week <-factor(week, levels = c(1:7), labels = c('일','월','화','수','목','금','토'))
week <-factor(week, levels = c(1:7), labels = c('일','월','화','수','목','금','토'), ordered = TRUE)
week


# Data.frame

name <- c('철수','영희','길동')
age <- c(21, 20, 31)
gender <- factor(c('M','F','M'))
person <- data.frame(name,age,gender)
str(person)

person
person$name
person[1,]
person[,2]
person[3,1]
