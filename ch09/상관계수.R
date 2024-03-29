# Chapter 09. 상관과 회귀
hf <- read.table("http://www.randomservices.org/random/data/Galton.txt", header=T, stringsAsFactors = FALSE)
str(hf)
View(hf)
hf$Gender <- factor(hf$Gender, levels=c("M", "F"))
hf.son <- subset(hf, Gender=="M")
hf.son <- hf.son[c("Father", "Height")]
str(hf.son)

f.mean <- mean(hf.son$Father)
s.mean <- mean(hf.son$Height)


cov.num <- sum((hf.son$Father - f.mean) * (hf.son$Height - s.mean))
(cov.xy <- cov.num / (nrow(hf.son)-1))  
# R 함수를 이용한 공분산(표본)
cov(hf.son$Father, hf.son$Height)

(x.ry <- cov.xy / (sd(hf.son$Father) * sd(hf.son$Height)))
# R 함수를 이용한 상관계수
cor(hf.son$Father, hf.son$Height)
