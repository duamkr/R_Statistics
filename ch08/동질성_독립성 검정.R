# 8-2. 동질성 검정과 독립성 검정
# 예제 -2. 연령대별 SNS 이용률의 동질성 검토

sns.c <- read.csv('snsbyage.csv',header = T, stringsAsFactors = FALSE)
str(sns.c)
# 검정통계량으로 구하기
sns.c <- transform(sns.c, age.c = 
                     factor(age, levels=c(1, 2, 3), 
                            labels=c("20대", "30대", "40대")))

sns.c <- transform(sns.c, service.c = 
                     factor(service, levels=c("F", "T", "K", "C", "E"), 
                            ordered=TRUE))
c.tab <- table(sns.c$age.c, sns.c$service.c)

(a.n <- margin.table(c.tab, margin=1))
(s.n <- margin.table(c.tab, margin=2))
(s.p <- s.n / margin.table(c.tab))
(expected <- a.n %*% t(s.p))    

(o.e <- c.tab-expected)
(t.t <- sum(  (o.e)^2 / expected ))     # 검정 통계량
qchisq(0.95, df=8)
1-pchisq(t.t, df=8)         # p-value

# R에서 구하기
chisq.test(c.tab)

# x.test를 통해 더 많은 정보 
result <- chisq.test(c.tab)
names(result)
result$expected
str(result)
result$p.value

addmargins(chisq.test(c.tab)$expected)


# 예제-3. 성별에 따른 대학원 입학 여부의 독립성 검정
data(UCBAdmissions)
ucba.tab <- apply(UCBAdmissions, c(1,2), sum)     # c(1,2) 열과 행으로 더해서 sum으로 저장해라
ucba.tab
round(prop.table(ucba.tab, margin=2) * 100, 1)

# 독립성 검정 
(a.n <- margin.table(ucba.tab, margin=1))
(g.n <- margin.table(ucba.tab, margin=2))

(a.p <- a.n / margin.table(ucba.tab))
(g.p <- g.n / margin.table(ucba.tab))

(expected <- margin.table(ucba.tab) * (a.p %*% t(g.p)))
addmargins( expected )

# chisq square statistic

o.e <- (ucba.tab - expected)^2 / expected
addmargins(o.e)

chisq.t <- sum(o.e)
chisq.t
qchisq(0.95, df=1)
1-pchisq(112.250, df=1)
1-pchisq(chisq.t, df=1)


chisq.test(ucba.tab)

