# Chapter08. 범주형 자료분석

x <- seq(0, 15, by=0.01)
dc <- dchisq(x, df=3)

alpha <- 0.05
tol <- qchisq(0.95, df=3)

par(mar=c(0,1,1,1))
plot(x, dc, type="l", axes=F, ylim=c(-0.03, 0.25), xlab="", ylab="")
abline(h=0)
tol.g <- round(tol, 2)
polygon(c(tol.g, x[x>tol.g], 15), c(0, dc[x>tol.g], 0), col="red")
text(0, -0.03, "0", cex=0.8)
text(tol, -0.03, expression(chi[0.05]^{2}==2.14), cex=0.8)
polygon(c(0.9254),dc, density = 10 , angle = 135)

#par(mar=c(2, 1, 1, 1))
#plot(x, yf, type="l", ylim=c(-0.1, 1), xlab="", ylab="", axes=F)
#abline(h=0)
#tol.r <- round(tol, 2)
#polygon(c(tol.r, x[x>=tol.r], 4), c(0, yf[x>=tol.r], 0), col="red")
#arrows(tol, 0.3, tol, 0.08, length=0.1)
#text(tol, 0.32, paste("P(F(2, 147) > ", round(tol, 3),")=0.05", sep=""), cex=0.8)
#lines(c(f.t, f.t), c(0,df(f.t, 2, 147)), lty=2)
#arrows(f.t, -0.05, f.t, 0, length=0.05)
#text(f.t, -0.1, paste("F(2, 147)=", round(f.t, 3),sep=""), cex=0.8)



# 멘델의 유전법칙 R을 이용하여 구하기(chisq.test())
x <- c(315,101,108,32)
chisq.test(x, p=c(9,3,3,1) /16)



