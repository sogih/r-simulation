help(quantile)

#정규성검정에서 qqline의 이해
set.seed(20170519)
x <- sort(rnorm(40,100,15))
  # X~N(100,15^2)에서 40개의 난수 발생
#p <- (1:40 - 0.5)/40 type5의p
p = (1:length(x) - 1)/(length(x) - 1) #type7
q <- qnorm(p,0,1)

qq <- qnorm(c(0.25,0.75),0,1) #N(0,1)의 제1,3 사분위수
xq <- quantile(x, c(0.25, 0.75)) #x

qqnorm(x) #normal q-q plot
qqline(x, col=2) # 1st and 3rd quantiles from normal
lines(qq, xq, col="green", lwd=2)

#
par(mfrow=c(2,1))
curve(dnorm(x,100,25), from=50, to=150, ylim=c(0,0.03), ylab="density", main="N(100,225)")
set.seed(20170519)
x <- rnorm(40,100,25)
qqnorm(x)
qqline(x, col = "green", lwd=2)
  #qqline = (Q3-Q1)/(z0.25-z0.75)를 기울기로하는 직선
abline(v=c(0,1), h=c(100,125), lty="dotted")

p <- (1:40-1)/(length(x)-1)
zq <- qnorm(p) #x축
xq <- 100 + 25 * zq #y축
lines(zq, xq, col=2, lwd=2)
  #sigma를 기울기로하는 직선

#
par(mfrow=c(2,1))

  #그림1-pdf
curve(dnorm(x,70,15),from=0,to=200,ylim=c(0,0.03),ylab="density")
text(x=70, y=0.028, "N(70,225)")
par(new=T)  # 겹쳐그리기
curve(dnorm(x,130,15), from=0, to=200, ylim=c(0,0.03), ylab="density", col="red")
text(x=130, y=0.028, "N(130,225)", col="red")

  #그림2-qq plot
x1 <- rnorm(50,70,15)
x2 <- rnorm(50,130,15)
qqnorm(c(x1,x2))  #dot, 쌍봉그래프
qqline(c(x1,x2), col = 2, lwd = 2)  #line


#
f <-
