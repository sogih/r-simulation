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
x1 <- rnorm(100,70,15)
x2 <- rnorm(100,130,15)
qqnorm(c(x1,x2))  #dot, 쌍봉그래프
qqline(c(x1,x2), col = 2, lwd = 2)  #line
shapiro.test(c(x1, x2))$p.value


#
par(mfrow=c(2,1))
curve(dnorm(x,100,15), from=20, to=180, ylim=c(0,0.03), ylab="density", main="N(100,225)")

par(new=T)
x <- c(25, 175)
y <- c(0,0)
plot(y ~ x, xlim=c(20,180), ylim=c(0,0.03), cex=1, pch=19)

x2 <- rnorm(38,100,15)
qqnorm(c(x,x2))
qqline(c(x,x2), col=2)


# U(80,120)
par(mfrow=c(1,2))
f <- function(x){ return(1/40*((80<x)&(x<120)))}
curve(f, from=70, to=130, ylim=c(0,0.03), ylab="density", lty="dotted", main="U(80,120)")
  # 80에서 120까지 균일분포 도트로

x <- runif(100,80,120)
qqnorm(x)
qqline(x, col=2)

#이중 지수 분포 X~DE(0,1)
f <- function(x){ 1/2*exp(-1*abs(x))}
  # f <=> 이중 지수 분포
curve(f, from=-4, to=4, ylab="density", main="DE(0,1)")
rdexp <- function(n) {
  u <- runif(n,0,1)
  x <- rep(NA, n)
  for (i in 1:n) {
    if (u[i] <= 0.5) x[i] <- log(2*u[i])
    else x[i] <- -log(2*(1-u[i]))
  }
  return(x)
}
x <- rdexp(100)
qqnorm(x)
qqline(x, col=2)
shapiro.test(x)$p.value

# X~Lognormal(5,1)
f <- function(x,m,s2){
  1/(x*sqrt(2*pi*s2))*exp(-(log(x)-m)^2/(2*s2))}
curve(f(x,5,1), from=0, to=1500, ylab="density", main="Lognormal(N(5,1))")
x <- exp(rnorm(50,5,1))
qqnorm(x)
qqline(x, col=2)
  # what is lognormal dist ?
  # Y = lnX (X는 항상 양수)
  # Y는 정규분포를 가진다
  # 이때 X ~ lognormal dist

#
# X~Lognormal(5,1) -> 1500-x
f <- function(x,m,s2){
  1/(x*sqrt(2*pi*s2))*exp(-(log(x)-m)^2/(2*s2))}
curve(f(1500-x,5,1), from=0, to=1500, ylab="density", main="Lognormal(N(5,1))")

x <- 1500 - exp(rnorm(40,5,1))
qqnorm(x)
qqline(x, col=2)
shapiro.test(x)$p.value
#


