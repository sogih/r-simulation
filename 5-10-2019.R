#감마분포의 이해 1: Gamma(a,1)의 분포
x <- seq(0,25,length=200)
y <- dgamma(x,shape=4,scale=1)
plot(x,y,type="l", ylim=c(0,0.25), lwd=2, ylab="density", main="Gamma(a,1)")
text(x=6, y=0.21, "a=4, scale=1")


curve(dgamma(x,shape=5,scale=1), lwd=2, col="red", add=T)
text(x=8, y=0.16, "a=5, scale=1", col="red")



curve(dgamma(x,shape=9,scale=1), lwd=2, col="blue", add=T)
text(x=15, y=0.07, "a=9, scale=1", col="blue")
  #shape 모양, scale 퍼진 정도



# 감마분포의 이해2: Gamma(shape=a, scale=10)
par(mfrow=c(2,2))
x <- seq(0,150,length=200)
curve(dgamma(x,1,scale=10),0,150, ylim=c(0,0.1), ylab="density", main="Gamma(1,10)")

x <- seq(0,150,length=200)
curve(dgamma(x,5,scale=10),0,150, ylim=c(0,0.022), ylab="density", main="Gamma(5,10)")


x <- seq(20,220,length=200)
curve(dgamma(x,10,scale=10),20,220, ylim=c(0,0.015), ylab="density", main="Gamma(10,10)")


x <- seq(80,400,length=200)
curve(dgamma(x,20,scale=10),30,400, ylim=c(0,0.01), ylab="density", main="Gamma(20,10)")


# 감마분포의 이해3: Gamma(shape=2, scale=b)

par(mfrow=c(2,2))

curve(dgamma(x,2,scale=1),0,10, ylim=c(0,0.4), ylab="density", main="Gamma(2,1)")

curve(dgamma(x,2,scale=5),0,40, ylim=c(0,0.08), ylab="density", main="Gamma(2,5)")

curve(dgamma(x,2,scale=10),0,70, ylim=c(0,0.04), ylab="density", main="Gamma(2,10)")

curve(dgamma(x,2,scale=120),0,1000, ylim=c(0,0.01), ylab="density", main="Gamma(2,120)")


#이해3. 한화면에
par(mfrow=c(1,1))

x <- seq(0,25,length=200)
y <- dgamma(x,shape=2,scale=1)
plot(x,y,type="l", ylim=c(0,0.4), lwd=2, ylab="density", main="Gamma(a,1)")
text(x=4, y=0.40, "a=2, scale=1")

curve(dgamma(x,shape=2,scale=5), lwd=2, col="red", add=T)
text(x=8, y=0.09, "a=2, scale=5", col="red")

curve(dgamma(x,shape=2,scale=10), lwd=2, col="blue", add=T)
text(x=15, y=0.05, "a=2, scale=10", col="blue")

curve(dgamma(x,shape=2,scale=120), lwd=2, col="green", add=T)
text(x=15, y=0.008, "a=2, scale=120", col="green")


#카이제곱분포 gamma(shape=k/2, scale=2)=chisq(k)
x <- seq(0,30,length=200)
y <- dchisq(x,1)
plot(x,y,type="l", ylim=c(0,0.5), lwd=2, ylab="density", main="df=k, chisp")
text(x=3, y=0.40, "df1")

curve(dchisq(x,5), lwd=2, col="red", add=T)
text(x=5, y=0.16, "df5", col="red")

curve(dchisq(x,10), lwd=2, col="blue", add=T)
text(x=13, y=0.1, "df10")

curve(dchisq(x,20), lwd=2, col="green", add=T)
text(x=27, y=0.05, "df20", col="green")


#f(5,df2)
par(mfrow=c(2,2))

x <- seq(0,15,length=200)
curve(df(x,df1=5,df2=5),0,10, ylim=c(0,0.75), ylab="density", main="F(5,5)")
curve(df(x,df1=5,df2=10),0,10, ylim=c(0,0.75), ylab="density", main="F(5,10)")
curve(df(x,df1=5,df2=20),0,10, ylim=c(0,0.75), ylab="density", main="F(5,20)")
curve(df(x,df1=5,df2=50),0,10, ylim=c(0,0.75), ylab="density", main="F(5,50)")

#f(df1,5)
par(mfrow=c(2,2))
x <- seq(0,15,length=200)
curve(df(x,df1=5,df2=5),0,10, ylim=c(0,0.75), ylab="density", main="F(5,5)")
curve(df(x,df1=10,df2=5),0,10, ylim=c(0,0.75), ylab="density", main="F(10,5)")
curve(df(x,df1=20,df2=5),0,10, ylim=c(0,0.75), ylab="density", main="F(20,5)")
curve(df(x,df1=50,df2=5),0,10, ylim=c(0,0.75), ylab="density", main="F(50,5)")

#f(df1,df2)
par(mfrow=c(2,2))
x <- seq(0,15,length=200)
curve(df(x,df1=5,df2=5),0,2, ylim=c(0,1.5), ylab="density", main="F(5,5)")
curve(df(x,df1=10,df2=10),0,2, ylim=c(0,1.5), ylab="density", main="F(10,10)")
curve(df(x,df1=20,df2=20),0,2, ylim=c(0,1.5), ylab="density", main="F(20,20)")
curve(df(x,df1=50,df2=50),0,2, ylim=c(0,1.5), ylab="density", main="F(50,50)")

#베타분포
par(mfrow=c(1,1))
curve(dbeta(x,shape1=2,shape2=2),0,1, ylim=c(0,8.5), lwd=2, type="l", main="beta(shape1=2,shape2) fixed shape1=2")
text(x=0.5, y=1.8, "2,2")
curve(dbeta(x,shape1=2,shape2=5),0,1, ylim=c(0,8.5), add=T, col="red", lwd=2, type="l")
text(x=0.2, y=2.75, "2,5", col="red")
curve(dbeta(x,shape1=2,shape2=10),0,1, ylim=c(0,8.5), add=T, col="blue", lwd=2, type="l")
text(x=0.1, y=4.5, "2,10", col="blue")
curve(dbeta(x,shape1=2,shape2=20),0,1, ylim=c(0,8.5), add=T, col="green", lwd=2, type="l")
text(x=0.05, y=8.15, "2,20", col="green")

par(mfrow=c(1,1))
curve(dbeta(x,shape1=2,shape2=2),0,1, ylim=c(0,8.5), lwd=2, type="l", main="beta(shape1,shape2=2) fixed shape2=2")
text(x=0.5, y=1.8, "2,2")
curve(dbeta(x,shape1=2,shape2=5),0,1, ylim=c(0,8.5), add=T, col="red", lwd=2, type="l")
text(x=0.2, y=2.75, "5,2", col="red")
curve(dbeta(x,shape1=2,shape2=10),0,1, ylim=c(0,8.5), add=T, col="blue", lwd=2, type="l")
text(x=0.1, y=4.5, "10,2", col="blue")
curve(dbeta(x,shape1=2,shape2=20),0,1, ylim=c(0,8.5), add=T, col="green", lwd=2, type="l")
text(x=0.05, y=8.15, "20,2", col="green")


par(mfrow=c(1,1))
curve(dbeta(x,shape1=2,shape2=2),0,1, ylim=c(0,5.5), lwd=2, type="l", main="beta(shape1,shape2=2)")
text(x=0.5, y=1.5, "2,2")
curve(dbeta(x,shape1=5,shape2=5),0,1, ylim=c(0,5.5), add=T, col="red", lwd=2, type="l")
text(x=0.5, y=2.5, "5,5", col="red")
curve(dbeta(x,shape1=10,shape2=10),0,1, ylim=c(0,5.5), add=T, col="blue", lwd=2, type="l")
text(x=0.5, y=3.5, "10,10", col="blue")
curve(dbeta(x,shape1=20,shape2=20),0,1, ylim=c(0,5.5), add=T, col="green", lwd=2, type="l")
text(x=0.5, y=5, "20,20", col="green")


#포아송분포의 형태
par(mfrow=c(2,2))
x <- 1:10
plot(x, dpois(x,lambda=1), prob=TRUE, type="h", lwd=5, ylab="probability", main="P(1)")

x <- 1:15
plot(x, dpois(x,lambda=5), prob=TRUE, type="h", lwd=5, ylab="probability", main="P(5)")

x <- 1:20
plot(x, dpois(x,lambda=10), prob=TRUE, type="h", lwd=5, ylab="probability", main="P(10)")

x <- 1:40
plot(x, dpois(x,lambda=20), prob=TRUE, type="h", lwd=5, ylab="probability", main="P(20)")


#정규성검정 // 정규분포에서 난수 발생으로 표본을 얻은 경우 히스토그램 그려보기
par(mfrow=c(2,2))
x <- seq(6.5, 13.5, by=0.01)

y1 <- rnorm(50, mean=10, sd=1)
hist(y1, prob=TRUE, xlim=c(6.5, 13.5), ylim=c(0, 0.5), xlab=paste("shapiro pv is = ",round(shapiro.test(y1)$p.value,3)))
lines(x, dnorm(x, 10, 1))


y2 <- rnorm(100, mean=10, sd=1)
hist(y2, prob=TRUE, xlim=c(6.5, 13.5), ylim=c(0, 0.5), xlab=paste("shapiro pv is = ",round(shapiro.test(y2)$p.value,3)))
lines(x, dnorm(x, 10, 1))


y3 <- rnorm(200, mean=10, sd=1)
hist(y3, prob=TRUE, xlim=c(6.5, 13.5), ylim=c(0, 0.5), xlab=paste("shapiro pv is = ",round(shapiro.test(y3)$p.value,3)))
lines(x, dnorm(x, 10, 1))

y4 <- rnorm(500, mean=10, sd=1)
hist(y4, prob=TRUE, xlim=c(6.5, 13.5), ylim=c(0, 0.5), xlab=paste("shapiro pv is = ",round(shapiro.test(y4)$p.value,3)))
lines(x, dnorm(x, 10, 1))



#shaprio.test() .. 정규성 검정 함수
  #p가 크면 귀무가설 채택하여 정규성 만족한다
