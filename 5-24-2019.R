# 이항분포의 정규분포 근사
m <- 100
p <- 0.5
try.n <- c(5, 10, 15, 30)
par(mfrow=c(2,2)) 
for(i in 1:4) {
  n = try.n[i]
  res = rbinom(m,n,p)  # n,p인 이항분포의 모집단에서 포본m개 추출
  hist(res, prob=TRUE, xlab=paste("n =",n))
  curve(dnorm(x, n*p, sqrt(n*p*(1-p))), add=TRUE, col=i, lwd=2)
}

# 이항분포의 정규분포 근사 - 함수문을 사용해서
binom.approx.to.normal <- function(m, n, p, col) {
  res = rbinom(m,n,p)
  hist(res, prob=TRUE, xlab=paste("m = ", m), main=paste("Hist of B(n =",n,", p =",p,")"))
  curve(dnorm(x, n*p, sqrt(n*p*(1-p))), add=TRUE, lwd=2, col=col)
}

  # fixed m, n, variable p
binom.approx.to.normal(100, 50, 0.3, 1)
binom.approx.to.normal(100, 50, 0.5, 2)
binom.approx.to.normal(100, 50, 0.6, 3)
binom.approx.to.normal(100, 50, 0.75, 4)

  # fixed n, p, variable m
binom.approx.to.normal(50, 50, 0.5, 1)
binom.approx.to.normal(200, 50, 0.5, 2)
binom.approx.to.normal(500, 50, 0.5, 3)
binom.approx.to.normal(1000, 50, 0.5, 4)


# 이항분포의 정규근사 B(10,0.7): 중심극한정리
meanx <- function(nreps,k,n,p) {
  meanx = numeric(nreps)
  
  for(i in 1:nreps){
    x <- rbinom(k,n,p)
    meanx[i] <- mean(x)
  }
  return (meanx)
}

  # 모집단의 분포
x = 0:10
p = 0.7
prob = dbinom(x,10,p)
plot(x,prob,type="h", lwd=5, ylim=c(0,0.3), ylab="f(x)", main="B(10,0.7)")

k = 2
xbar = meanx(k, nreps=10000, n=10, p=0.7)
hist(xbar, freq=F, xlim=c(2,11), ylim=c(0,0.5), xlab=paste("sample size=",k), col=gray(0.8), breaks=seq(0,10,0.5)) # 막대사이 간격 0.5
curve(dnorm(x,7,sqrt(2.1/k)), col="blue", lwd=2, add=TRUE)

k = 10
xbar = meanx(k, nreps=10000, n=10, p=0.7)
hist(xbar, freq=F, xlim=c(5,9), ylim=c(0,1), xlab=paste("sample size=",k), col=gray(0.8), breaks=seq(0,10,0.5)) # 막대사이 간격 0.5
curve(dnorm(x,7,sqrt(2.1/k)), col="blue", lwd=2, add=TRUE)

k = 30
xbar = meanx(k, nreps=10000, n=10, p=0.7)
hist(xbar, freq=F, xlim=c(6,8), ylim=c(0,2), xlab=paste("sample size=",k), col=gray(0.8), breaks=seq(0,10,0.15)) # 막대사이 간격 0.5
curve(dnorm(x,7,sqrt(2.1/k)), col="blue", lwd=2, add=TRUE)

  # 함수로 구현
imsi <- function(k, nreps, n, p) {
  xbar = meanx(nreps, k, n, p)
  hist(xbar, freq=F, 
       xlim=c(n*p-0.45*sqrt(n*p*(1-p)),n*p+0.45*sqrt(n*p*(1-p))), 
       ylim=c(0, 2*sqrt(n*p*(1-p))),
       xlab=paste("sample size=",k), 
       breaks=seq(0,10,0.1)) # 막대사이 간격 0.5
  curve(dnorm(x,n*p,sqrt(n*p*(1-p)/k)), add=TRUE)
}

imsi(100, 10000, 10, 0.3)
imsi(100, 10000, 10, 0.4)
imsi(100, 10000, 10, 0.5)
imsi(100, 10000, 10, 0.6)


# 연속형 균일분포 U(0,1)에서
# 표본크기가 각각(5, 10, 15, 30, 50)인 표본을 100회씩 추출하여
# 평균을 계산하여 그래프를 그림
par(mfrow=c(1,1))
m <- 10000
x <- rep(0,m)
n.value=c(5,10,15,30,50)

plot(0,0, 
     type="n", 
     xlim=c(0.25,0.75), 
     ylim=c(0,10), 
     ylab="density",
     xlab="mx",
     main="uniform mean to normal"
     )
curve(dnorm(x,1/2,sqrt(1/(12*n))),add=T,lwd=5)
for( k in 1:length(n.value)){
  n <- n.value[k]
  for(i in 1:m) {
    x[i] <- mean(runif(n,0,1))
  }
  lines(density(x), lty=k, col=k, lwd=2)
}

