#t검정을 이용한 신뢰구간: 모분산을 모를 때 <338p>
mu <- 500
sigma <- 100
n <- 100
nreps <- 1000
pv <- rep(NA, nreps)

inout <- rep(NA, nreps)
for (i in 1:nreps){
  print(i)
  set.seed(i)
  x <- rnorm(n, mu, sigma)
  pv[i] <- t.test(x, mu = 500)$p.value
  lower <- t.test(x, mu = 500)$conf.int[1] #신뢰구간 하한
  upper <- t.test(x, mu = 500)$conf.int[2] #신뢰구간 상한
  inout[i] <- ifelse(500 >= lower & 500 <= upper,1,0)
}
table(inout)
mean(1-inout)
attributes(t.test(x,mu=500)) #사용가능한 데이터 목록
t.test(x,mu=500)$method
lower
upper


#예제9.1
x1 <- c(15, 20, 11, 23, 16, 21, 18, 16, 27, 24)
x2 <- c(23, 31, 13, 19, 23, 17, 28, 26, 25, 28)
t.test(x1, x2, paired=T, mu=0)
ttest = t.test(x1, x2, alternative="less", var.equal=T) ;ttest
  #모분산 동일, 단측검정
attributes(ttest)
ttest$conf.int[1] #신뢰구간 하한
ttest$conf.int[2] #신뢰구간 상한


#예제9.4
x1 <- c(11.8, 13.9, 16.3, 11.6, 8.4)
x2 <- c(11.4, 13.1, 16.1, 10.9, 8.3)
ttest = t.test(x1, x2, alternative="greater", var.equal=T, paired=T, mu=0) ;ttest
  #대응비교 paired=T
attributes(ttest)
ttest$conf.int
ttest$method
t.test(x1, x2, var.equal=T, paired=T, mu=0)$conf.int
  #신뢰구간 계산할때는 양측으로


#올림/내림함수
ceiling(2.23)
  #ceiling(x) - 정수로 올림
floor(2.23)
  #floor(x) - 정수로 내림
trunc(2.221323)
  #소수점 떼버림
signif(2.2323, 2)
  #signif(x, digits=i) - 유효숫자 2개까지만 표시 (반올림)
round(2.2232, 2)
  #round(x, digits=i) - 소수 둘째자리로 반올림

#함수 sample()
sample(1:4, 1000, replace=TRUE, p=c(0.1, 0.2, 0.3, 0.4))
  #1부터 4까지 추출, 1000회추출, 복원추출(replace), p=각 추출값이 추출될 확률


#함수 chisq.test()
  #카이제곱검정

#예제2.1 - 10p
x <- c(87, 96, 108, 89, 122, 98)
chisq.test(x)
  #적합도검정, x^2 = 8.58, pv = 0.127 
  #귀무가설 채택 (주사위가 균형적이다)


#실습과제1 - 14p
season <- c(25, 8, 29, 38)
chisq.test(season)
  #적합도검정, x^2 = 18.96, pv = 0.0002
  #귀무가설 기각 (계절별로 결혼하는 비율의 차이가 있다)

#실습과제2
child <- c(240, 505, 255)
chisq.test(child, p=c(1/4, 1/2, 1/4))
  #x^2 = 0.55, pv = 0.7596
  #귀무가설 채택 (자녀의 수가 이론적인 비율과 일치한다)

#실습과제3
color <- c(330, 320, 250)
chisq.test(color)
  #x^2 = 12.667, pv=0.001776
  #귀무가설 기각 (장롱의 색상에 대한 소비자 선호도가 다르다)

#예제 5.2.2 - p125 -독립성검정
coffee <- cbind(c(20,48), c(13,96)) ;coffee
chisq.test(coffee)
  #x^2 = 7.3272, pv=0.006792
  #귀무가설 기각 (커피선호와 고혈압사이에 서로 관계가 있다)


#실습과제1 - 129p -독립성검정
allergy <- cbind(c(95,20,35,65), c(15,10,23,37))
chisq.test(allergy)
  #x^2 = 18.9, pv=0.0002868
  #귀무가설 기각 (계절에 따른 알레르기 발병률의 차이가 있다)


#실습과제2 - 129p -독립성검정
alcohol.baby <- cbind(c(17,89), c(25,284))
chisq.test(alcohol.baby)
  #x^2 = 4.6411, pv=0.03122
  #귀무가설 채택 (산모의 음주와 신생아의 조산 사이에 관계가 없다)


#cumsum() 함수
x <- cumsum(1:10); x

#이항분포 시뮬레이션
set.seed(12345)
n <- rep(NA, 1000)
p <- rep(NA, 1000)

for(i in 1:1000) {
  x <- rbinom(i,1,0.5)
  p[i] <- mean(x)
  n[i] <- i
}

plot(n,p, type="l", xlab="Number of Trials", ylab="Proportion of Heads", main="Proportion of Heads")
lines(n, rep(0.5,1000), col="2", lwd=2)


#이항분포 시뮬레이션 - cumsum 함수사용
set.seed(12344)

x <- rbinom(2000,1,0.5)
y <- cumsum(x)
n <- 1:length(x)
p <- y/n

plot(n,p, ylim=c(0, 1), type="l", xlab="Number of Trials", ylab="Proportion of Heads", main="Proportion of Heads (using cumsum())")
lines(n, rep(0.5,length(x)), col="2", lwd=2)


#
x <- seq(-5,5,length=200)
y <- dnorm(x, mean=0, sd=1)
plot(x,y, type="l", ylim=c(0,0.5), main="t-분포의 정규분포 근사", col="blue")
curve(dt(x,df=1),add=T, col="grey50")
curve(dt(x,df=5),add=T, col="grey25")
curve(dt(x,df=20),add=T, col="grey2")
curve(dt(x,df=30),add=T, col="red")
text(x=-3.5, y=0.05, col="blue", "cauchy(0,1)")


