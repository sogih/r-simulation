#임의 수 생성
x <- runif(400, -1, 1)
  # U(-1,1)에서 난수 400개 생성
  # help(hist)
hist(x)
hist(x, prob=TRUE, main="Histogram of x", sub="unif(-1,1)")
  # prob=TRUE: 상대도수 히스토그램
  # 상대도수 = density * 계급크기 <=> density = 상대도수 / 계급크기
hist(x, plot=F)
  # plot=FALSE: 히스토그램 말고 자료 리스트를 출력
x <- rnorm(100)
hist(x)
hist(x, density=10, angle=45, col=3, border=10, breaks=seq(-3,3,1))
  # breaks: 계급 구간, 계급의 경계
  # angle: 막대 내의 빗금의 각도  
  # col: 막대 내부 색
  # density: 막대 내부 빗금의 밀도

#자료불러와서 히스토그램 출력하기
commute <- read.table("commute.csv", header=FALSE)
str(commute$V1)
hist(commute$V1, col=5, prob=TRUE, breaks=c(seq(0,70,10),90,120), angle=135, density=10, border=10)
axis(1, at=c(seq(0,70,10),90,120))
axis(2, at=seq(0,0.020,0.005))
  # breaks=c(seq(0,70,10),90,120):
  # 0부터 70까지는 10간격으로, 이후 70~90간격, 90~120간격
  # c: combine
  # help(axis)

#특정 분포에서 난수생성
x <- rbinom(1000,10,0.5)
  # B(10,0.5)에서 난수 1000개
table(x)
h=hist(x, plot=F)
h
sum(h$counts)
hist(x)
hist(x, breaks=c(0, h$breaks), include.lowest=F)
  # breaks=c(0, h$breaks): 0~1 계급추가
  # include.lowest=F: 0< <=1, 1< <=2, 2< <=3, ...

x <- rpois(1000,5)
  #poisson(5)에서 난수 1000개
hist(x, breaks = c(-1, seq(0,8,1),9,13), xlim=c(-1,14), freq=F)
axis(1, at=c(-1, seq(0,8,1),9,13,14))
  #상대도수 히스토그램 
hist(x, breaks=c(-1, seq(0,13,1)), freq=T)