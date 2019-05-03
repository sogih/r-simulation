#함수의 선언
oddsum <- function(n) {
  sum <- 0
  for(i in 1:n) {
    if (i %% 2 == 0) next
    print(i)
    sum <- sum + i
  }
return(sum)
}

oddsum(10) #함수호출


#출력 cat()사용
oddsum <- function(n) {
  sum <- 0
  for(i in 1:n) {
    if (i %% 2 == 0) next 
    print(i)
    sum <- sum + i 
    }
  cat("1부터",n, "까지 홀수들의 합 =",sum, "\n")
}

oddsum(10) #함수호출


#제곱합 함수
squaresum <- function(n) {
  sum <- 0
  for(i in 1:n) {
    sum <- sum + i * i
  }
  cat("1부터",n,"까지의 제곱합은",sum,"입니다")
}

squaresum(5)

#짝수들의 세제곱합 함수
evenSquareSum <- function(n) {
  sum <- 0
  for(i in 1:n){
    if (i %% 2 == 0) {
      sum = sum + i * i * i
    }
  }
  cat("1부터",n,"까지 짝수들의 세제곱 합은",sum,"입니다")
}

evenSquareSum(10)


#피타고라스 정리 - 인자값을 벡터로 받는 경우
pythang <- function(x) {
  s <- x[1]
  t <- x[2]
  a <- t^2 - s^2
  b <- 2 * s * t
  c <- s^2 + t^2
  cat("The Pythangorean triple is:",a,b,c,"\n")
}
x <- c(1,2)
pythang(x)


#피타고라스 정리 - 인자값을 두개를 받는 경우
pythang <- function(x,y) {
  s <- x
  t <- y
  a <- t^2 - s^2
  b <- 2 * s * t
  c <- s^2 + t^2
  cat("The Pythangorean triple is:",a,b,c,"\n")
}
x <- 1
y <- 2
pythang(x,y)


#2차 방정식의 해를 찾는 함수 / 내가 푼 것
  #ax^2+bx+c=0
  #x={-b+-root(b^2-4ac)}/2a
  #if b^2-4ac >= 0  then x =실수
  #else x = 허수
solution <- function(a,b,c){
  a <- a
  b <- b
  c <- c
  d = b^2-4*a*c
  x1 <- 0
  x2 <- 0
  if (d >= 0) {
    x1 = {-b + sqrt(d)} / (2*a)
    x2 = {-b - sqrt(d)} / (2*a)
    print(x1)
    print(x2)
  } else {
    print("해가 없음")
  }
}

solution(1,2,3)
solution(1,2,5)
solution(3,-6,-2)
solution(4,6,-3)


#2차방정식 해를 찾는 함수/ 교수님이 푼것
solution <- function(a,b,c) {
  x <- numeric(2)
  D <- b^2-4*a*c
  if (D >= 0) {
    x[1] = (-b + sqrt(D))/(2*a)
    x[2] = (-b - sqrt(D))/(2*a)
    print(x)
  } else {
    print("이차방정식의 해를 구할 수 없다")
  }
}

solution(4,6,-3)
solution(1,2,1)


#신뢰 구간 시뮬레이션
nreps <- 100
ll <- numeric(nreps)
ul <- numeric(nreps)
n <- 100
mu <- 500
sigma <- 100
for(i in 1:nreps) {
  print(i)
  set.seed(i)
  x <- rnorm(n, mu, sigma)
  ll[i] <- mean(x) - qnorm(0.975)*sqrt(sigma^2/n)
  ul[i] <- mean(x) + qnorm(0.975)*sqrt(sigma^2/n)
}

##
which(ul <= 500)
which(ll >= 500)

##
mean(( ll <= 500 ) && ( 500 <= ul ))

##
table( ll >= 500 )
table( ul <= 500 )
table((ll <= 500) & (500 <= ul))
hist(ll, xlim=c(430, 570), ylim=c(0,50), xlab="N(500,100^2)", main="Histogram of CI")
par(new=T)
hist(ul, xlim=c(430, 570), ylim=c(0,50), xlab="", ylab="", main="", lty=3)


##
ci <- function(nreps,mu,sigma,alpha,n) {
  ll <- numeric(nreps)
  ul <- numeric(nreps)
  
  for(i in 1:nreps) {
    #set.seed(i)
    x <- rnorm(n, mu, sigma)
    ll[i] <- mean(x) - qnorm(1-alpha/2)*sigma/sqrt(n)
    ul[i] <- mean(x) + qnorm(1-alpha/2)*sigma/sqrt(n)
  }
  p <- mean(((ll <= mu) & (mu <= ul)))
  return(p)
}
ci(100,500,100,0.05,1000)

