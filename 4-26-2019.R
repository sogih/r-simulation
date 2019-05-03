confident.mean.known <- function(nreps, n, mu, sigma, alpha){
  ll <- numeric(nreps)
  ul <- numeric(nreps)
  for(i in 1:nreps) {
    set.seed(i)
    x <- rt(n, mu, sigma)
    ll[i] <- mean(x) - qnorm(1-alpha/2) * sigma/sqrt(n)
    ul[i] <- mean(x) + qnorm(1-alpha/2) * sigma/sqrt(n)
  }
  p <- mean((ll <= mu) && (mu <= ul))
  cat("모평균을 포함하는 신뢰구간의 비율=", p,"\n")
}
confident.mean.known(nreps=1000, n=10, mu=500, sigma=100, alpha=0.05)

confident.mean.unknown <- function(nreps, n, mu, sigma, alpha) {
  ll <- numeric(nreps)
  ul <- numeric(nreps)
  for(i in 1:nreps){
    set.seed(i)
    x <- rnorm(n, mu, sigma)
    ll[i] <- mean(x) - qt(1-alpha/2, n-1)*sd(x)/sqrt(n)
    ul[i] <- mean(x) + qt(1-alpha/2, n-1)*sd(x)/sqrt(n)
  }
  p <- mean((ll <= mu) & (mu <= ul))
  cat("모평균을 포함하는 신뢰구간의 비율=", p, "\n")
}
confident.mean.unknown(nreps=1000, n=100, m=500, sigma=100, alpha=0.05)




#모분산 모를때 신뢰구간
confident.length.unknown <- function(nreps,n,mu,sigma,alpha) {
  length <- numeric(nreps)
  for(i in 1:nreps){
    set.seed(i)
    x <- rnorm(n,mu,sigma)
    length[i] = 2*qt(1-alpha/2, n-1) * sd(x)/sqrt(n)
  }
  cat("모평균에 대한 신뢰구간의 길이=",mean(length) ,"\n")
}
confident.length.unknown(nreps=1000, n=10, mu=500, sigma=100, alpha=0.05)


#모분산 알때 신뢰구간
confident.length.unknown <- function(nreps,n,mu,sigma,alpha) {
  length <- numeric(nreps)
  for(i in 1:nreps){
    set.seed(i)
    x <- rnorm(n,mu,sigma)
    length[i] = 2*qnorm(1-alpha/2) * sigma/sqrt(n)
  }
  cat("모평균에 대한 신뢰구간의 길이=",mean(length) ,"\n")
}
confident.length.unknown(nreps=1000, n=1000, mu=500, sigma=100, alpha=0.05)


#일표본 t-검정
t <- t.test(x, mu=500)
attributes(t) #통계량출력
t$conf.int[1] #신뢰구간하한
t$conf.int[2] #신뢰구간상한
t$conf.int #신뢰구간
t$estimate #모평균 추정량


#