t_stat <- function(n,df,nreps) {
  
  t_stat = numeric(nreps)
  
  for(i in 1:nreps) {
    x <- rchisq(n,df,nreps)
    t_stat[i] <- (mean(x)-df)/(sd(x)/sqrt(n)) 
  }
  return(t_stat)
}

hist(t_stat(50,5,5000))
curve(dchisq(x,5), xlim=c(0,20))

f_test <- function(n,k1,k2,nreps) {
  f_test <- numeric(nreps)
  for(i in 1:nreps) {
    x <- rf(k1,k2,nreps)
    f_test[i] <- (mean(x)-(k2/(k2-2)))/(sd(x)/sqrt(n))
  }
  return(f_test)
}

f <- f_test(100,4,6,5000)
f2 <- 1/f_test(100,6,4,5000)

fi <- function(nreps,k1,k2) {
  
  x <- fi(nreps,k1,k2)
  y <- 1/x
  
  hist(y, freq=F,
       main=paste("X ~ F(",k1,",",k2,") 일 때","1/F(",k1,",",k2,")의 분포"),
       xlab = "1/x",
       col=gray(0.8), 
       breaks=seq(0,10000,0.2), 
       xlim=c(0,6))
      
  curve(df(x,k2,k1),
       col="blue",
       lwd=2,
       add=TRUE)
       
  text(y=0.4, x=2.5, paste("<==F(",k2,",",k1,")") ,col="blue")
}

par(mfrow=c(2,2))
fi(10000,5,10)


#다항분포
csq <- function(n,nreps) { #n-다항시행의 시행횟수, nreps-반복추출할 표본수
  cs = numeric(nreps)
  lp = length(p)
  for(i in 1:nreps){
    x = rmultinom(1,n,prob=p) #다항분포난수
    #Ex = numeric(lp)
    #b = 0
    
    #for(j in 1:lp){
      #Ex[j] = (x[j]-n*p[j])^2/(n*p[j])
      #b=b+Ex[j]
    #}
    #cs[i] = b
    cs[i] <- sum((x-n*p)^2/(n*p)) # line57~64 리팩토링
  }
  #print(cs)
  return (cs)
}

#예3
p = c(1/6,1/3,1/2) ;p
y = csq(n=100, nreps=10000) ;max(y)
hist(y, breaks=seq(0,100,0.5), ylim = c(0,0.5), xlim=c(0,15), col="gray95", freq=F)  
curve(dchisq(x,length(p)-1), add=T, lwd=2, col="blue")
text(x=5,y=0.2,"chisqure(2)",col="blue")

#예4
p = c(0.2,0.3,0.2,0.2,0.1) ;p
y = csq(n=200, nreps=10000) ;max(y)
hist(y, breaks=seq(0,30,0.4), ylim = c(0,0.2), xlim=c(0,20), col="gray95", freq=F)  
curve(dchisq(x,length(p)-1), add=T, lwd=2, col="blue")
text(x=5,y=0.2,"chisqure(4)",col="blue")


#기말고사 시험범위 
#t검정 시뮬레이션~ (237p~)
