#특정 분포 난수를 발생시켜서 막대그래프 그리기
x <- rbinom(1000, 10, 0.5)
hist(x)
table(x)
barplot(table(x)) 
  #이산형 자료의 막대그래프
barplot(table(x)/1000, ylim=c(0,0.25), axis.lty=1, main="rminom(1000,10,0.5)") 
  #상대도수 막대그래프
  #axis.lty=1: x축 라인 그리기

#포아송분포에서 난수를 발생시켜서 그래프 그리기
plot(table(rpois(100,5)), type="h", col="red", lwd=10, main="rpois(100, lambda = 5)")
  #lwd=10: 막대 너비
  #col="red": 막대 color
barplot(table(x))
  #막대그래프
barplot(table(x)/100, ylim=c(0,0.25))
  #상대도수 막대그래프
barplot(table(x)/100, ylim=c(0,0.25), col=3, main="rpois(100, lambda = 5)")

#산점도 그리기 
geyser <- read.table("geyser299.txt", header = T)
attach(geyser)
plot(duration~waiting, xlim=c(40,110), ylim=c(1,6), type="n", xlab="Waiting time (min)", ylab="duration (min)", main="geyser")
  #xlab: x축 이름
  #xlim: x축 간격
  #type="n": 
text(x=waiting, y=duration, cex=0.75)
  #관찰치 번호 삽입
text(90, 5.5, "Geyser")
  #x=90, y=5.5 위치에 텍스트 삽입

#다중프레임
x1 <- rbeta(400,1,1)
  #베타분포로부터 400개 난수 발생
x2 <- rbeta(400,2,2)
x3 <- rbeta(400,4,4)
x4 <- rbeta(400,8,8)
par(mfrow=c(2,2))
  #plot 파티션
hist(x1, breaks=seq(0,1,0.1), freq=F, ylim=c(0,3))
hist(x2, breaks=seq(0,1,0.1), freq=F, ylim=c(0,3))
hist(x3, breaks=seq(0,1,0.1), freq=F, ylim=c(0,3))
hist(x4, breaks=seq(0,1,0.1), freq=F, ylim=c(0,3))
  #freq=F: y축 density
par(mfrow=c(1,1))
#다중프레임 예제2
x <- (0:20)/20
  #0~20까지 수를 20으로 나눈것을 x에 할당
b11 <- dbeta(x,1,1)
b22 <- dbeta(x,2,2)
b33 <- dbeta(x,3,3)
b44 <- dbeta(x,4,4)
par(mfrow=c(2,2))
plot(x, b11, type="l")
plot(x, b22, type="l")
plot(x, b33, type="l")
plot(x, b44, type="l")

for(i in 1:4) {plot(x, dbeta(x,i,i), type="l")}
for(i in 1:4) {plot(x, dbeta(x,i,i), type="l", main=paste0("beta (",i,", ",i,")"))}
  #반복문 활용해서 한줄로 쓰기

#겹쳐 그리기
par(mfrow=c(1,1))
  #curve(함수, from=NULL, to=NULL, n=101, add=FALSE, type="l", xname="x", xlab=xname, ylab=NULL, log=NULL, xlim=NULL, ...)
  #각각 메소드는 default값을 넣은것임
curve(dt(x,10), -4, 4)
  #t분포
curve(dnorm(y), -4, 4, xname="y")
curve(dnorm(x), -4, 4, n=1000)
  #[-4,4]를 1000등분하여 함수를 그린다

#겹쳐 그리고 예제2
par(mfrow=c(1,1))
x <- rt(1000,5)
  #자유도 5인 t-dist에서 난수 1000개 발생
hist(x, freq=F, xlim=c(-5,5), ylim=c(0,0.4), nclass=100, xlab="sumulated observations", main="t (df=5)")
  #nclass: breaks와 같은 역할(bar를 몇 개 할 것인지) (breaks=20으로 써도된다)
m <- mean(x) #평균
sd <- sd(x) #표준편차
curve(dnorm(x,m,sd), add=T)
  #add=T: 

#함수 정의 하기-왜도, 첨도를 구하는 함수
skew.and.kurto <- function(x)
{
  num1 <- mean((x-mean(x))^3)
  denom1 <- (mean((x-mean(x))^2))^1.5
  num2 <- mean((x-mean(x))^4)
  denom2 <- (mean((x-mean(x))^2))^2
  skew <- num1/denom1
  kurto <- num2/denom2 - 3
  return(c(skew, kurto))
} #skew(왜도) +: 좌경, -: 우경
  #kurto(첨도) +: 급첨, -: 완침
    #원래 정규분포에서 첨도는 3인데 함수내의 연산에서 -3을 해줘서 0을 기준으로 판단할 수 있도록했음

#함수 호출하기- skew.and.kurto()
skew.and.kurto(rnorm(100))

#함수 merge() - 데이터셋 병합
  #method
    #all.x=all: x에있는 값은 모두 포함시킨다
    #sort=TRUE: 오름차순 정렬
data1 <- read.table("mid.txt", header=T)
data2 <- read.table("final.txt", header=T)
data1.and.2 <- merge(x=data1, y=data2, by.x="id", by.y="id", all=TRUE)
  #data1과 data2에 동시에 같은 id가 있는 자료만 병합됨
  #all=T: data1과 data2 중 어느 한 쪽에서 id가 있기만 해도 병합 파일에 포함되도록

#데이터 세트 분할 함수 split()
gr <- c(1,2,1,1,2)
gr <- factor(gr)
  #gr의 값들을 범주형 자료로 변경
levels(gr) <- c("low", "high")
score <- c(98, 82, 45, 23, 74)
score.split <- split(score, gr)
  #score의 값들을 gr의 값들에 의해 분리시킴
  #score, gr의 index에 대응하는 값으로 분류되는 것임
subset(score,gr=="low")
  #gr=="low"에 대응하는 score의 value를 추출
score[gr=="low"] 
  #indexing

#예제
  #x <- c(45, 32, 34, 28, 80)
  #y <- c(23, 37, 12, 76, 65)
  #일때 y가 50보다 큰 값을 갖는 x들만 선택하시오.
x <- c(45, 32, 34, 28, 80)
y <- c(23, 37, 12, 76, 65)
  #solution
    subset(x, y>=50) 
    #함수를 활용한 경우
    x[y>=50]  
    #index을 활용한 경우
    
#데이터 프레임: R에서 데이터 입력하기
people <- c("Kim", "Bob", "Ted", "Sue", "Liz", "Amanda", "Tricia", "Jonathan", "Luis", "Isabel")
scores <- c(17,19,21,25,16,15,23,24,29,17)
quiz <- data.frame(people, scores)
fix(quiz)

#외부 데이터 파일 읽기
  #1.엑셀 파일
    np.test <- read.csv("commute.csv", header=T)
  #2.spss파일 //foreign 라이브러리 로드 필요
    ee <- read.spss("EEstock2000.sav")
    
#R 작업관리
Is()
  #현재작업 중인 object 보기
rm(제거할목록)
  #object 지우기
rm(list=Is())
  #현재 작업 중인 모든 오브젝트 제거
geyser <- read.table("geyser299.txt", header=T)
x <- rbinom(n=299, size=1, prob=0.50)
geyser.x <- cbind(geyser, x)
write.table(geyser.x, "geyser1.txt")
  #x가 추가된 새로운 데이터가 현재 작업디렉토리에 "geyser1.txt"라는 이름으로 저장됨
  #write.csv(geyser.x, "geyser1.csv): 엑셀 csv로 저장
sink("geyser.out")
  #이후 실행결과는 "geyser.out"으로 보내짐
summary(geyser.x)
sink()
  #출력파일을 닫음

