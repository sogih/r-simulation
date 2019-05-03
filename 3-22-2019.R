#3/22


#논리 연산
x <- c(2, 2, 3)
y <- c(1.2, 3.0, 4.8)
x ^ 3 + 1
x * y
x / y
round(3.33, 2)

M <- cbind(x,y) ;M #3x2
t(M) #2x3
#전치행렬 (행과 열을 바꿈)
M1 <- t(M) %*% M
M %*% t(M)
solve(M1) %*% M1
#역행렬함수 solve

E <- eigen(M1, symmetric=T) ;E
E$vectors %*% diag(E$values) %*% t(E$vectors)
E$vectors %*% t(E$vectors)
t(E$vectors) %*% E$vectors
#eigen:대칭행렬에 대한 고유값과 고유벡터, sqrt:제곱근, diag:대칭행렬(대각행렬), t:전치행렬, solve:역행렬

rtL = sqrt(E$values); rtL
rtM1 = E$vectors %*% diag(rtL) %*% t(E$vectors)
rtM1 %*% t(rtA)

#논리연산
f <- c(1.2, 2.0, 3.1, 4.9)
f > 3.14
as.numeric(f > 3.14) #bool->number/ 0= False, 1= True
f1 <- f[f>3.14] ;f1 #true인 value만 return

C1 <- c(T, T, F, F)
C2 <- c(T, F, T, F)
C1 & C2 #둘다 true일 때만 true return
C1 | C2 #둘 중 하나라도 true이면
!C1 #not


#결측값
#NA: not availble, NaN: not a number
x <- c(2, 2, 3, NA)
mean(x) #결측값이 있으면 수행하지 않는다
mean(x, na.rm = T) #na.rm: 결측값을 빼고 수행한다
0/0
1/0 #inf:무한대

#요인(factor): 숫자값을 범주형 변수로 인식
species <- c(1, 3, 2, 3, 1, 2) #수치형 변수로 인식
species.f <- factor(species, level = 1:3) ;species.f #수치형 변수를 범주형 변수로 변환(수치->명목)
table(species.f) #변수 species.f의 빈도를 구한다
levels(species.f) <- c("setosa", "versicolor", "virginica") ;species.f 
  #species.f의 세 값에 각각 level을 부여함
  #factor가 아닌 경우 level이 적용되지 않음
table(species.f) #변수 species.f에 대해 빈도를 구함

#자료 변환과 데이터 부분세트

x <- c(45, 32, 34, 28, 80)
y <- c(23, 37, 12, 76, 65)
d2 <- list(kor=x, eng=y)
d2a <- transform(d2, tot=kor + eng)  #data frame d2를 불러와 tot라는 변수를 추가하여 d2a에 저장함
str(d2a)
str(d2)
mean(d2a$tot)
d3 <- subset(d2a, tot > 100, select = kor:eng);d3 
  #k1: object, k2: condition, k3(optional):원하는 변수만 선택
  #d2a의 부분 세트


#순서정렬과 순위

x <- c(12, 6, 4, 7, 8)
y <- c(34, 26, 14, 57, 92)
sort(x) #오름차순 정렬
sort(x, decreasing=T) #내림차순 정렬
order(x)#가장작은 값의 위치, 두번째 작은 값의 위치, ...
order(x, decreasing=T)
order(x, na.last=T) #결측값을 맨뒤로, =NA는 결측값 제거
x[order(x)] # <=>sort(x)
y[order(x)] # x의 순위에 대응하는 위치의 y를 정렬
rank(x)
  #ties.method=c("average", "first", "random", "max", "min")
  #avg는 평균순위. first는 앞에 있는 것에 우선순위
  #random은 무작위로 순위부여
  #max는 동점을 갖는 값을 가장 큰 순자의 순위로
  #min은 동점을 갖는 값을 가장 작은 숫자의 순위로
x1 <- c(45, 45, 34, 28, 80)
rank(x1)
rank(x1, ties.method=c("first"))
rank(x1, ties.method=c("max"))
rank(x1, ties.method=c("random"))


#apply

geyser <- read.table("geyser299.txt", header=T); geyser
apply(geyser, 2, median) #열 별로 중앙값을 계산(k2:1=행, 2=열)
hist(apply(geyser, 1, sum)) #행 별로 합계를 계산것을 히스토그램으로 출력
summary(apply(geyser, 1, sum)) #행 의 기초통계 summary
a <- lapply(geyser, median) ;a #새 리스트 a를 만듬
a <- apply(geyser, 2, median) ;a #새 벡터 a를 만듬


#loop
#문법: for(변수 in 변수에 넣을 값) 반복할 표현식
a <- rep(1, 20)
b <- rep(1, 20)
for (i in 3:100) a[i] <- a[i-2] + a[i-1] #이때 a의 세번째부터 계산
a[a<100] #값이 100 이하인 것만 출력
for(i in 3:20){
  a[i] <- a[i-1] + a[i-2]
  b[i] <- b[i-1] + (-1)^(i-1) * b[i-2]
}
a[a<100]
b[b<100]
  # for문 안에서 " if(조건) break "로 
  # "if (조건) next "로 조건이 만족하면 수행하지 않고 그 루프를 실행하지 않음
  # %%:나머지, %/%:몫 10%/%3->3
#while
#while (조건) {명령문} // 조건이 참이면 명령문을 반복해서 수행 거짓이면 루프에서 나감
#repeat
#repeat{ //멈추는 조건을 충족할 때까지 해당 작업을 반복
# 명령문
#   조건  break
# 명령문
#}


