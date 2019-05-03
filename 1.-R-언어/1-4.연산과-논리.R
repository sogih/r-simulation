# 1-4. 연산과 논리

x <- c(2, 2, 3)
x^3 + 1

y <- c(1.2, 3.0, 4.8)
x * y
x / y
#같은 길이의 벡터의 연산은 각 위치에 대응하는 요소별 연산으로 정의됨

round(x / y, 2)
#반올림 함수 소수점 2자리로 반올림

M <- cbind(x, y)
M
t(M)
#행렬의 전치 transpose

M1 <- t(M) %*% M
M1
#행렬의 곱
# *과 %*%은 다르다

solve(M1)
M1
solve(M1) %*% M1
#역행렬함수 solve

E <- eigen(M1, symmetric=T)
E
E$vectors %*% diag(E$values) %*% t(E$vectors)
#즉, 원래의 행렬 M1으로 복귀한다
#대칭행렬 함수 engin
#대각행렬 함수 diag