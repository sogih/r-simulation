#1-5.논리 연산

g <- c(T, F, T, T)
g
f <- c(1.2, 2.0, 3.1, 4.9)
f > 3.14

as.numeric(f > 3.14)
#true-false -> 1-0

f1 <- f [ f > 3.14 ]
f1

C1 <- c(T, T, F, F)
C2 <- c(T, F, T, F)
C1 & C2
C1 | C2
! C1
! C2