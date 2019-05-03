#1.6.결측값,요인


#결측값
x <- c(2, 2, 3, NA)
mean(x)

mean(x, na.rm = T)
#na.rm: delete missing values

0 / 0 
1 / 0
#inf: infinite

10^seq(100, 1000, 100)


#요인 (factor): 범주형 변수

species <- c(1, 3, 2, 3)
#사실은 범주임에도 외형상 수치형으로 간주되므로

species.f <- factor(species, level = 1:3) 
#수준이 3개인 요인으로 변환

levels(species.f) <- c("setosa", "versicolor", "virginica")
#각 수준의 이름 지정

table(species.f)
#요인의 수준별 빈도