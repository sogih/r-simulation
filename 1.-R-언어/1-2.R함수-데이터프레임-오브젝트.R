help(read.table) # 함수 read.table의 사용법😇

geyser <- read.table("geyser299.txt", header=T)
# geyser: 데이터프레임이라고하는 하나의 '오브젝트'
# 데이터프레임: 데이터세트+메타정보로 변수 이름 등을 포함하는 구조체(str)

str(geyser)
# 메타정보 출력

ls()
# 현재 R 작업 오브젝트에 어떤 것이 있는지 출력

attach(geyser) # geyser$waiting -> waiting
hist(waiting)
hist(geyser$waiting) # attach했어도 작동한다
detach(geyser)
hist(waiting)
hist(geyser$waiting)
