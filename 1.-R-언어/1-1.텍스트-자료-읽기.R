# 텍스트 자료읽기
geyser <- read.table("geyser299.txt", header=T)
# read.table: 텍스트 파일을 읽기 위한 함수
# header=T: 파일의 첫 줄에 변수명이 있다는 뜻
# tools->global option에서 default working directory를 파일이 있는 곳으로 먼저 설정해줘야 한다
# geyser: 불러온 데이터 세트 이름

geyser #데이터셋 전체 출력
geyser[1:5,] #데이터셋 부분 출력
hist(geyser$waiting) #데이터 프레임 geyser에 속한 변수 'waiting'에 대한 히스토그램 출력