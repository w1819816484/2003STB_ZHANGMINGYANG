library(dplyr)
library(ggplot2)

foodshop <- read.csv("ggd_food_2023.csv", na="", stringsAsFactors = F)
str(foodshop)

foodshop <- foodshop %>%
  rename(open_date=인허가일자, status=상세영업상태명, close_date=폐업일자, name=사업장명, type=업태구분명, address=소재지전체주소) %>%
  select("name","type","status","open_date","close_date", "address")
str(foodshop)


foodshop$open_date <- gsub("-","",foodshop$open_date)
foodshop$close_date <- gsub("-","",foodshop$close_date)

foodshop$open_date <- as.integer(foodshop$open_date)
foodshop$close_date <- as.integer(foodshop$close_date)

str(foodshop)

foodshop<-foodshop%>%filter(open_date!= '') %>%select(name,type,status,open_date,close_date,address)
str(foodshop)
table(foodshop$status)

range(foodshop$open_date, na.rm = T)
table(is.na(foodshop$open_date))
foodshop$open_year<-substr(foodshop$open_date,1,4)


range(foodshop$close_date, na.rm = T)
foodshop$close_year<-substr(foodshop$close_date,1,4)

foodshop$district<-substr(foodshop$address,7,9)
table(foodshop$district)
foodshop$district <- ifelse(foodshop$district%in%c("도 제","시 망","시 수","시 영","시 원","시 일"),NA,foodshop$district)
table(foodshop$district)

foodshop$open_year <- as.integer(foodshop$open_year)
foodshop$close_year <- as.integer(foodshop$close_year)
str(foodshop)

#데이터분석

#1.가장 오래 영업 중인 음식점
foodshop %>%
  filter(!is.na(open_date)&status=="영업") %>% 
  filter(open_date==min(open_date)) %>% 
  select(name, type, open_date, address)

#2.주요 업종별로 가장 오래 영업중인 음식점
foodshop %>%
  filter(!is.na(open_date)&status=="영업") %>% 
  filter(type%in%c("기타","출장조리","분식","일식","중국식","호프/통닭","횟집"))%>%
  group_by(type) %>%
  filter(open_date==min(open_date)) %>% 
  select(name, type, open_date, address)

#3.업종별 개업 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% 
  group_by(type) %>%
  summarise(n=n()) %>% 
  mutate(total=sum(n),pct=round(n/total*100,1)) %>% 
  arrange(desc(n)) %>%
  head(10)

#4.영업 중인 음식점의 업종별 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% 
  filter(status=="영업") %>%
  group_by(type) %>%
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1)) %>%
  arrange(desc(n)) %>%
  head(5)

#5.전체 음식점의 영업과 폐업 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% 
  group_by(status) %>%
  summarise(n=n()) %>%
  mutate(total=sum(n),pct=round(n/total*100,1))

#6.주요 업종별 영업과 폐업 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% 
  filter(type%in%c("기타","출장조리","분식","일식","중국식","호프/통닭","횟집"))%>%
  group_by(type,status) %>%
  summarise(n=n()) %>% 
  mutate(total=sum(n),pct=round(n/total*100,1))%>%
  filter(status=="영업") %>%
  arrange(desc(n))

#7.개업이 많았던 연도
foodshop %>%
  filter(!is.na(open_date)&!is.na(district))%>% 
  group_by(open_year) %>%
  summarise(n=n()) %>% 
  arrange(desc(n)) %>%
  head(5)

#8.폐업이 많았던 연도
foodshop %>%
  filter(!is.na(close_date)&!is.na(district))%>%
  group_by(close_year) %>%
  summarise(n=n()) %>% 
  arrange(desc(n)) %>%
  head(5)


#9.연도별 개업 음식점수 그래프

open_trend <- foodshop %>%
  filter(!is.na(open_date)&!is.na(district)) %>%
  group_by(open_year) %>%
  summarise(open_n=n())

str(open_trend)

ggplot(data=open_trend,aes(x=open_year,y=open_n))+
  geom_col()+
  xlab("연도") + ylab("개업수")

#10.연도별 폐업 음식점수 그래프

close_trend <- foodshop %>%
  filter(!is.na(open_date)&!is.na(district)) %>% 
  group_by(close_year) %>%
  summarise(close_n=n())

str(close_trend)

ggplot(data=close_trend,aes(x=close_year,y=close_n))+
  geom_col()+
  xlab("연도") + ylab("폐업수")

#11.개업과 폐업 음식점 통합 그래프
open_trend1<-rename(open_trend,year=open_year)
close_trend1<-rename(close_trend,year=close_year)

open_close_trend<-left_join(open_trend1,close_trend1,by="year")

ggplot()+
  geom_line(data=open_close_trend, aes(year,open_n))+
  geom_line(data=open_close_trend, aes(year,close_n,color="red"))+
  xlab("연도") + ylab("개수")

#12.폐업음식점수가 개업음식점수보다 많았던 기간 확인
open_close_trend %>%
  filter(close_n>open_n)


#13.영업중인 음식점수가 가장 많은 5개 구
district_business<-foodshop %>%
  filter(!is.na(open_date)&!is.na(district)&status=="영업") %>%
  group_by(district) %>%
  summarise(n=n())

district_business %>%
  arrange(desc(n)) %>%
  head(5)


#14,25개 구의 음식점수 막대그래프
ggplot(data = district_business, aes(x=reorder(district,n),y=n))+
  geom_col()+
  coord_flip()+
  xlab("영업구")+
  ylab("영업 음식점 수")


#15.주요 업종별로 영업하는 음식점이 많은 구
foodshop %>%
  filter(!is.na(open_date)&!is.na(district)) %>% 
  filter(type%in%c("기타","출장조리","분식","일식","중국식","호프/통닭","횟집"))%>%
  filter(status=="영업") %>% 
  group_by(type,district) %>%
  summarise(n=n()) %>%
  mutate(total=sum(n),pct=round(n/total*100,1))%>% 
  group_by(type) %>%
  filter(pct==max(pct))