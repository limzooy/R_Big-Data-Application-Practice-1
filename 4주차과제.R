#4주차 과제
#문제1. ggplot2 패키지를 이용하여 다음 그래프를 그리시오.
library(ggplot2)
library(dplyr)
ggplot(cars, aes(x=speed, y=dist))+geom_point()
plot(cars, main="cars")
#답: cars %>% ggplot(aes(speed, dist))+geom_point()

#문제2. 다음 그래프의 1952년도 그래프에 나타난 데이터를 사용하여 해당 연도 아시아 국가들의
#gdpPercap 순위를 막대그래프로 시각화하시오(y축에에 국가 이름 표시)
library(ggplot2)
library(dplyr)
library(gapminder)
gapminder %>% filter(year==1952&continent=="Asia") %>% 
  ggplot(aes(reorder(country, gdpPercap), gdpPercap))+geom_bar(stat="identity")+coord_flip()
         
         