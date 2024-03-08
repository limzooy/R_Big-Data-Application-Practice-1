#3주차 과제
library(ggplot2)
library(dplyr)
library(gapminder)

#문제1 - 2007년도 아시아 대륙의 인구 총합을 구하시오.
install.packages("gapminder")
library(gapminder)
library(dplyr)
glimpse(gapminder)
apply(gapminder[gapminder$continent=="Asia"&gapminder$year==2007, c("pop")], 2, sum)

#문제2 - 중국의 1인당 국내총생산과 기대 수명을 전체 관측 기간에 걸쳐 각각 출력하시오.
#filter - 중국
gapminder[gapminder$country=="China", c("gdpPercap","lifeExp", "year")]
Q2 <- gapminder %>% filter(country=="China") %>% select("gdpPercap", "lifeExp", "year")
Q2
