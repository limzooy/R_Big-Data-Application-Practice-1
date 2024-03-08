#5주차과제
library(ggplot2)
library(dplyr)

#문제1: 훈련 집합이 X={10.0, 12.0, 9.5, 22.2, 8.0}, Y={360.2, 420.0, 359.5, 679.0, 315.3}
#1) lm함수로 모델링하기
x=c(10.0, 12.0, 9.5, 22.2, 8.0)
y=c(360.2, 420.0, 359.5, 679.0, 315.3)
m=lm(y~x)
m
#2) 잔차 제곱합과 평균제곱 오차를 구하라
residuals(m)
deviance(m)
deviance(m)/length(x)

#3) 새로운 샘플이 10.5, 25.0, 15.0일 때 predict함수로 예측한 결과를 제시
newx=data.frame(x=c(10.5, 25.0, 15.0))
predict(m, newdata=newx)


#문제2: 베이스R이 제공하는 women데이터를 lm으로 모델링하고 새로운 데이터(130, 140, 151)를 가상으로 만들어 예측을 수행하라
str(women)
women
women_model=lm(weight~height, data=women)
coef(women_model)
nw1 = data.frame(height = c(130, 140, 151))
predict(women_model, nw1)
