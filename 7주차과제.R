#7주차 과제
#문제: UCLA admisssion, colon, voice데이터에 SVM을 적용하고, 결정 트리와 랜덤포레스트와 
#정확률을 비교해라
library(ggplot2)
library(dplyr)
install.packages("e1071")
library(e1071)
install.packages("randomForest")
library(randomForest)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("rpart")
library(rpart)

#UCLA admission 데이터
ucla=read.csv('https://stats.idre.ucla.edu/stat/data/binary.csv')
str(ucla)
ucla$admit=factor(ucla$admit)
s1=svm(admit~., data=ucla)
print(s1)
table(predict(s1, ucla), ucla$admit)
#
r1=rpart(admit~., data=ucla)
par(mfrow=c(1, 1), xpd=NA)
plot(r)
text(r1, use.n=TRUE)
p1=predict(r, ucla, type='class')
table(p1, ucla$admit)
f1=randomForest(admit~., data=ucla)
print(f1)
treesize(f1)


#colon 데이터
install.packages("survival")
library(survival)
clean_colon=na.omit(colon)
clean_colon=clean_colon[c(TRUE, FALSE), ]
clean_colon$status=factor(clean_colon$status)
str(clean_colon)
s2=svm(status~., data=clean_colon)
print(s2)
table(predict(s2, clean_colon), clean_colon$status)
#
r2=rpart(status~rx+sex+age+obstruct+perfor+adhere+nodes+differ+extent+surg+node4, data=clean_colon)
p2=predict(r2, clean_colon, type='class')
table(p2, clean_colon$status)
plot(r2)
text(r2, use.n=TRUE)
summary(r2)
f2=randomForest(status~rx+sex+age+obstruct+perfor+adhere+nodes+differ+extent+surg+node4, data=clean_colon)
print(f2)
treesize(f2)


#voice 데이터
voice=read.csv("C:/Users/limjy/Desktop/3학년 1학기/빅데이터응용실습1/실습/voice.csv")
str(voice)
voice$label=factor(voice$label)
table(is.na(voice))
s3=svm(label~., data=voice)
print(s3)
table(predict(s3, voice), voice$label)
#
r3=rpart(label~., data=voice)
par(mfrow=c(1, 1), xpd=NA)
plot(r3)
text(r3, use.n=TRUE)
p3=predict(r3, voice, type='class')
table(p3, voice$label)
f3=randomForest(label~., data=voice)
print(f3)
treesize(f3)

