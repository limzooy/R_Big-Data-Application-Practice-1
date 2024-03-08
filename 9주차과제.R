#9주차 과제
#1번 iris 데이터에 대해서 p.21~23의 코드를 적용하시오.
install.packages("survival")
install.packages("caret")
install.packages("tibble")
library(survival)
library(ggplot2)
library(dplyr)
library(rpart)
library(randomForest)
library(caret)
library(e1071)

data(iris)
iris$Species=factor(iris$Species)
control=trainControl(method='cv', number=10)
#
formular=Species~.
#rpart(formular=Species~., data=iris, type='class')
#오류남
L=train(formular, data=iris, method='svmLinear', metric='Accuracy', trControl=control)
LW=train(formular, data=iris, method='svmLinearWeights', metric='Accuracy', trControl=control)
P=train(formular, data=iris, method='svmPoly', metric='Accuracy', trControl=control)
R=train(formular, data=iris, method='svmRadial', metric='Accuracy', trControl=control)
RW=train(formular, data=iris, method='svmRadialWeights', metric='Accuracy', trControl=control)
f100=train(formular, data=iris, method='rf', ntree=100, metric='Accuracy', trControl=control)
f300=train(formular, data=iris, method='rf', ntree=300, metric='Accuracy', trControl=control)
f500=train(formular, data=iris, method='rf', ntree=500, metric='Accuracy', trControl=control)
r=train(formular, data=iris, method='rpart', metric='Accuracy', trControl=control)
k=train(formular, data=iris, method='knn', metric='Accuracy', trControl=control)
g=train(formular, data=iris, method='glm', metric='Accuracy', trControl=control)
#p.22
resamp=resamples(list(선형=L, 선형가중치=LW, 다항식=P, RBF=R, 가중치=RW, rf100=f100, rf300=f300, rf500=f500, tree=r, knn=k, glm=g))
summary(resamp)
#p.23
sort(resamp, decreasing=TRUE)
dotplot(resamp)
#2번 ROCR패키지에는 ROCR.simple이라는 실습용 데이터가 들어있다. 이 데이터를 사용하여 ROC곡선을 그리고 AUC를 계산하라.
install.packages("ROCR")
library(ROCR)
data(ROCR.simple)
p=prediction(ROCR.simple$predictions, ROCR.simple$labels)
roc=performance(p, measure='tpr', x.measure='fpr')
plot(roc)
abline(a=0, b=1)
auc=performance(p, measure='auc')
auc@y.values

