install.packages("neuralnet")

#13주차 과제
#문제1번
library(neuralnet)
#1) 데이터 작성
x1 <- c(0, 0, 1, 1)
x2 <- c(0, 1, 0, 1)
d <- c(0, 1, 1, 0)

trainset <- data.frame(x1, x2, d)
trainset

testset <- data.frame(x1, x2, d)
testset

#2) 학습
nn <- neuralnet(d~x1+x2,
                trainset,
                hidden=2,
                linear.output=FALSE)

plot(nn)

#3) 테스트
predict<-compute(nn, testset)
predict$net.result

predict.bin<-ifelse(predict$net.result >= 0.5, 1, 0)
predict.bin

#4) 각 모형별 비교
#=2
nn <- neuralnet(d~x1+x2,
                trainset,
                hidden=2,
                linear.output=FALSE)

plot(nn)

predict<-compute(nn, testset)
predict$net.result

predict.bin<-ifelse(predict$net.result >= 0.5, 1, 0)
predict.bin

#=3
nn <- neuralnet(d~x1+x2,
                trainset,
                hidden=3,
                linear.output=FALSE)

plot(nn)

predict<-compute(nn, testset)
predict$net.result

predict.bin<-ifelse(predict$net.result >= 0.5, 1, 0)
predict.bin

#=4
nn <- neuralnet(d~x1+x2,
                trainset,
                hidden=4,
                linear.output=FALSE)

plot(nn)

predict<-compute(nn, testset)
predict$net.result

predict.bin<-ifelse(predict$net.result >= 0.5, 1, 0)
predict.bin
#=> 은닉층이 3개일 때 에러율이 가장 작기 때문에 은닉층이 3개인 모형이 가장 좋습니다.

#문제 2번
#데이터 준비비
cars
str(cars)
plot(cars)
trainset<-cars

max1<-max(cars[,1])
max2<-max(cars[,2])
trainset[,1]<-trainset[,1]/max1
trainset[,2]<-trainset[,2]/max2
trainset

testset<-data.frame(speed=c(15, 16))
testset[,1]<-testset[,1]/max1
testset

#학습
nn<-neuralnet(dist ~ speed,
              trainset,
              hidden=0,
              linear.output=TRUE)
plot(nn)

#예측
predict <- compute(nn, testset)
predict$net.result
predict$net.result*max(cars[,2])

