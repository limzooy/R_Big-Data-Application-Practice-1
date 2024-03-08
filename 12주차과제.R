#12주차 과제
#데이터 읽기
mtcars
str(mtcars)
data <- cbind(mtcars$hp, mtcars$mpg)
plot(mtcars$hp, mtcars$mpg, xlab="엔진마력", ylab="연비", col=factor(mtcars$carb))
#데이터 표준화
data.scaled<-as.data.frame(scale(data[,-11],center=TRUE, scale=TRUE))
head(data.scaled)
# 군집 수의 변화에 따른 TWSS의 변화
twss<-NULL
for(i in 1:15){
  kc<-kmeans(data.scaled, centers=i)
  twss<-c(twss, kc$tot.withinss)
}
plot(1:15, twss,
     xlim=c(0, 15), type="b",
     xlab="군집수", ylab="TWSS")
#군집화(클러스터링)
kc<-kmeans(data.scaled, 2)
kc
kc$cluster
kc$center
kc$withinss
kc$tot.withinss
#군집화 결과 출력
par(mar=c(5.1, 4.1, 4.1, 2.1))
plot(data.scaled[, 1], data.scaled[, 2], xlab="엔진마력", ylab="연비", pch=21, col=kc$cluster)
legend("topright", legend=levels(mtcars$carb), pch=21, col=kc$cluster, xpd=TRUE, inset=c(-0.5, 0))
points(kc$centers, pch=19, cex=1.5, col=rownames(kc$centers))

