install.packages("ISLR")
library(ISLR)
help("ISLR")
head(iris)
stand<- scale (iris[1:4])
finaldata<- cbind(stand, iris[5])
head(finaldata , n=3)
set.seed(101)
sample<-sample.split(finaldata$Species, SplitRatio = 0.7)
train<-subset(finaldata, sample=TRUE)
test<-subset(finaldata, sample=FALSE)
##
library(class)

predict<-knn(train[1:4],test[1:4], train$Species, k=1)
error<-mean(test$species!= predict)
error
#selecting right k value 
predict<- NULL
error<-NULL


for (i in 1:10) {
  set.seed(101)
  predict<- knn (train[1:4],test[1:4], train$Species, k=i)
  error[i]<- mean(test$Species != predict)

}
k<- 1:10
error <- data.frame(error, k)









