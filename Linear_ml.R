install.packages("ggplot2")
install.packages("ggplot2movies")
# rstudio ggplot2 cheat sheet 
library(ggplot2)
library(ggplot2movies)

# Data& aesthetics

pl<- ggplot(movies, aes(x=rating))

#geometry layer 

pl2<- pl+geom_histogram(binwidth = 0.1 , aes(fill=..count..))

help("aes")

pl3<- pl2+xlab("Movie Rating ")+ylab("counts")
print(pl3+ggtitle("my title"))

#scatterplot
library (ggplot2)
df<- mtcars
# data and aesthetics 
pl<-ggplot(df, aes(x= wt, y=mpg))
pl3<-pl+geom_point(aes(color=hp), size=5)
pl3 +scale_color_gradient(low="blue", high="red")

install.packages("caTools")
library(caTools)
set.seed(101)
#split up samples 
sample <- sample.split(df$G3, SplitRatio = 0.7)
train<- subset(df, sample==TRUE)
#%30% TEST DATA 
test<- subset(df,sample==FALSE)
model<- lm(train$G3~ ., data= train)
summary(model)

res<-residuals(model)
class(res)
res<-as.data.frame(res)
head(res)

ggplot(res, aes(res))+geom_histogram(binwidth =0.5)

#bar 
install.packages("plotly")
library(plotly)
df<- mpg
pl<-ggplot(df, aes(x=class))+ geom_bar()

ggplotly(pl)

plot(model)

prediction
predicted<- predict(model, test)
combinedc<-cbind(predicted,test$G3)
colnames(combinedc) <- c("predicted", "actual")
final<-as.data.frame(combinedc)
#take care of naegative bales

to_zero <- function(x){
  if (x<0) {
    return (0)
  } else {
    return(x)
  }
}

final$predicted<-sapply(final$predicted, to_zero)

Mean Squared Error
mse <- mean((final$actual-final$predicted)^2)
print(mse)

print(mse^0.5)

SSE <- sum(final$predicted-final$actual)^2
SSE
SST <- sum((mean(df$G3)-final$actual)^2)
SST
R2<- 1-SSE/SST
R2







