getwd()
df<- read.csv('titanic_train.csv')
head(df)
str(df)
install.packages("Amelia")
library(Amelia)
missmap(df, main="missing map", col=c("red", "yellow"), legend=FALSE)
head(df, n=4)
ggplot(df, aes(Survived))+ geom_bar()
ggplot(df, aes(Pclass))+geom_bar(aes(fill=factor(Pclass)))
ggplot(df, aes(Sex))+ geom_bar(aes(fill=factor(Sex)))
ggplot(df, aes( Age))+geom_histogram(bins = 20, alpha=0.5)
# drop rows is solution?
will you drop rows or fill NA  with imputation.
pl<- ggplot (df, aes(Pclass,Age))+ geom_boxplot(aes(group=Pclass, fill=factor(Pclass)))
pl + scale_alpha_continuous(breaks=seq(min(0),max(80), by=2))+theme_bw()
#imputation age based on class
impute_age<- function(age,class){
  out<-age
  for(i in 1:length(age)){
    if (is.na(age[i])){
      if (class[i]==1){
        out[i]<- 37
        }else if (class[i]==2){
          out[i]<-29
        }else{
          out[i]<- 24
          }
      }else{
        out[i]<-age[i] 
      }
    }
    return (out)
  } 

fixed_age<- impute_age(df$Age, df$Pclass)
fixed_age
