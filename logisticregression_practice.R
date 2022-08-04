df2<- read.csv("adult_sal.csv")
head(df, 4)
str(df2)
missmap(df2, col=c("blue", "red"))

df2<- select(df2, -X)
summary(df2)
head(df2)
table(df2$type_employer)

unemployed <- function(job){
  job<-as.character(job)
  if (job== "Never-worked" | job=="Without-pay" ){
    return("unemployed")}else {
      return(job)
    }
  }

df2$type_employer=sapply(df2$type_employer,unemployed )
table(df2$type_employer)

table(df2$education)
table(df2$type_employer)

selfemployed<- function(self){
  if (self=="Self-emp-inc" | self =="Self-emp-not-inc"){
    return("selfemployed")
  }else if (self=="Local-gov" | self== "State-gov"){
    return ("local")
  } else{
    return (self)
  }
  }
df2$type_employer <- sapply(df2$type_employer,selfemployed)

table(df2$type_employer)

table (df2$marital)

marriedd <- function(m){
  #m <- as.character(m)
  #notmarried 
  if (m=="Divorced" | m=="Separated"| m=="Widowed"| m=="Never-married"){
    return ("unmarried")} 
  #married 
  else if(m== "Married-AF-spouse"| m=="Married-civ-spouse"|m=="Married-spouse-absent"){
  return("married")} 
  else{
        return(m)
      }
    }
  
df2$marital <- sapply(df2$marital, marriedd)

table(df2$marital)


#countries

table(df2$country)


countrys <- function(q){
  if (q=="?"){
    return("na")
  } else {
    return(q)
  }
}

df2$country <- sapply(df2$country,countrys )
table(df2$country)


Asia<- c("China","Hong", "Iran", "Cambodia", "Japan","Laos","Philippines", " Vietnam", "Taiwan"," Thailand")

North.America<- c("Canada","United-States","Puerto-Rico")
Europe<- c("England", "France", "Germany", "Greece", "Holand-Netherlands", "Hungary", 
           "Irland","Italy","poland","Portugal"," Scotland", "Yugoslavia")

Latin.and.south.america<-c("Columbia", "Cuba", "Dominican-Republic")

other<- ("South")

group_country<- function(ctry){
  if (ctry %in% Asia){
    return("Asia")}
  else if (ctry %in% North.America){
      return("North.America")}
  else if (ctry%in% Europe){
        return ("Europe")}
  else if (ctry%in% Latin.and.south.america){
          return("Latin.and.south.america")}
  else {
            return("other")
          }
    }
      
df2$country<- sapply(df2$country, group_country ) 
 
table(df2$country)
#missing data 

library(Amelia)

df2[df2=="na"]<-NA
table(df2$type_employer)
df2[df2=="?"]<-NA
missmap(df2)
str(df2)
df$type_employer <- sapply(df$type_employer, factor)
df$country <- sapply(df$ country, factor)
df$marital<- sapply (df$ marital, factor)

df2<-na.omit(df2)
missmap(df2)
ggplot(df2, aes(age))+geom_histogram(binwidth=1, aes(fill=income, color="black"))+theme_bw()
head(df2)
ggplot(df2, aes (x=hr_per_week, y=age))+ geom_point(aes(fill= income, color="red"))+geom_col()

help("theme_bw")

df2<- rename(df2, region=country)

head(df2)

  