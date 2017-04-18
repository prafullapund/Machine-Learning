# exploring and preparing the data
teens=read.csv("snsdata.csv")
str(teens)
summary(teens)
table(teens$gender)
table(teens$gender,useNA="ifany")
summary(teens$age)
teens$age=ifelse(teens$age>=13 & teens$age<20,teens$age,NA)
summary(teens$age)

# data preparation - dummy coding missing values
summary(teens$gender)
teens$female=ifelse(teens$gender=="F" & !is.na(teens$gender),1,0)
summary(teens$gender)
teens$no_gender = ifelse(is.na(teens$gender),1,0)
table(teens$no_gender)
table(teens$gender,useNA = "ifany")
table(teens$female,useNA = "ifany")
table(teens$no_gender,useNA = "ifany")


#data preparation - inputing the missing values
mean(teens$age)
mean(teens$age,na.rm=TRUE)
aggregate(data=teens,age~gradyear, mean, na.rm= TRUE)

ave_age=ave(teens$age,teens$gradyear,FUN=function(x) mean(x, na.rm = TRUE))
teens$age=ifelse(is.na(teens$age), ave_age, teens$age)
summary(teens$age)

# training a model on the data

library(stats)
interests=teens[5:40]
interests_z=as.data.frame(lapply(interests,scale))
set.seed(2345)
teens_clusters=kmeans(interests_z,5)

#evaluating model performance

teens_clusters$size
teens_clusters$centers
teens_clusters$withinss
teens_clusters$totss

#improving model performance
teens$cluster=teens_clusters$cluster
teens[1:5,c("cluster","gender","age","friends")]
aggregate(date=teens,age ~ cluster,mean)
aggregate(data=teens,female~cluster,mean)
aggregate(data=teens,friends~cluster,mean)
aggregate(data = teens, friends ~ cluster, mean)

