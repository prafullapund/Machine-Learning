telco1=read.csv("Telco-Customer-Churn.csv")
telco=telco1[,-1]
str(telco)
which(is.na(telco),T)
row.has.na=apply(telco,1,function(x){any(is.na(x))})
sum(row.has.na)
telco=telco[!row.has.na,]
class1=subset(telco,Churn=="Yes")
class0=subset(telco,Churn=="No")
s1=sample(1:nrow(class1),0.7*(nrow(class1)))
s0=sample(1:nrow(class0),0.7*(nrow(class0)))

train1=class1[s1,]
test1=class1[-s1,]

train0=class0[s0,]
test0=class0[-s0,]

train=rbind(train1,train0)
test=rbind(test1,test0)
nrow(test)
nrow(train)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
head(telco)
apply(train[,c(17,18)],2,normalize)
apply(test[,c(17,18)],2,normalize)

fit=randomForest(Churn~.,ntree=500,train,importance=FALSE, do.trace=100)
summary(fit)
pred=predict(fit,test)
library(caret)
confusionMatrix(pred,test$Churn)

