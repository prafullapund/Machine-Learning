library(rpart)
telco1=read.csv("Telco-Customer-Churn.csv")
telco=telco1[,-1]
str(telco)
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

fit=rpart(factor(Churn)~.,method = "class",train,control = rpart.control(minsplit = 2))
fit
fit$cptable
plot(fit,compress = F,nspace = 0.1,uniform = T)
text(fit)
str(telco)

lossmatrix=matrix(c(0,0.4,0.6,0),nrow=2,byrow = T)
fit1=rpart(Churn~gender+factor(SeniorCitizen)+Partner+Dependents+PhoneService+MultipleLines+InternetService+OnlineSecurity+OnlineBackup+
             DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+PaperlessBilling+PaymentMethod+MonthlyCharges+TotalCharges,method = "class",train,control = rpart.control(minsplit = 2,xval=20,maxsurrogate = 10),parms = list(loss=lossmatrix,split="information"))
fit1$cptable
xer=fit1$cptable[which.min(fit1$cptable[,"xerror"]),"CP"]
prunfit=prune(fit1,xer)
p1=predict(prunfit,test,type="class")
summary(fit1)
library(caret)
confusionMatrix(p1,test$Churn)

library(ROCR)
pr <- prediction(as.numeric(p1), as.numeric(test$Churn))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
p=plot(prf,col="green",lwd=2,main="ROC curve")
abline(a=0,b=1,lwd=2,lty=2,col="red")



