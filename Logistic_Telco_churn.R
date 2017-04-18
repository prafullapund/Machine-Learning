telco=read.csv("Telco-Customer-Churn.csv")
boxplot(telco)
summary(telco)
str(telco)
is.na(telco)
telco=telco[,-1]
str(telco)
#telco$TotalCharges[is.na(telco$TotalCharges)]=mean(telco$TotalCharges)
summary(telco$TotalCharges)
#replace(telco$TotalCharges,is.na(telco$TotalCharges),mean(telco$TotalCharges[!is.na(telco$TotalCharges)]))
row.has.na <- apply(telco, 1, function(x){(is.na(x))})
sum(row.has.na)
#telco <- telco[!row.has.na,]#Deleting NA's
summary(telco)
nrow(telco)

minmax <- function(x) {
  newx <- (x-min(x))/(max(x)-min(x))
}
telco[,18] <- minmax(telco[,18])
telco[,19] <- minmax(telco[,19])
apply(telco[,18:19],2,minmax)
head(telco)

mylist <- split(telco,telco$Churn)
churn_yes<-mylist$Yes
churn_no<-mylist$No
##or
#churn_yes <- which(telco$Churn=="Yes",T)
#churn_no <- which(telco$Churn=="No",T)

churn_y<-sample((1:nrow(churn_yes)), round(0.8*nrow(churn_yes)))
churn_n<-sample((1:nrow(churn_no)), round(0.8*nrow(churn_no)))

train_y <- churn_yes[churn_y,]
train_n <- churn_no[churn_n,]
test_y <- churn_yes[-churn_y,]
test_n <- churn_no[-churn_n,]

train <- rbind(train_y,train_n)
test <- rbind(test_y,test_n)

#Checking the head and tail of data 
head(train)
tail(train)

#Checking the number of rows in test and train data set
nrow(train)
nrow(test)

#Checking the summary of data
summary(telco)

#Checking for NA values
anyNA(train)
summary(train)
str(train)
telcoglm=glm(Churn~.,family = binomial(logit), train)
summary(telcoglm)

telcoglm1 = glm(Churn~factor(gender)+SeniorCitizen+factor(Partner)+factor(Dependents)+factor(PhoneService)+factor(MultipleLines)+factor(InternetService)+factor(OnlineSecurity)+factor(OnlineBackup)+factor(DeviceProtection)+factor(TechSupport)+factor(StreamingTV)+factor(StreamingMovies)+factor(Contract)+factor(PaperlessBilling)+
                 factor(PaymentMethod)+MonthlyCharges+TotalCharges,family=binomial(logit), data=train)
summary(telcoglm1)
library(car)
predict1= predict.glm(telcoglm1,test$Churn,type = "response")
out1=ifelse(predict1>0.5,"1","0")
out1=as.factor(out1)
out1

#checking the accuracy
library(caret)
confusionMatrix(out1,test$Churn)
#Receive Operating characteristic curve
library(pROC)
plot(roc(test$Churn, predict1, direction="<"),
     col="yellow", lwd=3, main="ROC")






