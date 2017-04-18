library(car)
library(ROCR)
library(psych)
#Creating the working directory
#setwd("")

#Importing the file
Cancerfile <- read.csv(file = "cancer.csv")

#Checking whether the file is imported properly
head(Cancerfile)
tail(Cancerfile)
str(Cancerfile)
#Dividing the data into train and test data
ind <- which(Cancerfile$grade==1,T)
class1 <- Cancerfile[ind,]
class0 <- Cancerfile[-ind,]
ind0 <- sample(1:nrow(class0),round(0.80*(nrow(class0))))
ind1 <- sample(1:nrow(class1),round(0.80*(nrow(class1))))
train1 <- class1[ind1,]
train0 <- class0[ind0,]
test1 <- class1[-ind1,]
test0 <- class0[-ind0,]

train <- rbind(train1,train0)
test <- rbind(test1, test0)

#Checking the head and tail of data 
head(train)
tail(train)

#Checking the number of rows in test and train data set
nrow(train)
nrow(test)

#Checking the summary of data
summary(Cancerfile)

#Checking for the NA values
anyNA(Cancerfile)

#Checking for the outliers
boxplot(Cancerfile$ethnicity,horizontal = F)
boxplot(Cancerfile$age,horizontal = T)

#Checking the relation between response and each independent varaible
hist(Cancerfile[,1])
plot(Cancerfile[,"grade"],Cancerfile[,1])
cor(Cancerfile[,1],Cancerfile[,"grade"])


hist(Cancerfile[,2])
plot(Cancerfile[,"grade"],Cancerfile[,2])
cor(Cancerfile[,2],Cancerfile[,"grade"])


hist(Cancerfile[,3])
plot(Cancerfile[,c(3,11)])
cor(Cancerfile[,3],Cancerfile[,"grade"])

hist(Cancerfile[,4])
plot(Cancerfile[,c(4,11)])
cor(Cancerfile[,4],Cancerfile[,"grade"])

hist(Cancerfile[,5])
plot(Cancerfile[,c(5,11)])
cor(Cancerfile[,5],Cancerfile[,"grade"])

hist(Cancerfile[,6])
plot(Cancerfile[,c(6,11)])
cor(Cancerfile[,6],Cancerfile[,"grade"])

hist(Cancerfile[,7])
plot(Cancerfile[,c(7,11)])
cor(Cancerfile[,7],Cancerfile[,"grade"])

hist(Cancerfile[,8])
plot(Cancerfile[,c(8,11)])
cor(Cancerfile[,8],Cancerfile[,"grade"])

hist(Cancerfile[,9])
plot(Cancerfile[,c(9,11)])
cor(Cancerfile[,9],Cancerfile[,"grade"])

hist(Cancerfile[,10])
plot(Cancerfile[,c(10,11)])
cor(Cancerfile[,10],Cancerfile[,"grade"])

plot(Cancerfile)
pairs.panels(Cancerfile)
#Developing a model considering all independent variables
fit <- glm(grade ~ sqrt(age)+ethnicity+ER+PR+RT+CT+HT+N+tumorStage+tumorSize, data=train,binomial(link = "logit"))
fit
summary(fit)
vif(fit)
p <- predict.glm(fit, test, type="response")
ppp <- ifelse(p>=0.5,1,0)
ppp
pr <- prediction(p, test$grade)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf,col="green",lwd=2,main="ROC curve for Logistic Regression")
abline(a=0,b=1,lwd=2,lty=2,col="red")

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

count<-0
accuracy <-0
for(i in 1:nrow(test)){
  if(p[i]==test[i,11]){
    count = count+1
  }
}
accuracy = count/nrow(test)
library(caret)
confusionMatrix(ppp,test$grade)

