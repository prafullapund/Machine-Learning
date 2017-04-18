library(car)
spam=read.csv("spam.csv")
boxplot(spam)
summary(spam)
str(spam)

ind <- which(spam$V58==1,T)
class1 <- spam[ind,]
class0 <- spam[-ind,]
ind0 <- sample(1:nrow(class0),round(0.80*(nrow(class0))))
ind1 <- sample(1:nrow(class1),round(0.80*(nrow(class1))))
train1 <- class1[ind1,]
train0 <- class0[ind0,]
test1 <- class1[-ind1,]
test0 <- class0[-ind0,]

train <- rbind(train1,train0)
test <- rbind(test1, test0)

fit=prcomp(train[-11],retx=T,center=T)
fit
fit$sdev #will give lambda
fit$rotation#will give eigen vector
dim(fit$rotation)
fit$x
fit$sdev/sum(fit$sdev)*100 # will give maximun variance
plot(fit$sdev/sum(fit$sdev)*100,type = "l")
newtrain=fit$rotation[,1:4]
newtest=scale(test[,-11],center = T,scale=F)%*%fit$rotation[,1:4]
plot(fit$rotation[,1],fit$rotation[,2])

lr=glm(V58~.,family=binomial("logit"),train)
summary(lr)
vif(lr)
step(lr)
predict1= predict.glm(lr,test,type = "response")
summary(lr)

lr1=glm(formula = V58 ~ V1 + V2 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + 
          V12 + V14 + V16 + V17 + V19 + V20 + V21 + V22 + V23 + V24 + 
          V25 + V26 + V27 + V28 + V29 + V31 + V33 + V35 + V36 + V38 + 
          V39 + V41 + V42 + V43 + V44 + V45 + V46 + V47 + V48 + V49 + 
          V52 + V53 + V54 + V56 + V57, family=binomial("logit"), train)

summary(lr1)

out1=ifelse(predict1>0.5,"1","0")
out1=as.factor(out1)
out1
library(caret)
confusionMatrix(out1,test$V58)
library(pROC)
plot(roc(test$V58, predict1, direction="<"),
     col="yellow", lwd=3, main="ROC")


