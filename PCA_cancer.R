cancer <- read.csv("cancer.csv")
boxplot(cancer)
summary(cancer)
str(cancer)
ind=sample(1:nrow(cancer),round(0.70*nrow(cancer)))

class1=subset(cancer,grade==1)
class0=subset(cancer,grade==0)

cancer$grade==1
class1=cancer[which(cancer$grade==1,T),]
class0=cancer[which(cancer$grade==0,T),]


ind=which(cancer$grade==1,T)
class1=cancer[ind,]
class0=cancer[-ind,]

ind0=sample(1:nrow(class0),round(0.80*(nrow(class0))))
ind1=sample(1:nrow(class1),round(0.80*(nrow(class1))))

train1=class1[ind1,] 
train0=class0[ind0,]
test1=class1[-ind1,]
test0=class0[-ind0,]
train=rbind(train1,train0)
test=rbind(test1,test0)

summary(cancer[,1]) [1]

anyNA(cancer)

which(is.na(cancer),T)

fit=prcomp(train[-11],retx=T,center=T)
summary(fit)
fit$sdev #will give lambda
fit$rotation#will give eigen vector
dim(fit$rotation)
fit$x
fit$sdev/sum(fit$sdev)*100 # will give maximun variance
plot(fit$sdev/sum(fit$sdev)*100,type = "l")
newtrain=fit$rotation[,1:4]
newtrain=as.data.frame(newtrain)
newtest=scale(test[,-11],center = T,scale=F)%*%fit$rotation[,1:4]
plot(fit$rotation[,1],fit$rotation[,2])
plot(cancer[,1])
hist(cancer[,1])
plot(cancer[,1],cancer[,"grade"])
plot(cancer[,"grade"],cancer[,1])
cor(cancer[,1],cancer[,"grade"])
hist(cancer[,2])
lr=glm(grade~.,newtrain,family=binomial("logit"))
out=predict(fit,newtest,type="response")
summary(fit)

vif(lr)
step(lr)
lr1=glm(formula = grade ~ age + RT + HT + N, family = binomial("logit"),data = train)
out1=predict.glm(lr1,newtest,type = "response")
summary(lr1)
library(ROCR)
ppp <- ifelse(out1>=0.5,1,0)
ppp
pr <- prediction(out1, newtest$grade)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
p=plot(prf,col="green",lwd=2,main="ROC curve for Logistic Regression")
abline(a=0,b=1,lwd=2,lty=2,col="red")

library(caret)
confusionMatrix(ppp,newtest$grade)
    