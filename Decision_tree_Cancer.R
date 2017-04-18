library(rpart)
tumor=read.csv("cancer.csv",stringsAsFactors = T)
tumor$grade=as.factor(tumor$grade)
str(tumor)
head(tumor)
class1=subset(tumor,grade=="1")
class0=subset(tumor,grade=="0")

s1=sample(1:nrow(class1),0.7*(nrow(class1)))
s0=sample(1:nrow(class0),0.7*(nrow(class0)))

train1=class1[s1,]
test1=class1[-s1,]

train0=class0[s0,]
test0=class0[s0,]

train=rbind(train1,train0)
test=rbind(test1,test0)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
head(tumor)
apply(train[,c(1,9,10)],2,normalize)
apply(test[,c(1,9,10)],2,normalize)

fit=rpart(factor(grade)~.,method = "class",train,control = rpart.control(minsplit = 2))
fit
rpart.control(minsplit = 20,cp=0.01)
fit$cptable
plot(fit,compress = F,nspace = 0.1,uniform = T)
text(fit)
str(tumor)
lossmatrix=matrix(c(0,0.6,0.4,0),nrow=2,byrow = T)
fit1=rpart(factor(grade)~age+factor(ethnicity)+factor(ER)+factor(PR)+factor(RT)+factor(CT)+factor(HT,levels = unique(tumor["HT"]))+factor(N)+tumorStage+tumorSize,
           method = "class",train,control = rpart.control(minsplit = 2,xval=20,maxsurrogate = 10),parms = list(loss=lossmatrix,split="gini"))
fit1$cptable
summary(fit1)
xer=fit1$cptable[which.min(fit1$cptable[,"xerror"]),"CP"]
fit2=rpart(factor(grade)~age+factor(ethnicity)+factor(ER)+factor(PR)+factor(RT)+factor(CT)+
             factor(HT,levels = unique(tumor["HT"]))+factor(N)+tumorStage+tumorSize,
           method = "class",train,control = rpart.control(minsplit = 2,xval=20, maxsurrogate = 10),
              parms = list(loss=lossmatrix,split="information"))
fit2$cptable
xer1=fit2$cptable[which.min(fit2$cptable[,"xerror"]),"CP"]
summary(fit2)

if(xer>xer1)
{
  cpvalue=xer1
}else
{
  cpvalue=xer
}

xer
xer1

prufit=prune(fit1,cpvalue)
p1=predict(prufit,test,type="class")

library(caret)
confusionMatrix(p1,test$grade)

library(pROC)
library(ROCR)
#plot.roc(p1,test$grade)
#plot.roc(test$grade,p1)

library(ROCR)
pr <- prediction(as.numeric(p1), as.numeric(test$grade))
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
p=plot(prf,col="green",lwd=2,main="ROC curve")
abline(a=0,b=1,lwd=2,lty=2,col="red")


