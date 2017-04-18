library(car)
library(class)
#Multiclassification - One Vs. all

glass=read.csv("glass_multiclassification.csv")
boxplot(glass)
summary(glass)
str(glass)
anyNA(glass)

type1 <- subset(glass, Type==1)
type2 <- subset(glass, Type==2)
type3 <- subset(glass, Type==3)
type5 <- subset(glass, Type==5)
type6 <- subset(glass, Type==6)
type7 <- subset(glass, Type==7)

indtype1 <- sample(1:nrow(type1), round(0.7*nrow(type1)))
indtype2 <- sample(1:nrow(type2), round(0.7*nrow(type2)))
indtype3 <- sample(1:nrow(type3), round(0.7*nrow(type3)))
indtype5 <- sample(1:nrow(type5), round(0.7*nrow(type5)))
indtype6 <- sample(1:nrow(type6), round(0.7*nrow(type6)))
indtype7 <- sample(1:nrow(type7), round(0.7*nrow(type7)))

train1 <- type1[indtype1,]
train2 <- type2[indtype2,]
train3 <- type3[indtype3,]
train5 <- type5[indtype5,]
train6 <- type6[indtype6,]
train7 <- type7[indtype7,]

test1 <- type1[-indtype1,]
test2 <- type2[-indtype2,]
test3 <- type3[-indtype3,]
test5 <- type5[-indtype5,]
test6 <- type6[-indtype6,]
test7 <- type7[-indtype7,]

train <- rbind(train1, train2, train3, train5, train6, train7)
test <- rbind(test1, test2, test3, test5, test6, test7)
nrow(train)
nrow(test)

scaling=function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
apply(train[,c(1,2,3,4,5,6,7,8)],2,scaling)
apply(test[,c(1,2,3,4,5,6,7,8)],2,scaling)

#1 keeping 1 vs all
t1 <- train
t1$Type <- ifelse(train$Type==1,1,0)
te1 <- test
te1$Type <- ifelse(test$Type==1,1,0)
model <- glm(Type ~ ., family = binomial(link = logit),data=t1)
summary(model)
str(glass)
vif(model)
step(model)

model1 <- glm(Type ~ RI + Al + K + Ba + Fe,family = binomial(link = logit),data=t1)
summary(model1)

step(model)
model2 <- glm(Type ~ Na + Mg + Si + K +Ca + Ba ,family = binomial(link = logit),data=t1)
summary(model2)
vif(model2)

p1 <- round(predict(model1, test, type="response"), digits = 3)


#2 keeping 2 vs all
t2 <- train
t2$Type <- ifelse(train$Type==2,1,0)

te2 <- test
te2$Type <- ifelse(test$Type==2,1,0)

model3 <- glm(Type ~ ., family = binomial(link = logit),data=t2)
summary(model3)
vif(model3)

step(model3)
model4 <- glm(formula = Type ~ Na + Mg + Al + Si + K + Ca + Ba, family = binomial(link = logit), data = t2)
summary(model4)

model22 <- glm(Type ~  Na + Mg + Al + Si + K + Ba + Fe,family = binomial(link = logit),data=t2)
summary(model22)
vif(model22)

p2 <- round(predict(model22, test, type="response"),3)


#3 keeping 3 vs all
t3 <- train
t3$Type <- ifelse(train$Type==3,1,0)

te3 <- test
te3$Type <- ifelse(test$Type==3,1,0)

model3 <- glm(Type ~ ., family = binomial(link = logit),data=t3)
model3
summary(model3)
vif(model3)

p3 <- round(predict(model3, test, type="response"),3)


#5 keeping 5 vs all
t5 <- train
t5$Type <- ifelse(train$Type==5,1,0)

te5 <- test
te5$Type <- ifelse(test$Type==5,1,0)

model5 <- glm(Type ~ ., family = binomial(link = logit),data=t5)
model5
summary(model5)
vif(model5)

p5 <- round(predict(model5, test, type="response"),3)

#6 keeping 6 vs all
t6 <- train
t6$Type <- ifelse(train$Type==6,1,0)

te6 <- test
te6$Type <- ifelse(test$Type==6,1,0)

model6 <- glm(Type ~ ., family = binomial(link = logit),data=t6)
model6
summary(model6)
vif(model6)

p6 <- round(predict(model6, test, type="response"),3)

#7 keeping 7 vs all
t7 <- train
t7$Type <- ifelse(train$Type==7,1,0)

te7 <- test
te7$Type <- ifelse(test$Type==7,1,0)

model7 <- glm(Type ~ ., family = binomial(link = logit),data=t7)
model7
summary(model7)
vif(model7)

model71 <- glm(Type ~ ., family = binomial(link = logit),data=t7[,-3])
summary(model71)
vif(model71)

p7 <- round(predict(model71, test, type="response"),3)

p=data.frame(p1,p2,p3,p5,p6,p7)
p
mp=c()
for(i in 1:nrow(p))
{
  mp[i]=which.max(p[i,])
}
mp
a=test$Type
count=0
b=c()
for(i in 1: length(a))
 { for(j in 1: length(mp))
  {
   if(a[i]==mp[j])
  {
    count=count+1
  } 
   i=i+1
  }
}
print(count)

acc=length(which(mp==a,T))/length(mp)
acc



