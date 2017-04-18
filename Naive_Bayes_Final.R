library(caret)
## Loading required package: lattice
## Loading required package: ggplot2
library("e1071", lib.loc="~/R/win-library/3.3")
data=(read.csv("flag data.csv",header=T,stringsAsFactors=T))
# ind=(read.csv("ind.csv",header=F))
# f1=(read.csv("f1.csv",header=F))

train=(read.csv("train_index.csv",header=T))

datan=data[-which(data["population"]==0|data["area"]==0,T)[,1],]
datan["population"]=(datan["population"]-min(datan["population"]))/(max(datan["population"])-min(datan["population"]))
datan["area"]=(datan["area"]-min(datan["area"]))/(max(datan["area"])-min(datan["area"]))

split=0.8
trainIndex <- createDataPartition(as.factor(datan$religion), p=split, list=F)
data_train <- data.frame(datan[trainIndex,])
data_test <- data.frame(datan[-trainIndex,])

model <- naiveBayes(religion ~ ., data =data_train, laplace =0)
#model <- naiveBayes(religion ~ ., data =data_train[,c(2,  6 , 7 , 9, 13, 14, 18, 20, 21, 24, 26, 29, 30)], laplace =0)
pred=predict(model,data_train, type = "class")
confusionMatrix(pred, data_train$religion)$overall[1]
##  Accuracy 
## 0.3333333
model <- naiveBayes(religion ~ ., data =data_train, laplace =0)
#model <- naiveBayes(religion ~ ., data =data_train[,c(2,  6 , 7 , 9, 13, 14, 18, 20, 21, 24, 26, 29, 30)], laplace =0)
pred=predict(model,data_test,type= "class")
confusionMatrix(pred, data_test$religion)$overall[1]
##   Accuracy 
## 0.04166667
# split=0.8
# trainIndex <- createDataPartition(as.factor(datan$religion), p=split, list=F)
# data_train <- data.frame(datan[trainIndex,])
# data_test <- data.frame(datan[-trainIndex,])
#model <- naiveBayes(religion ~ ., data =data_train, laplace =0)
model <- naiveBayes(religion ~ ., data =data_train[,c(2,  6 , 7 , 9, 13, 14, 18, 20, 21, 24, 26, 29, 30)], laplace =0)
pred=predict(model,data_train, type = "class")
confusionMatrix(pred, data_train$religion)$overall[1]
##  Accuracy 
## 0.7280702
#model <- naiveBayes(religion ~ ., data =data_train, laplace =0)
model <- naiveBayes(religion ~ ., data =data_train[,c(2,  6 , 7 , 9, 13, 14, 18, 20, 21, 24, 26, 29, 30)], laplace =0)
pred=predict(model,data_test,type= "class")
confusionMatrix(pred, data_test$religion)$overall[1]
## Accuracy 
##      0.5
t=c()
##LOO CV
for(i in seq(1:114)){
  #data_train=data_train[-i,]
  data_test=data_train[i,]
  model <- naiveBayes(religion ~ ., data =data_train[-i,c(2,  6 , 7 , 9, 13, 14, 18, 20, 21, 24, 26, 29, 30)], laplace =0)
  
  pred=predict(model,data_test, type = "class")
  table(pred, data_test$religion)
  t=c(t,confusionMatrix(pred, data_test$religion)$overall[1])
}
mean(t)  
## [1] 0.5263158
##Optimal Samples and Features 
trainIndex=train$Resample1
trainIndex
##   [1]   1   2   3   4   5   7   8   9  10  11  12  14  15  16  17  19  21
##  [18]  22  23  25  26  28  30  31  32  33  36  37  38  39  44  45  46  47
##  [35]  48  49  50  51  52  53  54  55  57  58  59  60  61  62  64  66  67
##  [52]  69  70  71  72  74  75  76  77  78  79  80  81  82  83  84  85  86
##  [69]  87  88  89  90  91  93  94  95  96  97  98  99 100 101 102 103 105
##  [86] 106 107 108 109 110 111 112 113 114 115 116 117 118 120 121 122 123
## [103] 124 125 126 127 128 130 131 132 133 135 137 138
data_train <- data.frame(datan[trainIndex,])
data_test1 <- datan[-trainIndex,]
t=c()

for(i in 1:50){
  data_test <- data_test1[sample(1:24,24),]
  #data_test <- data_test1[sample(1:24,20),]
  model <- naiveBayes(religion ~ ., data =data_train[,c(  2,  6 , 7 , 9, 13, 14, 18, 20, 21, 24, 26, 29, 30)], laplace =0)
  
  pred=predict(model,data_test, type = "class")
  table(pred, data_test$religion)
  t=c(t,confusionMatrix(pred, data_test$religion)$overall[1])
}
table(pred, data_test$religion)
##                  
## pred              Buddhist Catholic Ethnic Hindu Marxist Muslim
##   Buddhist               0        0      0     0       0      0
##   Catholic               0        5      0     0       1      0
##   Ethnic                 0        0      4     0       1      2
##   Hindu                  0        0      0     0       0      0
##   Marxist                1        0      1     0       1      0
##   Muslim                 0        0      0     0       0      4
##   Other Christian        0        0      0     0       0      0
##   Others                 0        0      0     0       0      0
##                  
## pred              Other Christian Others
##   Buddhist                      0      0
##   Catholic                      0      0
##   Ethnic                        1      0
##   Hindu                         0      0
##   Marxist                       0      0
##   Muslim                        0      0
##   Other Christian               3      0
##   Others                        0      0
##Test Error
confusionMatrix(pred, data_test$religion)$overall[1]
##  Accuracy 
## 0.7083333
mean(t)
## [1] 0.7083333
t=c()
##LOO CV
for(i in seq(1:114)){
  #data_train=data_train[-i,]
  data_test=data_train[i,]
  model <- naiveBayes(religion ~ ., data =data_train[-i,c(2,  6 , 7 , 9, 13, 14, 18, 20, 21, 24, 26, 29, 30)], laplace =0)
  
  pred=predict(model,data_test, type = "class")
  table(pred, data_test$religion)
  t=c(t,confusionMatrix(pred, data_test$religion)$overall[1])
}
mean(t)  
## [1] 0.5350877
###Training Error
#model <- naiveBayes(religion ~ ., data =data_train, laplace =0)

model <- naiveBayes(religion ~ ., data =data_train[,c(2,  6 , 7 , 9, 13, 14, 18, 20, 21, 24, 26, 29, 30)], laplace =0)
pred=predict(model,data_train, type = "class")
table(pred, data_train$religion)
##                  
## pred              Buddhist Catholic Ethnic Hindu Marxist Muslim
##   Buddhist               1        0      0     0       0      0
##   Catholic               0       22      1     0       0      0
##   Ethnic                 0        0     18     1       0      5
##   Hindu                  0        0      0     3       0      1
##   Marxist                3        1      0     0      11      0
##   Muslim                 2        0      0     0       0     18
##   Other Christian        1        1      1     0       0      0
##   Others                 0        0      1     0       1      0
##                  
## pred              Other Christian Others
##   Buddhist                      0      0
##   Catholic                      4      0
##   Ethnic                        3      0
##   Hindu                         0      0
##   Marxist                       0      0
##   Muslim                        1      0
##   Other Christian               9      0
##   Others                        1      4
###Training Error
confusionMatrix(pred, data_train$religion)$overall[1]
## Accuracy 
## 0.754386
