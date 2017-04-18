wine <- read.csv("winequality-red.csv")
boxplot(wine)
cor(wine[c("fixed.acidity","volatile.acidity","citric.acid","residual.sugar","chlorides","free.sulfur.dioxide","total.sulfur.dioxide","density","pH","sulphates","alcohol","quality")])
summary(wine$quality)
View(wine)
sort(unique(wine$quality))
str(wine)
#fixed.acidity + volatile.acidity + citric.acid + chlorides + total.sulfur.dioxide + density + sulphates
sampl <- floor(0.7 * nrow(wine))
train_ind <- sample(seq_len(nrow(wine)),size = sampl)
train <- wine[train_ind,]
test <- wine[-train_ind,]
ptest <- test

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
head(wine)
apply(train[,c(-2,-3,-4,-5,-8,-10,-11)],2,normalize)
apply(train[,c(-2,-3,-4,-5,-8,-10,-11)],2,normalize)

#install.package("moments")
library(moments)
boxplot(train$fixed.acidity)
summary(train$fixed.acidity) #values of min max median first quartile third quartile
IQR(train$fixed.acidity) #inter quartile range
sd(train$fixed.acidity)
var(train$fixed.acidity)
skewness(train$fixed.acidity)
kurtosis(train$fixed.acidity)

boxplot(train$volatile.acidity)
summary(train$volatile.acidity)
IQR(train$volatile.acidity)
sd(train$volatile.acidity)
var(train$volatile.acidity)
skewness(train$volatile.acidity)
kurtosis(train$volatile.acidity)

boxplot(train$citric.acid)
summary(train$citric.acid)
IQR(train$citric.acid)
sd(train$citric.acid)
var(train$citric.acid)
skewness(train$citric.acid)
kurtosis(train$citric.acid)

boxplot(train$residual.sugar)
summary(train$residual.sugar)
IQR(train$residual.sugar)
sd(train$residual.sugar)
var(train$residual.sugar)
skewness(train$residual.sugar)
kurtosis(train$residual.sugar)

boxplot(train$chlorides)
summary(train$chlorides)
IQR(train$chlorides)
sd(train$chlorides)
var(train$chlorides)
skewness(train$chlorides)
kurtosis(train$chlorides)

boxplot(train$free.sulfur.dioxide)
summary(train$free.sulfur.dioxide)
IQR(train$free.sulfur.dioxide)
sd(train$free.sulfur.dioxide)
var(train$free.sulfur.dioxide)
skewness(train$free.sulfur.dioxide)
kurtosis(train$free.sulfur.dioxide)

boxplot(train$total.sulfur.dioxide)
summary(train$total.sulfur.dioxide)
IQR(train$total.sulfur.dioxide)
sd(train$total.sulfur.dioxide)
var(train$total.sulfur.dioxide)
skewness(train$total.sulfur.dioxide)
kurtosis(train$total.sulfur.dioxide)

boxplot(train$density)
summary(train$density)
IQR(train$density)
sd(train$density)
var(train$density)
skewness(train$density)
kurtosis(train$density)

boxplot(train$pH)
summary(train$pH)
IQR(train$pH)
sd(train$pH)
var(train$pH)
skewness(train$pH)
kurtosis(train$pH)

boxplot(train$sulphates)
summary(train$sulphates)
IQR(train$sulphates)
sd(train$sulphates)
var(train$sulphates)
skewness(train$sulphates)
kurtosis(train$sulphates)

boxplot(train$alcohol)
summary(train$alcohol)
IQR(train$alcohol)
sd(train$alcohol)
var(train$alcohol)
skewness(train$alcohol)
kurtosis(train$alcohol)

boxplot(train$quality)
summary(train$quality)
IQR(train$quality)
sd(train$quality)
var(train$quality)
skewness(train$quality)
kurtosis(train$quality)

fit1 <- lm(quality ~.,train)
pr <- predict(fit1,test)
error=predict(fit1,test)-test["quality"]
rmse=sqrt(mean(error^2))
summary(fit1)
plot(fit1)
vif(fit1)
step(fit1)
fit2=lm(formula = quality ~ volatile.acidity + citric.acid + 
     residual.sugar + chlorides + total.sulfur.dioxide + density + 
     pH + sulphates + alcohol, data = train)
summary(fit2)
pr=predict(fit2,test)
error=pr-test$quality
rmse=sqrt(mean(error^2))
rmse
