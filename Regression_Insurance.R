#Predict medical expenses using linear regression
a=read.csv("insurance.csv") # will store data as data.frame
as.data.frame(a)
str(a) # give the structure of data set
summary(a)
View(a)
summary(a$expenses) 
hist(a$expenses)
table(a$region)
a$smoker
table(a$smoker)
cor(a[c("age","bmi","children","expenses")]) # gives the corelation between variables significant change in one variable changes other variable
# for feature selection corelation is important, range is -1 to 1 
#covariance is when one variable spred what is the effect on other variable

# Visualization
pairs(a[c("age","bmi","children","expenses")])

sampl=sample(1:nrow(a),round(0.7*nrow(a)))
train=a[sampl,]
test=a[-sampl,]

#
library(psych)
pairs.panels(a[c("age","bmi","children","expenses")])

# training a model on the data

ins=lm(expenses~age+children+bmi+sex+smoker+region,train)
ins
summary(ins)
library(car)
vif(ins)
step(ins)

#Improving model performance
a$age2=a$age^2
a$bmi30= ifelse(a$bmi>=30,1,0)

sampl=sample(1:nrow(a),round(0.7*nrow(a)))
train=a[sampl,]
test=a[-sampl,]
ins2=lm(expenses~age+age2+children+bmi+sex+bmi30*smoker+region,train)
summary(ins2)

pr <- predict(ins2,test)
error=predict(ins2,test)-test["expenses"]
rmse=sqrt(mean(error^2))
rmse
