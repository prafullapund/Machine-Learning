library(choroplethr)
library(choroplethrMaps)
library(DescTools)
library(DT)
library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(plyr)
library(lubridate)
library(randomForest)
library(reshape2)
library(stargazer)
library(DBI)
library(RSQLite)
#importing data from SQLite file
con = dbConnect(SQLite(), dbname="database.sqlite")
myQuery <- dbSendQuery(con, "SELECT * FROM loan")
loan <- dbFetch(myQuery, n = -1)
loan=loan[-1]
str(loan)
dim(loan)
View(loan)
write.csv(loan,"loan_new.csv")
#imporing data
loan=read.csv("loan.csv")
#Structure of Data set
str(loan)
dim(loan)
summary(loan)
as.data.frame(rapply(loan,function(x)length(unique(x))))

#Descriptive Statistics
stargazer(loan,type="text",title="Descriptive Statistics",digits=1)

# Cheking and Removing Duplicated Records
nrow(loan) - nrow(unique(loan))
x<-loan[duplicated(loan),]
dim(x)
col_uv = sapply(loan, function(x) length(unique(x)))
cat("Constant feature count:", length(col_uv[col_uv==1]))
names(col_uv[col_uv==1]) # Remove this feature
#loan = loan[, !names(loan) %in% names(col_uv[col_uv==1])]

#checking NA values and removing unwanted variables
data=loan
ncol=rep(nrow(data) ,each=ncol(data))
missingdata=as.data.frame(cbind(colnames=names(data),ncol,nmsg=as.integer(as.character(as.vector(apply(data, 2, function(x) length(which(is.na(x)))))))))
missingdata$nmsg=as.numeric(levels(missingdata$nmsg))[missingdata$nmsg]

missingdata=cbind(missingdata,percmissing=as.integer(missingdata$nmsg/ncol*100))
drops=as.character(subset(missingdata,missingdata$percmissing>0)[,1])
length(drops)

loan$tot_coll_amt[which(is.na(loan$tot_coll_amt))]=median(loan$tot_coll_amt,na.rm = T)
summary(loan$tot_coll_amt)

loan$total_rev_hi_lim[which(is.na(loan$total_rev_hi_lim))]=median(loan$total_rev_hi_lim,na.rm = T)
summary(loan$total_rev_hi_lim)

loan$tot_cur_bal[which(is.na(loan$tot_cur_bal))]=median(loan$tot_cur_bal,na.rm = T)
summary(loan$tot_cur_bal)


#spliting the Year and month and converting it into factor
loan$month_iss=as.factor(str_split_fixed(loan$issue_d, "-", 2)[,1]) # Extract Month of Issue date
loan$year_iss=as.factor(str_split_fixed(loan$issue_d, "-", 2)[,2]) # Extract Year

loan$month_earliest_cr_line=as.factor(str_split_fixed(loan$earliest_cr_line, "-", 2)[,1]) # Extract Month Ch
loan$year_earliest_cr_line=as.factor(str_split_fixed(loan$earliest_cr_line, "-", 2)[,2]) # Extract Year

loan$month_last_pymnt=as.factor(str_split_fixed(loan$last_pymnt_d, "-", 2)[,1]) # Extract Month of Last payement date
loan$year_last_pymnt=as.factor(str_split_fixed(loan$last_pymnt_d, "-", 2)[,2]) # Extract Year of Last payment date

loan$month_next_pymnt=as.factor(str_split_fixed(loan$next_pymnt_d, "-", 2)[,1]) # Extract Month of next payment date
loan$year_next_pymnt=as.factor(str_split_fixed(loan$next_pymnt_d, "-", 2)[,2]) # Extract Year of next payment date

loan$month_last_credit_pull=as.factor(str_split_fixed(loan$last_credit_pull_d, "-", 2)[,1]) # Extract Month of last credit pull
loan$year_last_credit_pull=as.factor(str_split_fixed(loan$last_credit_pull_d, "-", 2)[,2]) # Extract Year of last credit pull


colSums(is.na(loan))
loan=loan[-c(1,2,11,19,20,22,23,24,27,29,30,34,46,48,49,51,52,54,55,56,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74)]
loan=loan[-(which(is.na(loan$annual_inc))),]
loan=loan[-(which(is.na(loan$delinq_2yrs))),]
loan=loan[-(which(is.na(loan$collections_12_mths_ex_med))),]
colSums(is.na(loan))
summary(loan)
str(loan)
dim(loan)

normalization=function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
str(loan)
apply(loan[,c(1,2,3,5,6,11,17,22,23,27,28,29,30,32,34)],2,normalization)

#loan=loan[-39787,]
#table(loan$loan_status)
#loan_new=na.omit(loan)
#levels(loan$loan_status)

##levels(loan$loan_status)[c(1,2,3,4,7,8,9,10)] = 1   
#levels(loan$loan_status)[c(5,6)] = 0

#Outcome Variable
#loan$loan_status=ifelse(loan$loan_status=="Default" | loan$loan_status=="Charged Off" | loan$loan_status=="Late (31-120 days)" 
#                        |loan$loan_status=="Does not meet the credit policy. Status:Charged Off"|loan$loan_status=="Late (16-30 days)"
#                        |loan$loan_status=="In Grace Period",1,0)
loan$Default=ifelse(loan$loan_status=="Current" | loan$loan_status=="Fully Paid" | loan$loan_status=="Issued" 
                    | loan$loan_status=="In Grace Period",0,1)
table(loan$Default)/nrow(loan)

month_num <- function(x) match(tolower(x), tolower(month.abb)) # Function to convert month in character to number

loan$MTD<-  (4-month_num (loan$month_iss)) + (2016-as.numeric(levels(loan$year_iss))[loan$year_iss])*12 # months till date

loan$Mon_T_Date<-0

loan <-loan %>% 
  rowwise() %>% 
  mutate(Mon_T_Date = ifelse(term..60.months ==1,min(36,MTD),min(18,MTD)))
table(loan$Mon_T_Date)


loan <-loan %>% 
  rowwise() %>% 
  mutate(Perc_M_T_Date = ifelse(term..36.months ==1,min(36,MTD)/36*100,min(18,MTD)/18*100))
summary(loan$Perc_M_T_Date)


loan %>% 
  filter(loan_status == '0') %>% 
  select(annual_inc, int_rate, loan_status) %>% 
  datatable(., options = list(pageLength = 10))





#spliting class variables
ind=which(loan$loan_status==1,T)
class1=loan[ind,]
class0=loan[-ind,]
samp1=sample(1:nrow(class1),round(0.7*nrow(class1)))
samp0=sample(1:nrow(class0),round(0.7*nrow(class0)))

train1=class1[samp1,]
train0=class0[samp0,]
test1=class1[-samp1,]
test0=class0[-samp1,]

train=rbind(train1,train0)
test=rbind(test1,test0)
str(loan)
#generating logistic regression
model1=glm(loan_status~loan$factor(term)+int_rate+grade+dti,data=train,family = binomial(link = "logit"))

out1=predict.glm(model1,test,type = "response")

ppp <- ifelse(out1>=0.5,1,0)

library(caret)
confusionMatrix(ppp,test$loan_status)



#row.has.na <- apply(loan_class, 1, function(x){(is.na(x))})
#sum(row.has.na)


# Missing Variables while comparing with loan and datadictionary
library(readxl)

dataDictionary <- read_excel("LCDataDictionary.xlsx")

# fields available in the data dictionary
dd_names <- as.character(na.omit(dataDictionary$LoanStatNew))

# fields available in the loan book
loanbook_names <- names(loan)

# show the fields described in data dictionary but not in the loan book
setdiff(dd_names, loanbook_names)


Desc(loan$loan_amnt, main = "Loan amount distribution", plotit = TRUE)

loan$issue_d <- as.Date(gsub("^", "01-", loan$issue_d), format="%d-%b-%Y")
amnt_df <- loan %>% 
  select(issue_d, loan_amnt) %>% 
  group_by(issue_d) %>% 
  summarise(Amount = sum(loan_amnt))

ts_amnt <- ggplot(amnt_df, 
                  aes(x = issue_d, y = Amount))
ts_amnt + geom_line() + xlab("Date issued")

#spliting class variables
library(caTools)
sample = sample.split(loan$loan_status, SplitRatio =0.01) 
train = subset(loan, sample == TRUE )
test = subset(loan, sample == FALSE)
table(train$loan_status)/nrow(train)
dim(train)

#Model Building

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats =1 ,search = "random")

xgb_model <- train(y=train$loan_status,x=train[,c(1:ncol(train))],
                   method = "xgbTree",
                   trControl = fitControl,tuneLength = 5)

xgb_model

predictions <- predict(xgb_model, newdata = test[,c(1:ncol(test))],
                       contrasts(as.factor(train$loan_status)))

confusionMatrix(predictions,test$loan_status)

library(pROC)
predictions <- predict(xgb_model, newdata = test[,c(1:ncol(test))],type="prob")
names(predictions)

rocCurve <- roc(response = test$loan_status,
                predictor = predictions[,1])
auc(rocCurve)

plot(rocCurve, print.thres = "best")

confusionMatrix(predictions[,1]>0.045,(as.numeric(factor(test$loan_status))-1)==0)

varImp(xgb_model)

#Model Building - Simple tree
library(rpart )
library(rpart.plot)
library(caTools)

ctrl <-rpart.control(minsplit = 100, minbucket = round(100/3), cp = 0.001, xval = 10, surrogatestyle = 0, maxdepth = 10)

rpart_model=rpart (loan_status~.,train[,c(1:ncol(train))],control= ctrl)

prp(rpart_model)

printcp(rpart_model)


library(pROC)
library(caret)

predictions <- as.data.frame(predict(rpart_model, newdata = test[,c(1:ncol(test))],row.names=FALSE))

names(predictions)

rocCurve <- roc(response = test$loan_status,
                predictor = predictions[,1])
auc(rocCurve)
plot(rocCurve, print.thres = "best")

confusionMatrix(predictions[,1]>0.088,(as.numeric(factor(test$loan_status))-1)==0)
