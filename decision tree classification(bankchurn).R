################# Decision Tree #####################
#### Classification ##############


library(caret)
library(ggplot2)
library(pROC)
library(ROCR)

path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/bankchurn.csv"

bank_c=read.csv(path,header=T) #,stringsAsFactors = T)

#bank_c=read.csv("bankchurn.csv",header = T)
colnames(bank_c)
dim(bank_c)
View(bank_c)

colnames(bank_c)[sapply(bank_c,is.integer)]
nc=colnames(bank_c)[sapply(bank_c,is.numeric)]
fc=colnames(bank_c)[sapply(bank_c,is.character)]
nc
fc

bank_c$custid=NULL
bank_c$surname=NULL

### EDA
# 1.) Factors- check Levels.

for(c in fc)
{
  print(c)
  
  print(levels(factor((unlist(bank_c[c])))))
  
  cat('\n')
}

bank_c$country[bank_c$country%in%c('Espanio','spain')]="Spain"

bank_c$country[bank_c$country%in%c('Ger','germany')]="Germany"

bank_c$country[bank_c$country%in%c('Fra','france')]="France"

table(bank_c$country)

bank_c$country=as.factor(bank_c$country)
str(bank_c$country)

## Gender -> merge the levels.

bank_c$gender[bank_c$gender%in%c('f','female','Female')]="F"
bank_c$gender[bank_c$gender%in%c('m','male','Male')]="M"

table(bank_c$gender)

bank_c$gender=as.factor(bank_c$gender)

str(bank_c$gender)


######## converting the Y variable into factor.
bank_c$churn=as.factor(bank_c$churn)

##### Checking the numeric columns.
# nulls
# zeros
# multicollinearity

nc
nc[apply(bank_c[nc],2,CheckZeros)]

## Check for the multicollinearity.
library(corrplot)
corr=cor(bank_c[nc])
corrplot(corr,method='number',type='lower') ## WE can see that there is no multicollinarity.

######## Now Split the data.
totalrows=nrow(bank_c)
ss=sample(seq(1,totalrows),floor(0.7*totalrows))
train=bank_c[ss,]
test=bank_c[-ss,]

colnames(train)
str(trai)


### For Decision Tree libraries are:
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

View(bank_c)

### Building the decision tree model.
# without any hyperparamter tuning.

# formula=y~x.
m1=rpart(churn~.,data=train,method="class") ## method=anova (for regression)

## Visualise the decision tree
rpart.plot(m1,type=4,extra=101,tweak = 3.0)
?rpart.plot ### check all the hyperparameters.

#### Predict
p1=predict(m1,test,type="class")
p1

#### COnfusion Matrix
confusionMatrix(test$churn,as.factor(p1),positive = "1")


table(test$churn)

### Complexity Parameter.
plotcp(m1)
m1$cptable

## Getting the minimum value of Cp corresponds to xerror

mincp=m1$cptable[which.min(m1$cptable[,"xerror"]),"CP"]

## prune the tree
m1_prune=prune(m1,mincp)

## predict on the prune model.
p1_prune=predict(m1_prune,test,type="class")

## confusion matrix on pruned predictions
confusionMatrix(test$churn,as.factor(p1_prune),positive = "1")

### check if the post-pruned model prediction are better than the pre-pruned model.

#1.) both model are same-then does not matter if the tree is pruned.

#2.) post-pruned model gives better result than the pre-pruned model-> retain the post-pruned model.

data.frame(score=m1$variable.importance)

################ NOW build the new model using hyperparameter tunning.

?rpart.control 

colnames(bank_c)
m2=rpart(churn~creditscore+country+gender+age+tenure+balance+active+salary,
         data=train,method="class",minsplit=200,
         cp=0.05,
         maxdepth=2)

## Visualise the tree
rpart.plot(m2,type=4,extra=101,tweak=1.5)

## predict
p2=predict(m2,test,type="class")

table(test$gender)
table(train$gender)


str(test$gender)
str(train$gender)
str(bank_c$gender)

## confusion matrix on pruned predictions
confusionMatrix(test$churn,as.factor(p2),positive = "1")














