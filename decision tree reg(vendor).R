#decision tree regression 
#vendor

path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/comp_perf.csv"

comp=read.csv(path,header=T) #,stringsAsFactors = T)

#comp=read.csv("comp_perf.csv",header = T)
View(comp)

# vendor
table(comp$vendor)
comp$vendor=as.factor(comp$vendor)

# Model Name
table(comp$model_name)
comp$model_name=NULL

## EDA
## Multicollinearity.
str(comp)

## split the data into train/test
total=nrow(comp)
ss=sample(seq(1,total),0.7*total)
train=comp[ss,]
test=comp[-ss,]

dim(train)
dim(test)

comp$vendor=NULL

## Build 2 Models
# 1.) Linear Regression
lm1=lm(erp~.,data=train)
p1=predict(lm1,test)


# 2.) DT
library(rpart)
library(rpart.plot)
dpt1=rpart(erp~.,data=train,method="anova")

# Visualise the tree
rpart.plot(dpt1,extra=101,type=4,tweak=2.5)

# predictions
p2=predict(dpt1,test)

## store results of LM and DT in a dataframe.
result=data.frame('actual'=test$erp,'LM'=floor(p1),'DT'=floor(p2))

View(result)

library(caret)
RMSE(test$erp,p1)
RMSE(test$erp,2)