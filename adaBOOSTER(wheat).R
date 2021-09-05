#ADA Boost technique

library(maboost)
library(caret)
library(MASS)
library(corrplot)
path="C:/Users/mayur/Desktop/datascience DELL/data analyst vedios/r language/machine learning/wheat.csv"

wheat=read.csv(path,header=T)

View(wheat)
head(wheat)
dim(wheat)
colnames(wheat)
str(wheat)
#convert the y-variable to factor
wheat$type=as.factor(wheat$type)
str(wheat)

#do the EDA
anyNA(wheat)

#shuffle the dataset (since data is grouped by the 'type)
wheat=wheat[order(sample(seq(1,nrow(wheat)))),]
View(wheat)

#split the data into train and test
total=nrow(wheat)
ss=sample(seq(1,total),0.7*total)
train=wheat[ss,]
test=wheat[-ss,]

dim(train)
dim(test)

#split the train into trainx and trainy
ndx=grep("type",colnames(wheat))
trainx=train[,-ndx]
trainy=train[,ndx]

head(trainx)
head(trainy)

?maboost

#build the random forest model/prediction
library(randomForest)
m1 =randomForest(trainx,trainy)
p1=predict(m1,test)
c1=confusionMatrix(test$type,p1)
c1  
  #parameter settings
## ----------------
#c50tree -> set to TRUE for multi-class
#CF (confidence factor)
m2=maboost(x=trainx,y=trainy,C50tree = T,
    iter = 125,       #iter=100(can change later) 
            C5.0Control(CF=0.25,minCases = 2))

summary(m2)
p2=predict(m2,test)
c2= confusionMatrix(test$type,p2)
c2











