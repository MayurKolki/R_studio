#RANDOM FOREST CLASSIFICATION
#dataset:diabetes
# library
library(corrplot)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/diab.csv"

diab=read.csv(path,header=T) #,stringsAsFactors = T)

dim(diab)

colnames(diab)

#remove amy of the y variables
diab$class_val=NULL

str(diab)

#convert y to factor
diab$class=as.factor(diab$class)

#numeric columns
nc=names(diab)[sapply(diab,is.numeric)]
nc


checkzero=function(x) return(any(x==0))
nc[apply(diab[nc],2,checkzero)]

#need to impute the following cols to value greater than 0
#1)pg_conc

#2)dia_bp
#3)se_ins
#4)bmi



table(diab$dia_bp)
table(diab$tri_sf_th)
#bmi=weight*703/(height)^2
table(diab$bmi)
mean(diab$bmi)

#colnames(diab)
# EDA
CheckZero=function(x) return(any(x==0))
checknull=function(x) return(any(is.na(x)))

names(diab)[apply(diab,2,CheckZero)]
names(diab)[apply(diab,2,checknull)]


#split the data into train /test
total=nrow(diab)
ss=sample(seq(1,total),floor(0.7*total))
train=diab[ss,]
test=diab[-ss,]

dim(train)
dim(test)

table(train$class)

table(test$class)

#split the data into trainx/trainy and testx/testy
pos=which(colnames(train)=="class")
print(pos)

trainx=train[,1:(pos-1)]
trainy=train[,pos]

head(trainx)
head(trainy)

testx=test[,1:(pos-1)]
testy=test[,pos]

#train[,1:(pos-1)]
#train[,-pos]


#random forest model
#mtry :- no. of feature in each tree
#mtree: -- no. of DT to build
m1=randomForest(trainx,trainy,mtry=2,ntree = 100)
?randomForest
#confusion matrix
m1$confusion
print(m1)
plot(m1)
table(trainy)
#predict(specify the testx as the test data)
p1=predict(m1,testx)
#confusion matrix 
confusionMatrix(testy,p1,positive = "1")

#######################################################################
#model 2 with dif HPT
m2=randomForest(trainx,trainy,mtry=3,ntree = 150)
?randomForest
#confusion matrix
m2$confusion
print(m2)
plot(m2)
#table(trainy)
#predict(specify the testx as the test data)
p2=predict(m2,testx)
#confusion matrix 
confusionMatrix(testy,p2,positive = "1")

####################################################
m3=randomForest(trainx,trainy,mtry=2,ntree = 200)
?randomForest
#confusion matrix
m3$confusion
print(m3)
plot(m3)
#table(trainy)
#predict(specify the testx as the test data)
p3=predict(m3,testx)
#confusion matrix 
confusionMatrix(testy,p3,positive = "1")

#important features
varImpPlot(m1)  #this librar can only be used in randomforest

#store imp features in dataframe
imp=data.frame('score'=importance(m1))
names(imp)
names(imp)='score'#renaming toscore
imp
imp$features=rownames(imp)
rownames(imp)=NULL
imp=imp[order(imp$score,decreasing = T),]
print(imp)


