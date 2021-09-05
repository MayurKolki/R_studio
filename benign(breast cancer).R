library(caret)
library(MASS)
library(corrplot)
library(ROCR)
path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/benign.csv"

bbd=read.csv(path,header=T,stringsAsFactors = T)

View(bbd)
head(bbd)
tail(bbd)

dim(bbd)

# check if data is all numeric
str(bbd)


#conert2Fact= function(df,cols)
#{
 # for (c in cols )
  #df[c]=as.factor(df[c])
#}

#df
#colnames(bbd)
#cols=c('stratum','degree','med_check','mst','diagnosis')
#conert2Fact(bbd,cols)

#convert the folowing features to factors
bbd$stratum =as.factor(bbd$stratum)
str(bbd$stratum)
#too many levels
bbd$stratum =as.integer(bbd$stratum)

bbd$diagnosis =as.factor(bbd$diagnosis)


bbd$observation = as.factor(bbd$observation)
str(bbd$observation)
bbd$degree =as.factor(bbd$degree)
str(bbd$degree)
bbd$med_check=as.factor(bbd$med_check)
str(bbd$med_check)
bbd$mst =as.factor(bbd$mst)
str(bbd$mst)


str(bbd)
#find the distribution of the class
table(bbd$diagnosis) #there is imbalance

#EDA

#1) numeric data

numc=names(bbd)[sapply(bbd,is.numeric)]
numc
factc=names(bbd)[sapply(bbd,is.factor)]
factc


#insert 
#cor=cor(bbd[numc])
#corplot(corr,method="number",type="lower")
# functions to check Nulls/0
checkNull=function(x) return(any(is.na(x)))
checkdot=function(x) return(any(x=="."))

# EDA
colnames(bbd)[apply(bbd,2,checkNull)]
colnames(bbd)[apply(bbd,2,checkdot)]

#split the data 
totalrows= nrow(bbd)
ss=sample(seq(1,totalrows),floor(0.7*totalrows))
train=bbd[ss,]
test=bbd[-ss,]
dim(train)
dim(test)
#build the model
m1=glm(diagnosis~.,data=train,
       binomial(link="logit"),control=list(maxit=200))

summary(m1)

#incase of warning during model building ,build the model with frature name to determine with feature causes the warning

colnames(bbd)
#observation
#mst
m1= glm(diagnosis~stratum+age+school+degree+med_check+agp1+agmn+nlv+liv+weight+aglp,data=train,
        binomial(link="logit"),control=list(maxit=200))
summary(m1)
unique(train$observation)





m1= glm(diagnosis~observation+mst,data=train,
        binomial(link="logit"))

str(bbd$mst)
str(train$mst)
typeof(bbd$mst)
typeof(train$mst)

#################################################
split.data.frame()





m1= glm(diagnosis~stratum+age+school+degree+med_check+agp1+agmn+nlv+liv+weight+aglp+mst,data=train,
     binomial(link="logit"),control=list(maxit=100))
summary(m1)


table(bbd$observation)
table(train$observation)

#build a model using stratum asthe only feature 
#m1= glm(diagnosis~observation,data=train, binomial(link="logit"))
#summary(m1)
#table(train$observation)
#table(bbd$observation)



#predict on model 1
p1=predict(m1,test,type="response") #response will give probability
nrow(test)
length(p1)
p1[1:10]

#convert prediction into classes
table(test$diagnosis)

cutoff=0.5

#length(p1[p1<0.5])
#length(p1[p1>0.5])

#predicted diagnosis
pred1= ifelse(p1<0.5,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred1),positive = "1")

#install.packages("e1071")

table(test$diagnosis)

#change the cutoff to see does it makes any difference
cutoff=0.4
#predicted diagnosis
pred1_2= ifelse(p1<0.4,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred1_2),positive = "1")

#try with 0.3
cutoff=0.3
#predicted diagnosis
pred1_3= ifelse(p1<0.3,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred1_3),positive = "1")

#try cutof 0.22
cutoff=0.22
#predicted diagnosis
pred1_4= ifelse(p1<0.22,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred1_4),positive = "1")

#to plot the curves
preds= prediction(p1,test$diagnosis)
#plor roc
pref=performance(preds,"tpr","fpr")
plot(pref,colorize=T,main = "ROC curve",
     ylab ="sensitivity",xlab ="1-specification")
abline(a=0,b=1,col='blue')
#area under the curve (auc)
auc= performance(preds,"auc")
round(unlist(slot(auc,"y.values")),3)


###########################################
colnames(bbd)
table(test$diagnosis)

m2= glm(diagnosis~age+degree+med_check+agp1+agmn+nlv+liv+weight+aglp+mst,data=train,
        binomial(link="logit"),control=list(maxit=100))

summary(m2)


table(bbd$observation)
table(train$observation)

#build a model using stratum asthe only feature 
#m1= glm(diagnosis~observation,data=train, binomial(link="logit"))
#summary(m1)
#table(train$observation)
#table(bbd$observation)



#predict on model 1
p2=predict(m1,test,type="response") #response will give probability
nrow(test)
length(p2)
p2[1:10]

#convert prediction into classes
table(test$diagnosis)

cutoff=0.5

#length(p1[p1<0.5])
#length(p1[p1>0.5])

#predicted diagnosis
pred2= ifelse(p1<0.5,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2),positive = "1")

#install.packages("e1071")

table(test$diagnosis)

#change the cutoff to see does it makes any difference
cutoff=0.4
#predicted diagnosis
pred2_2= ifelse(p2<0.4,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_2),positive = "1")

#try with 0.3
cutoff=0.3
#predicted diagnosis
pred2_3= ifelse(p2<0.3,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_3),positive = "1")

#try cutof 0.22
cutoff=0.22
#predicted diagnosis
pred2_4= ifelse(p2<0.22,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_4),positive = "1")

#to plot the curves
preds= prediction(p2,test$diagnosis)
#plor roc
pref=performance(preds,"tpr","fpr")
plot(pref,colorize=T,main = "ROC curve",
     ylab ="sensitivity",xlab ="1-specification")
abline(a=0,b=1,col='blue')
#area under the curve (auc)
auc= performance(preds,"auc")
round(unlist(slot(auc,"y.values")),3)

#######################################################

m3= glm(diagnosis~degree+med_check+agp1+agmn+nlv+liv+weight+aglp+mst,data=train,
        binomial(link="logit"),control=list(maxit=100))

summary(m2)


table(bbd$observation)
table(train$observation)

#build a model using stratum asthe only feature 
#m1= glm(diagnosis~observation,data=train, binomial(link="logit"))
#summary(m1)
#table(train$observation)
#table(bbd$observation)



#predict on model 1
p2=predict(m1,test,type="response") #response will give probability
nrow(test)
length(p2)
p2[1:10]

#convert prediction into classes
table(test$diagnosis)

cutoff=0.5

#length(p1[p1<0.5])
#length(p1[p1>0.5])

#predicted diagnosis
pred2= ifelse(p1<0.5,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2),positive = "1")

#install.packages("e1071")

table(test$diagnosis)

#change the cutoff to see does it makes any difference
cutoff=0.4
#predicted diagnosis
pred2_2= ifelse(p2<0.4,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_2),positive = "1")

#try with 0.3
cutoff=0.3
#predicted diagnosis
pred2_3= ifelse(p2<0.3,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_3),positive = "1")

#try cutof 0.22
cutoff=0.22
#predicted diagnosis
pred2_4= ifelse(p2<0.22,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_4),positive = "1")

#to plot the curves
preds= prediction(p2,test$diagnosis)
#plor roc
pref=performance(preds,"tpr","fpr")
plot(pref,colorize=T,main = "ROC curve",
     ylab ="sensitivity",xlab ="1-specification")
abline(a=0,b=1,col='blue')
#area under the curve (auc)
auc= performance(preds,"auc")
round(unlist(slot(auc,"y.values")),3)


#####

view(bbd)
bbd$newobs[bbd$observation==1] ="case"
bbd$newobs[bbd$observation%in%c(2,3,4)]= "control"

View(bbd$newobs)


bbd$newobs=as.factor(bbd$newobs)
View(bbd[,c('observation','newobs')])

table(bbd$newobs)

ss=sample(seq(1,totalrows),0.7*totalrows)
train=bbd[ss,]
test=bbd[-ss,]

m55= glm(diagnosis~newobs,data=train,
        binomial(link="logit"),control = list(maxit=100))

summary(m55)

#############
m2= glm(diagnosis~med_check+agp1+agmn+nlv+liv+weight+aglp+mst,data=train,
        binomial(link="logit"),control=list(maxit=100))

summary(m2)


table(bbd$observation)
table(train$observation)

#build a model using stratum asthe only feature 
#m1= glm(diagnosis~observation,data=train, binomial(link="logit"))
#summary(m1)
#table(train$observation)
#table(bbd$observation)



#predict on model 1
p2=predict(m1,test,type="response") #response will give probability
nrow(test)
length(p2)
p2[1:10]

#convert prediction into classes
table(test$diagnosis)

cutoff=0.5

#length(p1[p1<0.5])
#length(p1[p1>0.5])

#predicted diagnosis
pred2= ifelse(p1<0.5,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2),positive = "1")

#install.packages("e1071")

table(test$diagnosis)

#change the cutoff to see does it makes any difference
cutoff=0.4
#predicted diagnosis
pred2_2= ifelse(p2<0.4,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_2),positive = "1")

#try with 0.3
cutoff=0.3
#predicted diagnosis
pred2_3= ifelse(p2<0.3,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_3),positive = "1")

#try cutof 0.22
cutoff=0.22
#predicted diagnosis
pred2_4= ifelse(p2<0.22,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_4),positive = "1")

#to plot the curves
preds= prediction(p2,test$diagnosis)
#plor roc
pref=performance(preds,"tpr","fpr")
plot(pref,colorize=T,main = "ROC curve",
     ylab ="sensitivity",xlab ="1-specification")
abline(a=0,b=1,col='blue')
#area under the curve (auc)
auc= performance(preds,"auc")
round(unlist(slot(auc,"y.values")),3)

###################################################3
m2= glm(diagnosis~agp1+agmn+nlv+liv+weight+aglp+mst,data=train,
        binomial(link="logit"),control=list(maxit=100))

summary(m2)


table(bbd$observation)
table(train$observation)

#build a model using stratum asthe only feature 
#m1= glm(diagnosis~observation,data=train, binomial(link="logit"))
#summary(m1)
#table(train$observation)
#table(bbd$observation)



#predict on model 1
p2=predict(m1,test,type="response") #response will give probability
nrow(test)
length(p2)
p2[1:10]

#convert prediction into classes
table(test$diagnosis)

cutoff=0.5

#length(p1[p1<0.5])
#length(p1[p1>0.5])

#predicted diagnosis
pred2= ifelse(p1<0.5,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2),positive = "1")

#install.packages("e1071")

table(test$diagnosis)

#change the cutoff to see does it makes any difference
cutoff=0.4
#predicted diagnosis
pred2_2= ifelse(p2<0.4,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_2),positive = "1")

#try with 0.3
cutoff=0.3
#predicted diagnosis
pred2_3= ifelse(p2<0.3,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_3),positive = "1")

#try cutof 0.22
cutoff=0.22
#predicted diagnosis
pred2_4= ifelse(p2<0.22,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_4),positive = "1")

#to plot the curves
preds= prediction(p2,test$diagnosis)
#plor roc
pref=performance(preds,"tpr","fpr")
plot(pref,colorize=T,main = "ROC curve",
     ylab ="sensitivity",xlab ="1-specification")
abline(a=0,b=1,col='blue')
#area under the curve (auc)
auc= performance(preds,"auc")
round(unlist(slot(auc,"y.values")),3)
###########################################


m2= glm(diagnosis~agp1+agmn+liv+weight+aglp+mst,data=train,
        binomial(link="logit"),control=list(maxit=100))

summary(m2)


table(bbd$observation)
table(train$observation)

#build a model using stratum asthe only feature 
#m1= glm(diagnosis~observation,data=train, binomial(link="logit"))
#summary(m1)
#table(train$observation)
#table(bbd$observation)



#predict on model 1
p2=predict(m1,test,type="response") #response will give probability
nrow(test)
length(p2)
p2[1:10]

#convert prediction into classes
table(test$diagnosis)

cutoff=0.5

#length(p1[p1<0.5])
#length(p1[p1>0.5])

#predicted diagnosis
pred2= ifelse(p1<0.5,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2),positive = "1")

#install.packages("e1071")

table(test$diagnosis)

#change the cutoff to see does it makes any difference
cutoff=0.4
#predicted diagnosis
pred2_2= ifelse(p2<0.4,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_2),positive = "1")

#try with 0.3
cutoff=0.3
#predicted diagnosis
pred2_3= ifelse(p2<0.3,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_3),positive = "1")

#try cutof 0.22
cutoff=0.22
#predicted diagnosis
pred2_4= ifelse(p2<0.22,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_4),positive = "1")

#to plot the curves
preds= prediction(p2,test$diagnosis)
#plor roc
pref=performance(preds,"tpr","fpr")
plot(pref,colorize=T,main = "ROC curve",
     ylab ="sensitivity",xlab ="1-specification")
abline(a=0,b=1,col='blue')
#area under the curve (auc)
auc= performance(preds,"auc")
round(unlist(slot(auc,"y.values")),3)
############################################################

m6= glm(diagnosis~agp1+agmn+weight+aglp+mst,data=train,
        binomial(link="logit"),control=list(maxit=100))

summary(m6)


table(bbd$observation)
table(train$observation)

#build a model using stratum asthe only feature 
#m1= glm(diagnosis~observation,data=train, binomial(link="logit"))
#summary(m1)
#table(train$observation)
#table(bbd$observation)



#predict on model 1
p6=predict(m6,test,type="response") #response will give probability
nrow(test)
length(p6)
p6[1:10]

#convert prediction into classes
table(test$diagnosis)

cutoff=0.5

#length(p1[p1<0.5])
#length(p1[p1>0.5])

#predicted diagnosis
pred22= ifelse(p6<0.5,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred22),positive = "1")

#install.packages("e1071")

table(test$diagnosis)

#change the cutoff to see does it makes any difference
cutoff=0.4
#predicted diagnosis
pred2_2= ifelse(p2<0.4,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_2),positive = "1")

#try with 0.3
cutoff=0.3
#predicted diagnosis
pred2_3= ifelse(p2<0.3,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_3),positive = "1")

#try cutof 0.22
cutoff=0.22
#predicted diagnosis
pred2_4= ifelse(p2<0.22,0,1)

#confusion matrix and classification
confusionMatrix(test$diagnosis,as.factor(pred2_4),positive = "1")

#to plot the curves
preds= prediction(p2,test$diagnosis)
#plor roc
pref=performance(preds,"tpr","fpr")
plot(pref,colorize=T,main = "ROC curve",
     ylab ="sensitivity",xlab ="1-specification")
abline(a=0,b=1,col='blue')
#area under the curve (auc)
auc= performance(preds,"auc")
round(unlist(slot(auc,"y.values")),3)

######################################################3
#split the dataset into train/test
splitdata=function(df,size=0.7)
{
  ret=c()
  totalrows=nrow(df)
  ss=sample(seq(1,totalrows),floor(size*totalrows))
  train=df[ss,]
  test=df[-ss,]
  
  ret[1]=list(train)
  ret[2]=list(test)  
  return(ret)  
  
  
}
#function call 
ret=splitdata(bbd)
mytrain=data.frame(ret[1])
View(mytrain)
mytest=data.frame(ret[2])
View(mytest)


