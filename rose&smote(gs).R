#SMOTE

#ROSE



library(caret)
library(MASS)
library(corrplot)
path="C:/Users/mayur/Desktop/datascience DELL/data analyst vedios/r language/machine learning/gs.csv"

gs=read.csv(path,header=T)

View(gs)
head(gs)
dim(gs)
colnames(gs)

table(gs$admit)
prop.table(table(gs$admit))

# check if data is all numeric
str(gs)

#split the data into train and test
total = nrow(gs)
ss=sample(seq(1,total),0.7*total)
train=gs[ss,]
test=gs[-ss,]

dim(train)
dim(test)

#check the prop in the train/test 
prop.table(table(train$admit))
prop.table(table(test$admit))

str(test)

#ROSE technique
library(ROSE)

t1= table(train$admit)
count = t1[1]

#oversampling 
over=ovun.sample(admit~.,data=train,
                  method="over",
                  N=count*2)$data
table(over$admit)

View(over)

#undersampling 
count =t1[2]
under=ovun.sample(admit~.,data=train,method="under",N=count*2)$data
table(under$admit)

#both under and oversampling 
#start with 50% and then tune it as per requirement 
bothunderovr=ovun.sample(admit~.,data=train,method="both",p=0.5,
                         N=total)$data
table(bothunderovr$admit)

View(bothunderovr)

mymodel = function(train,test)
{
  y='admit'
   f = as.formula(paste(y,"~."))
  
  #build the logistic regression model 
  m1=glm(f,data=train,binomial(link="logit"))

  #prdiction
  p1=predict(m1,test)
  p1[1:10]
  pred=ifelse(p1<0.5,0,1)
  cm=confusionMatrix(as.factor(unlist(test[y])),factor(pred))
  return(cm)

}

#run the logistic reg on the 4 train data
#train,under,over,both
c1=mymodel(train,test)
c2=mymodel(over,test)
c3=mymodel(under,test)
c4=mymodel(bothunderovr,test)


#compare the CM of all models and select the best
c1

#to study is it worth doing oversamplin undersampling

####SMOTE#####
library(DMwR)
#DMwr -> data mining with r

#y-varaible has to be a factor 
train$admit=as.factor(train$admit)

#bothsampling 
s_both = SMOTE(admit~.,data=train,perc.over = 100,perc.under = 200)

table(s_both$admit)

#undersampling 
s_under = SMOTE(admit~.,data=train,perc.under = 100)
table(s_under$admit)

#oversampling 
s_over = SMOTE(admit~.,data=train,perc.over = 200)
table(s_over$admit)


library(rpart)
#using the decision treee to predict on all 4 train sets
mymodel11 = function(train,test)
{
  y='admit'
  f = as.formula(paste(y,"~."))
  
  #build the logistic regression model 
  model11=rpart(f,data=train)
  
  #prdiction
  p11=predict(model11,test,type='class')
 
  cm1=confusionMatrix(as.factor(unlist(test[y])),factor(p11))
  return(cm1)
  
}

#build and predict
c11=mymodel11(train,test)
c22=mymodel11(s_over,test)
c33=mymodel11(s_over,test)
c44=mymodel11(s_both,test)

c11
c22
c33
c44
