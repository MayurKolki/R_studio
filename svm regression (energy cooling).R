#svm regression
#dataser:energy cooling


library(caret)
library(MASS)
library(corrplot)
library(ROCR)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(e1071)   #svm
path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/energy_cooling_load.csv"

energy=read.csv(path,header=T) #,stringsAsFactors = T)
#file.choose()

#eda
#split the data into train and test
total=nrow(energy)
ss=sample(seq(1,total),floor(0.7*total))
train=energy[ss,]
test=energy[-ss,]
dim(train)
dim(test)
?svm

names(train)
#regression type ='EPS-regression'
#4 kernels->linear, radial,sigmoid,polynomial
r_type='eps-regression'
m1=svm(cold_load~.,data=train,type=r_type,
       kernel='linear',
       cross=10)
summary(m1)
p1=predict(m1,test)

cbind(test$cold_load,p1)
rmse1=RMSE(test$cold_load,p1)

#visualisation the result
ggplot(test,aes(cold_load,p1))+
  geom_point(colour='green')+
  geom_smooth(colour='red',method=lm)+
  ggtitle('EPS/Linear/C=1')

#hyperparameter tunning (change the cost)=> (c)cost=2
m2=svm(cold_load~.,data=train,type=r_type,
       kernel='linear',
       cross=10,
       cost=2)
summary(m2)
p2=predict(m2,test)
rmse2=RMSE(test$cold_load,p2)

cat("RMSE1=",rmse1,"\n","RMSE2=",rmse2)


#regression type ='EPS-regression'
#4 kernels->linear, radial,sigmoid,polynomial
#kernel='radial'

r_type='eps-regression'
m3=svm(cold_load~.,data=train,type=r_type,
       kernel='radial',
       cross=10)
summary(m3)
p3=predict(m3,test)
rmse3=RMSE(test$cold_load,p3)
cat("RMSE1=",rmse1,"\n","RMSE2=",rmse2,"\nRNSE3=",rmse3)

#regression type ='EPS-regression'
#4 kernels->linear, radial,sigmoid,polynomial
#kernel='radial'

r_type='eps-regression'
m4=svm(cold_load~.,data=train,type=r_type,
       kernel='sigmoid',
       cross=10)
summary(m4)
p4=predict(m4,test)
rmse4=RMSE(test$cold_load,p4)
cat("RMSE1=",rmse1,"\n","RMSE2=",rmse2,"\nRNSE3=",rmse3,"\nRMSE4=",rmse4)

#regression type ='EPS-regression'
#4 kernels->linear, radial,sigmoid,polynomial
#kernel='polynomial'

r_type='eps-regression'
m5=svm(cold_load~.,data=train,type=r_type,
       kernel='polynomial',
       cross=10)
summary(m5)
p5=predict(m5,test)
rmse5=RMSE(test$cold_load,p5)
cat("RMSE1=",rmse1,"\n","RMSE2=",rmse2,"\nRNSE3=",rmse3,"\nRMSE4=",rmse4,"\nRMSE5=",rmse5)

#2) regresion type='nu-regression'
#4 kernels->linear, radial,sigmoid,polynomial
r_type='nu-regression'
m6=svm(cold_load~.,data=train,type=r_type,
       kernel='linear',
       cross=10)
summary(m6)
p6=predict(m6,test)
rmse6=RMSE(test$cold_load,p6)
cat("RMSE1=",rmse1,"\n","RMSE2=",rmse2,"\nRNSE3=",rmse3,"\nRMSE4=",rmse4,"\nRMSE5=",rmse5)
cat("RMSE(linear kernel,c=1)\nEPS="rmse1,"\nNU="rmse6)


#regresion type='nu-regression'
#4 kernels->linear, radial,sigmoid,polynomial
#kernel='radial'

r_type='nu-regression'
m7=svm(cold_load~.,data=train,type=r_type,
       kernel='radial',
       cross=10)
summary(m7)
p7=predict(m7,test)
rmse7=RMSE(test$cold_load,p7)
cat("RMSE1=",rmse1,"\n","RMSE2=",rmse2,"\nRNSE3=",rmse3)

#regresion type='nu-regression'
#4 kernels->linear, radial,sigmoid,polynomial
#kernel='sigmoid'

r_type='nu-regression'
m8=svm(cold_load~.,data=train,type=r_type,
       kernel='sigmoid',
       cross=10)
summary(m8)
p8=predict(m8,test)
rmse8=RMSE(test$cold_load,p8)
cat("RMSE1=",rmse1,"\n","RMSE2=",rmse2,"\nRNSE3=",rmse3,"\nRMSE4=",rmse4)

#regression type ='nu-regression'
#4 kernels->linear, radial,sigmoid,polynomial
#kernel='polynomial'

r_type='nu-regression'
m9=svm(cold_load~.,data=train,type=r_type,
       kernel='polynomial',
       cross=10)
summary(m9)
p9=predict(m9,test)
rmse9=RMSE(test$cold_load,p5)
cat("RMSE1=",rmse1,"\n","RMSE2=",rmse2,"\nRNSE3=",rmse3,"\nRMSE4=",rmse4,"\nRMSE5=",rmse5)

###########################################################
buildSVM=function(rtype,ker,C=1,train,test,Y)
{
  f=as.formula(paste(Y,"~."))
  
  model=svm(f,data=train,cost=C,
            type=rtype,
            kernel=ker)
  

  pred=predict(model,test)
  
  rmse=RMSE(unlist(test[Y]),pred)
  
  return(rmse)
  
}

buildSVM(rtype='nu-regression',ker='linear',train=train,test=test,Y='cold_load')


#test['cold_load']
#Y="cold_load"
#formula=as.formula()

#as.formula(paste(Y,"~."))
#svm(as.formula(paste(Y,"~.")),data=train,type=r_type,)
#######################################################################
#sinle function cll to build and predict the different types od SVM regression models
#function call

rtype=c('nu-regression','eps-regression')
ktype=c('linear','radial','sigmoid','polynomial')
for(rt in rtype)
{
  for(kt in ktype)
  {
    err=buildSVM(rtype=rt,ker=kt,
                 train=train,test=test,Y='cold_load')
    
    cat("Regression type=",rt,"kernel=",kt,"RMSE=",err)
    cat("\n")
    
    
  }
  
}




#########
m11=buildSVM(rtype='nu-regression',ker='radial',train=train,test=test,Y='cold_load')

m11





#regression type='nu-regression'










