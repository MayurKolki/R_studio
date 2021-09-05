#knn numeric regression#dataser:cars

library(class) #knn
library(corrplot)
library(rpart) #decision tree
library(rpart.plot)
library(caret) #confusion matrix #rmse
library(randomForest)
library(funModeling)
path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/cars.csv"

cars=read.csv(path,header=T)
View(cars)
dim(cars)
colnames(cars)

#remove unwanted features
cars$name=NULL
colnames(cars)
str(cars)

#eda
checkNull=function(x) return(any(is.na(x)))


# EDA
colnames(cars)[apply(cars,2,checkNull)]

#standardize the data set
#minmax standar
minmax =function(x) return((x-min(x))/(max(x)-min(x)) )

#write the scaling function
#make a copy of the dataset to scale the data
#scale the data set
cars_scale=cars
colnames(cars_scale)
pos=grep("mpg",names(cars_scale))
pos

cars_scale=as.data.frame(lapply(cars_scale[-pos],minmax))
View(cars_scale)
summary(cars_scale)

#add the y value to scaled data
cars_scale$mpg=cars$mpg


#split the data into train/test
total=nrow(cars_scale)
ss=sample(seq(1,total),floor(0.7*total))
train=cars_scale[ss,]
test=cars_scale[-ss,]
dim(train)
dim(test)

names(train)
#split the data further into trainx/y testx/y
pos=grep('mpg',names(train))

trainx=train[,-pos]
trainy=train[,pos]
dim(trainx)
length(trainy)
testx=test[,-pos]
testy=test[,pos]
dim(testx)
length(testy)

#to get the optimal value of k by crossvalidation
#in knn model is build and predicted happens at same time
#from this model we have find 

k=seq(3,20) #(min,max,separared by)
k  # k is hyperparameter

cv_rmse=c()
for(i in k)
{
  
  
  model=knnreg(trainx,trainy,k=i)
  pred=predict(model,testx)
  
  err=RMSE(testy,pred)
  
  #store the acc in list
  cv_rmse=append(cv_rmse,err)
  
}
#print the error
print(cv_rmse)
optk=k[which.min(cv_rmse)]
print(optk)

#build model using the opt k
m1=knnreg(trainx,trainy,k=optk)
p1=predict(m1,testx)

#check the actual and predicted values
cbind(actual=testy,pred=round(p1,1))

#asignnment using the same data set ,use linear regression, DT and RF to predict MPG 
#model 1:-random forest 
rf1=randomForest(trainx,trainy,ntree=75,mtry =2 )

p_rf1=predict(rf1,testx)
head(p_rf1)


#model 2:- decision tree
dt1=rpart(mpg~.,data=train,method='anova')
p_dt1=predict(dt1,test)
head(p_dt1)

#model 3:- linear reg
lr1=lm(mpg~.,data=train)
p_lr1=predict(lr1,test)
head(p_lr1)

#store predictions in dataframe for comparison
res=data.frame('actual'=testy,'knn'=round(p1,1), 'rf'=round(p_rf1,2),'dt'=round(p_dt1,2),'lr'=round(p_lr1,2))
print(res)

#RMSE
e_rf=RMSE(testy,p_rf1)
e_dt=RMSE(testy,p_dt1)
e_lr=RMSE(testy,p_lr1)

data.frame('RMSE_RF'=e_rf,
           'RMSE_DT'=e_dt,
           'RMSE_lr'=e_lr)






#STORE THE PREDICTION IN A DATA FRAME AAND 
#COMPARE ALL THE 4 RESULTS
#IDENTIFY THE BEST MODEL
#######################################################################
#######################################################################
##########################################################################
# the optimal neighbour is one that gives the highest prediction accuracy
max(cv_acc)
which.max(cv_acc)
opt_k=k[which.max(cv_acc)]
print(paste('optimal neighbour=',opt_k))

#build the knn model using the opt_k
p1=knn(trainx,testx,trainy,k=opt_k)

#build confusion matrix 
confusionMatrix(as.factor(testy),as.factor(p1))
table(testy)


#confusionMatrix(as,fact)
###########################################################


#split the data into train/test
total=nrow(glass)
ss=sample(seq(1,total),floor(0.85*total))
train=glass[ss,]
test=glass[-ss,]
dim(train)
dim(test)

#split the data further into trainx/y testx/y
pos=grep('Type',names(train))

trainx=train[,1:pos-1]
trainy=train[,pos]
dim(trainx)
length(trainy)
testx=test[,-pos]
testy=test[,pos]
dim(testx)
length(testy)

#to get the optimal value of k by crossvalidation
#in knn model is build and predicted happens at same time
#from this model we have find 

k=seq(3,11,2) #(min,max,separared by)
k  # k is hyperparameter

cv_acc=c()
for(i in k)
{
  
  
  model=knn.cv(trainx,trainy,k=i)
  
  #compare the test and the predict y
  
  #prediction accuracy
  acc=length(which(model==trainy))/length(trainy)
  
  #store the acc in list
  cv_acc=append(cv_acc,acc)
  
}

print(cv_acc)

# the optimal neighbour is one that gives the highest prediction accuracy
max(cv_acc)
which.max(cv_acc)
opt_k=k[which.max(cv_acc)]
print(paste('optimal neighbour=',opt_k))

#build the knn model using the opt_k
p1=knn(trainx,testx,trainy,k=opt_k)

#build confusion matrix 
confusionMatrix(as.factor(testy),as.factor(p1))
table(testy)

############################################################

?knn





