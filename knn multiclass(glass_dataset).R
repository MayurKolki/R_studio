#knn classfication
#multiclass classification
#dataser:glass
library(class) #knn
library(corrplot)
library(rpart) #decision tree
library(rpart.plot)
library(caret) #confusion matrix
library(randomForest)
library(funModeling)
path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/glass.csv"

glass=read.csv(path,header=T)
View(glass)
dim(glass)
colnames(glass)

#remove unwanted features
glass$id_number=NULL
colnames(glass)
str(glass)

#eda
#standardize the data set
#minmax standar
minmax =function(x) return((x-min(x))/(max(x)-min(x)) )

#write the scaling function
#make a copy of the dataset to scale the data
glass_scale=glass

#scalin of data has to be done only on features
glass_scale=as.data.frame(lapply(glass_scale,minmax))
View(glass_scale)
glass_scale$Type=NULL

#summary gives min and maxc value
summary(glass_scale)
range(glass_scale)
glass_scale$Type=glass$Type
View(glass_scale)
#grep(glass_scale[,-5])
head(glass_scale)
head(glass)


#shuffle the data
glass=glass[sample(seq(1,nrow(glass))),]
glass_scale=glass_scale[sample(seq(1,nrow(glass_scale))),]
#split the data into train/test
total=nrow(glass_scale)
ss=sample(seq(1,total),floor(0.85*total))
train=glass_scale[ss,]
test=glass_scale[-ss,]
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





