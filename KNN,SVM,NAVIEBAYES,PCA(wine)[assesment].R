##Mayur Kolki-> PGA14
#####Class Assessment
###KNN + SVM + Naive Bayes + PCA -
 ## Dataset: wine

path="C:/Users/mayur/Desktop/datascience DELL/mayurstuff/KNN+SVM+NaiveBayes+PCA/KNN + SVM + Naive Bayes + PCA/Wine.csv"

wine1=read.csv(path,header=T)

View(wine1)
dim(wine1)
head(wine1)
anyNA(wine1)


library(caTools)
library(e1071)
library(caret)



#View(wine)

# y-variable (customer_segment) categorises customers based on various parameters
# for a given wine, predict to which customer segment this wine has to be recommended

# how to plot all these variables in a graph to show the segmentation
# use PCA to extract the 2 most (new) important features that can explain the maximum variation in the dataset
# prediction region and prediction boundary can then be viewed from this reduced dimensions

# these newly extracted features are called PRINCIPAL COMPONENTS

length(colnames(wine1))

# feature scaling using the minmax()
minmax=function(x) return( (x-min(x)) / (max(x)-min(x)) )

#pos = grep('Customer_Segment', colnames(wine))
#wine_scale = wine[,c(1:pos-1)]
#wine_scale=as.data.frame(lapply(wine_scale,minmax))
#wine_scale[pos]=wine[pos]
#View(wine_scale)

# scale the dataset
winescale=as.data.frame(lapply(wine1,minmax))
winescale$Customer_Segment=wine1$Customer_Segment
View(winescale)

pos = grep('Customer_Segment', colnames(winescale))
pos

# apply the PCA
# --------------
pca=prcomp(winescale[-pos])
# pca
summ = summary(pca)
# look under "porportion of variance" to get the % of variance explained
print(summ)

res1=t(data.frame(summ$importance))
View(res1)
expl_var = res1[,'Proportion of Variance']

# explained variation
# expl_var = (summ$sdev^2)/sum( (summ$sdev)^2)

screeplot(pca,col="brown",main="Principal Components")


# Yes, rotation (orthogonal) is necessary because it maximizes the difference between variance captured by the component. This makes the components easier to interpret. Not to forget, that's the motive of doing PCA where, we aim to select fewer components (than features) which can explain the maximum variance in the data set. By doing rotation, the relative location of the components doesn't change, it only changes the actual coordinates of the points.

# If we don't rotate the components, the effect of PCA will diminish and we'll have to select more number of components to explain variance in the data set.


df = data.frame(PC= paste0("PC",1:13), var_explained=expl_var)
df$PC=factor(df$PC, levels=paste0("PC",1:13))
df
str(df)

ggplot(df,aes(x=PC,y=var_explained)) +
  geom_col(size=1,fill="white", colour="blue") +
  labs(title = "Scree Plot")


ggplot(df,aes(x=PC,y=var_explained)) +
  geom_bar(stat="identity", colour="black",fill="violet") +
  labs(title = "Scree Plot")



# build the PCA
wine_pca = as.data.frame(pca$x)
wine_pca = wine_pca[c(1,2)]
wine_pca$Customer_Segment = wine1$Customer_Segment
View(wine_pca)


# ---------------------------------------------
# from here, build any classification model
# ----------------------------------------------

# shuffle the dataset
wine_pca = wine_pca[order(sample(seq(1,nrow(wine_pca)))),]
View(wine_pca)

# split the dataset into train and test
split=sample.split(wine_pca$Customer_Segment,SplitRatio = 0.8)
train=subset(wine_pca,split==TRUE)
test=subset(wine_pca,split==FALSE)
nrow(wine_pca); nrow(train); nrow(test)

View(train)
View(test)


# build an SVM
model=svm(Customer_Segment~., data=train, kernel='linear',
          type='C-classification')
prediction = predict(model,test[-3])
confusionMatrix(as.factor(test$Customer_Segment), as.factor(prediction))


# visualize the results
# ----------------------

# install.packages("ElemStatLearn")
 #  library(ElemStatLearn) 9feb -

set=train
X1=seq(min(set[,1])-1,max(set[,1])+1, by=0.1) 
X2=seq(min(set[,2])-1,max(set[,2])+1, by=0.1) 
grid_set = expand.grid(X1,X2)
colnames(grid_set)=c('PC1','PC2')
y_grid = predict(model,newdata = grid_set)
length(y_grid)

plot(set[,-3],
     main="SVM Classification",
     xlab='PC1', ylab='PC2',
     xlim=range(X1), ylim=range(X2))

contour(X1,X2,matrix(as.numeric(y_grid),length(X1),length(X2)),add=T)

points(grid_set,pch='.',col=ifelse(y_grid==2,'deepskyblue', ifelse(y_grid==1, 'springgreen3','tomato') ))
points(set,pch=21,bg=ifelse(set[,3]==2, 'blue3', ifelse(set[,3]==1, 'green4','red3')))


###############################################################################

#Naive bayers classification

library(naivebayes)
library(caret)
library(MASS)
library(corrplot)
path="C:/Users/mayur/Desktop/datascience DELL/mayurstuff/KNN+SVM+NaiveBayes+PCA/KNN + SVM + Naive Bayes + PCA/Wine.csv"

wine2=read.csv(path,header=T)

View(wine2)
head(wine2)
dim(wine2)
colnames(wine2)


# check if data is all numeric
str(wine2)

#convert the data into factors
wine2$admit=as.factor(wine2$admit)
wine2$prestige=as.factor(wine2$prestige)


#EDA
anyNA(wine2)
checkNull=function(x) return(any(is.na(x)))
checkZero=function(x) return(any(x==0))

# EDA
colnames(wine2)[apply(wine2,2,checkNull)]
colnames(wine2)[apply(wine2,2,checkZero)]

#check for nulls
#no multicolinearity
#only relevant features (x) shd be present

#visualisation
ggplot(wine2,aes(x=admit,y=gpa,fill=admit))+geom_boxplot()

table(wine2$admit)
prop.table(table(gs$admit))

#split the data into train and test
total = nrow(gs)
ss=sample(seq(1,total),0.7*total)
train=gs[ss,]
test=gs[-ss,]

dim(train)
dim(test)

#build the naive bayers model

#set parameter 'usekernel' = T in case if the numeric feature dont have a normal distribution

m1=naive_bayes(admit~.,data=train,usekernel = T)
print(m1)


#predictions 
p1=predict(m1,test)

#confusion matrix
confusionMatrix(test$admit,p1,positive = "1")

#########################################################################
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
path="C:/Users/mayur/Desktop/datascience DELL/mayurstuff/KNN+SVM+NaiveBayes+PCA/KNN + SVM + Naive Bayes + PCA/Wine.csv"

wine3=read.csv(path,header=T)
View(wine3)
dim(wine3)
colnames(wine3)

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
wine_scale=wine3

#scalin of data has to be done only on features
wine_scale=as.data.frame(lapply(wine_scale,minmax))
View(wine_scale)
wine_scale$Customer_Segment=NULL

#summary gives min and maxc value
summary(wine_scale)
range(wine_scale)
wine_scale$Customer_Segment=wine3$Customer_Segment
View(wine_scale)
#grep(glass_scale[,-5])
head(wine_scale)
head(wine3)


#shuffle the data
wine3=wine3[sample(seq(1,nrow(wine3))),]
wine_scale=wine_scale[sample(seq(1,nrow(wine_scale))),
]
#split the data into train/test
total=nrow(wine_scale)
ss=sample(seq(1,total),floor(0.85*total))
train=wine_scale[ss,]
test=wine_scale[-ss,]
dim(train)
dim(test)

#split the data further into trainx/y testx/y
pos=grep('Customer_Segment',names(train))

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


