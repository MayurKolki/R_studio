# Mayur Kolki PGA14
# In class Project = KNN + SVM + Naive Bayes + PCA -Class Assessment
# Dataset = Wine

library(caret)
library(corrplot)
library(caTools)
library(lattice)

wine = read.csv(file.choose(), header = T)
dim(wine)
str(wine)
View(wine)
colnames(wine)

table(wine$Customer_Segment)


# 1. Compute and plot those feature which are related to each other?
#--------------------------------------------------------------------

numc=names(wine)[sapply(wine,is.numeric)]
numc  

cor=cor(wine[numc])
corrplot(cor,method="number",type="lower")

# With the help of multicorrelation graph we can see how features are related to each other

# summarize the correlation matrix
correlationMatrix = cor(wine[,1:14])

# find attributes that are highly corrected (ideally >0.60)

highlyCorrelated = findCorrelation(correlationMatrix, cutoff=0.5)
highlyCorrelated

# customer_segment,Alcohol,proline,OD280,Hue,Total_phrnol,Flavanoids are highly correlated than other features which percentage is more than 60.




# 2.What are the optimum number of principal components in PCA?
#---------------------------------------------------------------

# feature scaling using the minmax()
minmax=function(x) return( (x-min(x)) / (max(x)-min(x)) )


# scale the dataset
winescale=as.data.frame(lapply(wine,minmax))
winescale$Customer_Segment=wine$Customer_Segment
View(winescale)

pos = grep('Customer_Segment', colnames(winescale))
pos

# apply the PCA
# --------------
pca=prcomp(winescale[-pos])
pca
summ = summary(pca)

# look under "porportion of variance" to get the % of variance explained
print(summ)

res1=t(data.frame(summ$importance))
View(res1)
expl_var = res1[,'Proportion of Variance']


screeplot(pca,col="brown",main="Principal Components")

df = data.frame(PC= paste0("PC",1:13), var_explained=expl_var)
df$PC=factor(df$PC, levels=paste0("PC",1:13))
df

ggplot(df,aes(x=PC,y=var_explained)) +
  geom_col(size=1,fill="white", colour="blue") +
  labs(title = "Scree Plot")

# build the PCA
wine_pca = as.data.frame(pca$x)
wine_pca = wine_pca[c(1,2,3)]
wine_pca$Customer_Segment = wine$Customer_Segment
View(wine_pca)


# "3" is optimum number of principal component
# PCA 1 = 40% , PCA 2 = 18% , PCA 3 = 8%
# Maximum information put in the first component, then maximum remaining information in the second and so on with others component.

# split the dataset into train and test
split=sample.split(wine_pca$Customer_Segment,SplitRatio = 0.8)
train=subset(wine_pca,split==TRUE)
test=subset(wine_pca,split==FALSE)
nrow(wine_pca); nrow(train); nrow(test)



# 3.Build a KNN classifier considering optimal number of principal components and value of K and state its score.
#----------------------------------------------------------------------------
library(class)
#split the data further into trainx/y testx/y
pos=grep('Customer_Segment',names(train))

trainx=train[,-pos]
trainy=train[,pos]
dim(trainx)
length(trainy)
length(trainy)
testx=test[,-pos]
testy=test[,pos]
dim(testx)
length(testy)

#cheking the K feasible
k=seq(3,11,2)
k
cv_acc=c()
for (i in k)
{
  model=knn.cv(trainx,trainy,k=i)
  #predication accuracy
  acc=length(which(model==trainy))/length(trainy)
  #store the acuuracy in the list
  cv_acc=append(cv_acc,acc)
}
print(cv_acc)

max(cv_acc)
which.max(cv_acc)

#the opmtimal neighbour is one that gives the highest predication accuracy
opt_k=k[which.max(cv_acc)]
print(paste('optimal neighbour =',opt_k))

#build the kNN model now
m1_KNN=knn(trainx,testx,trainy,k=opt_k)
#prediction is 
#p1=m1 beacues its a simaultaneous process in it

#confusion matrix
cm_knn = confusionMatrix(as.factor(testy),as.factor(m1_KNN))
cm_knn
table(testy)
#so in above code with just k=3 accuracy and prediction is average



# 4. Build a SVM Classifier and tune the hyperparameters to get the optimum model.
#------------------------------------------------------------------------------------

# shuffle the dataset
table(wine$Customer_Segment)

wine_pca = wine_pca[order(sample(seq(1,nrow(wine_pca)))),]
View(wine_pca)

# split the dataset into train and test
split=sample.split(wine_pca$Customer_Segment,SplitRatio = 0.8)
train=subset(wine_pca,split==TRUE)
test=subset(wine_pca,split==FALSE)
nrow(wine_pca); nrow(train); nrow(test)

View(train)
View(test)

library(e1071)
# build an SVM
model_svm=svm(Customer_Segment~., data=train, kernel='linear',
              type='C-classification')
prediction_svm = predict(model_svm,test[-4])
head(prediction_svm)
cm_svm = confusionMatrix(as.factor(test$Customer_Segment), as.factor(prediction))


# 5. Build a Naive Bayes Classifier and comment about its accuracy.
# ----------------------------------------------------------------
#Naive bayes classification

library(naivebayes)

#build the naive bayers model
View(train)
plot(train)#to know the distribution of data
#set parameter 'usekernel' = T in case if the numeric feature dont have a normal distribution
train$Customer_Segment=as.factor(train$Customer_Segment)
model_N=naive_bayes(Customer_Segment~.,data=train,usekernel = T)
print(model_N)


#predictions 
p1_n=predict(model_N,test)

#confusion matrix
cm_n = confusionMatrix(as.factor(test$Customer_Segment),as.factor(p1_n),positive = "1")
cm_n


# 6.Compare all of the models and justify your choice about the optimum model.
# ----------------------------------------------------------------------------


# Now store the data in one file for comparison..


# MOdel 1
df1 = as.data.frame(cm_knn$byClass)
View(df1)
df1$model = "KNN_Model"
df1$accuracy = cm_knn$overall[1]
View(df1)

# Model 2
df2 = as.data.frame(cm_svm$byClass)
View(df2)
df2$model = "SVM_Model"
df2$accuracy = cm_svm$overall[1]
View(df1)


# MOdel3
df3 = as.data.frame(cm_n$byClass)
View(df3)
df3$model = "Naive bayes_Model"
df3$accuracy = cm_n$overall[1]
View(df3)


df_all_model = cbind(df1,df2,df3)
View(df_all_model)
