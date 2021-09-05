#heart disease prediction
#decision tree
#in class project 23dec2020
library(corrplot)
library(class) #knn
#library()
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)

path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/HeartDisease.csv"

ht=read.csv(path,header=T) #,stringsAsFactors = T)

colnames(ht)
# EDA
CheckZero=function(x) return(any(x==0))
checknull=function(x) return(any(is.na(x)))

names(ht)[apply(ht,2,CheckZero)]
names(ht)[apply(ht,2,checknull)]

View(ht)

#shuffle the data
ht=ht[sample(seq(1,nrow(ht))),]
#ht_scale=ht_scale[sample(seq(1,nrow(ht_scale))),]


# correlation matrix and corrplot(remove the y- variable)

cols=names(ht)
cols= cols[-which(names(ht )=="target")]
corr = cor(ht[cols])
window()
corrplot(corr,type='lower',method='number')

############################################################################
# split the data into trail/test

total = nrow(ht)
ss = sample(seq(1,total),floor(0.7*total))
train=ht[ss,]
test=ht[-ss,]

# build the decisiion tree model

m1=rpart(target~., data=train, method='class')

# visualise
rpart.plot(m1,type=4,extra = 101, tweak = 2)

# important features
m1$variable.importance

# predict
p1=predict(m1,test,type='class')

# confusion matrix
confusionMatrix(as.factor(test$target), as.factor(p1), positive = "1")
table(test$target)
#check the cp and prune the tree

#compare the results of the actual model vs pruned mode
# plot the complexity parameter
plotcp(m1)

# complexity parameter
m1$cptable

mincp= m1$cptable[which.min(m1$cptable[,"xerror"]),"CP"]
m1_prune=prune(m1,mincp)
p1_prune=predict(m1_prune,test,type="class")
test$ht=as.factor(test$target)
cm1=confusionMatrix(as.factor(test$target),as.factor(p1_prune),positive="1")

#####################################################################
#model 1:-random forest 
#split train/test into trainx/traiy and testx/y
ndx=which(names(ht)=="target")
print(ndx)
trainx=train[,-ndx]
trainy=train[,ndx]

testx=test[,-ndx]
testy=test[,ndx]

dim(trainx)
dim(testx)

length(trainx)
length(testy)

head(trainx)
head(testx)

str(trainy)
trainy = as.factor(trainy)

rf1=randomForest(trainx,trainy,ntree=75,mtry =2) #,method ='class')

View(trainx)



p_rf1=predict(rf1,testx,type='class')
head(p_rf1)

#confusion matrix 
testy=as.factor(testy)
confusionMatrix(testy,p_rf1,positive = "1")
View(testy)



#######################################################

#model 3:- log
log1=glm(target~.,data=train,binomial(link="logit"))
summary(log1)

#prdiction
p_lr1=predict(log1,test,type='response')
p_lr1[1:10]

# predictions to be converted to classes
p_lr1[p_lr1>0.5]

#################################
length(p_lr1[p_lr1<0.5])
length(p_lr1[p_lr1>0.5])


#convert the probabilities to class 0 and 1
pred1=ifelse(p_lr1<0.5,0,1)



#####################

head(pred1)
summary(pred1)

table(test$target) #check actual and predicted with the help of table to verifty which is actual and predected


#confusion matrix and classification report
View(test$target)
str(test$target)
test$target=as.factor(test$target)
str(pred1)
pred1=as.factor(pred1)
confusionMatrix(as.factor(test$target),as.factor(pred1),positive = "1")

confusionMatrix(as.factor(p_lr1),test$target,positive="1")






df1=as.data.frame(cm1$byClass)
df1$model="decision tree"
df1$accuracy=cm1$overall[1]

df2=as.data.frame(cm2$byClass)
df2$model="random tree"




#store predictions in dataframe for comparison
res=data.frame('actual'=testy,'rf'=round(p_rf1,2),'dt'=round(p1_prune1,2),'log'=round(p_lr1,2))
print(res)

#RMSE
#cancel and do confusion martrix dataframe
e_rf=RMSE(testy,p_rf1)
e_dt=RMSE(testy,p_dt1)
e_log=RMSE(testy,p_lr1)

data.frame('RMSE_RF'=e_rf,
           'RMSE_DT'=e_dt,
           'RMSE_log'=e_log)



############################################################################


