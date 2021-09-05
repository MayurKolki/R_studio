# load libraries
library(corrplot)
library(caret)
library(rpart)
library(rpart.plot)

path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/wheat.csv"

wheat=read.csv(path,header=T,stringsAsFactors = T)

View(wheat)



#eda
checkzero=function(x) return(any(x==0))
checknull=function(x) return(any(is.na(x)))
  names(wheat)[apply(wheat,2,checkzero)]
names(wheat)[apply(wheat,2,checknull)]

#corrplot (remove the y variable)
cols=names(wheat)
cols
cols=cols[-wheat(names(wheat=="type"))]
corr=cor(wheat[cols])

windows()
corrplot(corr,type='lower',method='number')

#lot of multicolinearity exists.
#challange is how to remove multicolinearity?
#feature engineering 
##################################################################
wheat1=wheat
wheat1$area=NULL
wheat1$perimeter=NULL
cols=names(wheat1)
cols=cols[-wheat(names(wheat1)=="type")]
corr=cor(wheat1[cols])
wheat$area_pm=wheat1$area+wheat1$perimeter
wheat1$
#remove the features 'area','perimeter'

windows()
corrplot(corr,type='lower',method='number')
View(wheat1)



#model building


#shuffle the dataset since it is arranged according to the wheat type

View(wheat)
wheat=wheat[sample(seq(1,nrow(wheat))),]

table(wheat$type)


#split the data 
# split the data into train and test
totalrows=nrow(wheat)
ss = sample(seq(1,totalrows),floor(0.8*totalrows))
train = wheat[ss,]
test = wheat[-ss,]

print(dim(train))
print(dim(test))

#build model
m1=rpart(type~.,data=train,method='class')

#visualize the tree
rpart.plot(m1,type=4,extra=101,tweak=1.5)

#important feature
m1$variable.importance


#prediction
p1=predict(m1,test,type='class')

#confusion matrix
confusionMatrix(as.factor(test$type),as.factor(p1))

table(test$type)
#check the cp and prune the tree
#compare the results of the actual model vs pruned mode

