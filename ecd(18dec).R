#random forest reg
#dataser:energy_cooling

library(corrplot)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(funModeling)
path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/energy_cooling_load.csv"

ecd=read.csv(path,header=T) #,stringsAsFactors = T)
View(ecd)
dim(ecd)
str(ecd)
###########################################################
#EDA
cols=names(ecd)
cols
cols=cols[-which(names(ecd)=="cold_load")]
cols
cor=cor(ecd[cols])
corrplot(cor,type = "lower",method = "number")

ecd1=ecd
ecd1$rl_ov=ecd1$rel_comp+ecd$over_ht
cols=names(ecd1)
cols
ecd1$rel_comp=NULL
ecd1$over_ht=NULL
print(cols)
cols=cols[-which(names(ecd1)=="cold_load")]
cols
ecd1$rel_comp=NULL
ecd1$over_ht=NULL
cor=cor(ecd1[cols])
corrplot(cor,type = "lower",method = "number")


#######################################################################

CheckZero=function(x) return(any(x==0))
checknull=function(x) return(any(is.na(x)))

names(ecd)[apply(ecd,2,CheckZero)]
names(ecd)[apply(ecd,2,checknull)]

#feq(ecd)
table(ecd$orient)
table(ecd$surf_area)
table(ecd$rel_comp)
#model 3: linear regression
#mode 2: decision tree 
#model 1: random forest


#split the data into train/test
total=nrow(ecd)
ss=sample(seq(1,total),floor(0.7*total))
train=ecd[ss,]
test=ecd[-ss,]

#model 1:-random forest 
#split train/test into trainx/traiy and testx/y
ndx=which(names(ecd)=="cold_load")
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

rf1=randomForest(trainx,trainy,ntree=75,mtry =2 )

p_rf1=predict(rf1,testx)
head(p_rf1)


#model 2:- decision tree
dt1=rpart(cold_load~.,data=train,method='anova')
p_dt1=predict(dt1,test)
head(p_dt1)

#model 3:- linear reg
lr1=lm(cold_load~.,data=train)
p_lr1=predict(lr1,test)
head(p_lr1)

#store predictions in dataframe for comparison
res=data.frame('actual'=testy,'rf'=round(p_rf1,2),'dt'=round(p_dt1,2),
               'lr'=round(p_lr1,2))
print(res)

#RMSE
e_rf=RMSE(testy,p_rf1)
e_dt=RMSE(testy,p_dt1)
e_lr=RMSE(testy,p_lr1)

data.frame('RMSE_RF'=e_rf,
           'RMSE_DT'=e_dt,
           'RMSE_lr'=e_lr)

