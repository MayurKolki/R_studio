################# Decision Tree #####################
#### Classification ##############


library(caret)
library(ggplot2)
library(pROC)
library(ROCR)
library(funModeling)
library(calender)
library(lubridate)
path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/Taxi Fare Prediction/Dataset/TaxiFare.csv"
taxi=read.csv(path,header=T) #,stringsAsFactors = T)
View(taxi)

#bank_c=read.csv("bankchurn.csv",header = T)

datetime=data.frame(do.call("rbind",strsplit(as.character(taxi$date_time_of_pickup)," ",fixed = TRUE)))
View(datetime)


taxi1$datetime=datetime
View(taxi1)
colnames(taxi)

# write the dataframe into a file (.CSV format)
write.csv(taxi1,"Taxifare11.csv",row.names=F)

# file location - in the default working directory
getwd()
path="C:/Users/mayur/Desktop/RSTUDIO 1OCT2020/Taxifare11.csv"
taxi11=read.csv(path,header=T) #,stringsAsFactors = T)
colnames(taxi11)
View(taxi11)


#######################################
#not required
CheckZero=function(x) return(any(x==0))
checknull=function(x) return(any(is.na(x)))

p3=log(taxi11$longitude_of_pickup,10)
View(p3)
ztr=data.frame(apply(taxi11$longitude_of_pickup,2,scale))
hist(taxi11$longitude_of_pickup)


##########################################################

p1=(taxi11$longitude_of_pickup)^2
View(p1)
#names(p1)[apply(p1,2,CheckZero)]
#no. of zeros in longitude_of_pickup
length(which(p1<=0))
z1=which(p1<=0)
#m1= mean(taxi$longitude_of_pickup)
m1=mean(p1)
# update the ZERO values with the mean value
#taxi$longitude_of_pickup[z1] = m1
p1[z1]=m1
View(p1)
p11=sqrt(p1)
View(p11)
###################################################################

p2=(taxi11$latitude_of_pickup)^2
View(p2)
###names(p2)[apply(p2,2,CheckZero)]
#no. of zeros in longitude_of_pickup
length(which(p2<=0))
z2=which(p2<=0)
#m1= mean(taxi$longitude_of_pickup)
m2=mean(p2)
# update the ZERO values with the mean value
#taxi11$latitude_of_pickup[z2] = m2
p2[z2]=m2
View(p2)
p22=sqrt(p2)
View(p22)
###############################################################

p3=(taxi11$longitude_of_dropoff)^2
View(p3)
###names(p2)[apply(p2,2,CheckZero)]
#no. of zeros in longitude_of_pickup
length(which(p3<=0))
z3=which(p3<=0)
#m1= mean(taxi$longitude_of_pickup)
m3=mean(p3)
# update the ZERO values with the mean value
#taxi11$latitude_of_pickup[z2] = m2
p3[z3]=m3
View(p3)
p33=sqrt(p3)
View(p33)
###################################################################

p4=(taxi11$latitude_of_dropoff)^2
View(p4)
###names(p2)[apply(p2,2,CheckZero)]
#no. of zeros in longitude_of_pickup
length(which(p4<=0))
z4=which(p4<=0)
#m1= mean(taxi$longitude_of_pickup)
m4=mean(p4)
# update the ZERO values with the mean value
#taxi11$latitude_of_pickup[z2] = m2
p4[z4]=m4
View(p4)
p44=sqrt(p4)
View(p44)
###############################################
View(taxi11)

taxi11$longitude_of_pickup = NULL
taxi11$latitude_of_pickup = NULL
taxi11$longitude_of_dropoff = NULL
taxi11$latitude_of_dropoff = NULL

View(taxi11)


taxi11$longitude_of_pickup= p11
taxi11$latitude_of_pickup= p22
taxi11$longitude_of_dropoff= p33
taxi11$latitude_of_dropoff= p44

View(taxi11)

colnames(taxi)
colnames(taxi11)
str(taxi11)


taxi11$no_of_passenger=taxi$no_of_passenger
View(taxi11$no_of_passenger)
table(taxi11$no_of_passenger)
length(which(taxi11$mo_of_passenger<=0))
taxi11$no_of_passenger =as.numeric(taxi11$no_of_passenger)
summary(taxi11$no_of_passenger)



# identify the rows that have zero values for passenger
nullrow22= as.integer(rownames(taxi11[(taxi11$no_of_passenger)==0,]))
print(nullrow22)
# update the NULL values with the mean value
taxi11$no_of_passenger[nullrow22] = 1
# verify the update
taxi11$no_of_passenger[nullrow22]
View(taxi11)


###################################
taxi11$amount=abs(taxi$amount)
View(taxi11$amount)

length(which(taxi11$amount<=0))
View(taxi11)
taxi11$datetime.X3=NULL
View(taxi11)

library(corrplot)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(funModeling)


#########
#EDA
cols=names(taxi11)
cols
cols=cols[-which(names(taxi11)=="amount")]
cols
#implement cor plot
cor=cor(taxi11[4:7])
corrplot(cor,type = "lower",method = "number")


###########################################################
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
total=nrow(taxi11)
ss=sample(seq(1,total),floor(0.7*total))
train=taxi11[ss,]
test=taxi11[-ss,]

#model 1:-random forest 
#split train/test into trainx/traiy and testx/y
ndx=which(names(taxi11)=="amount")
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
print(p_rf1)
RMSE(testy,p_rf1)

#model 2:- decision tree
dt1=rpart(amount~.,data=train,method='anova')
p_dt1=predict(dt1,test)
head(p_dt1)


#store predictions in dataframe for comparison
res=data.frame('actual'=testy,'rf'=round(p_rf1,2),'dt'=round(p_dt1,2))
print(res)

#RMSE
e_rf=RMSE(testy,p_rf1)
e_dt=RMSE(testy,p_dt1)


data.frame('RMSE_RF'=e_rf,
           'RMSE_DT'=e_dt  )










########################################
###############################################################
names(ecd)[apply(ecd,2,checknull)]


library(tidyr)



colnames(taxi)
dim(taxi)
str(taxi)
View(taxi)
taxi1=taxi
colnames(taxi1)
taxi1$no_of_passenger=NULL
taxi1$date_time_of_pickup=NULL
taxi1$unique_id=NULL
taxi1$amount=NULL
View(taxi1)
minmax=function(x) return((x-min(x))/(max(x)-min(x)))

hist(taxi1$longitude_of_pickup)
mean(taxi1$longitude_of_pickup)

#standardize the data set

minmaxtaxi=as.data.frame(lapply(taxi1,minmax))
View(taxi1)
#ensure the svcalin has been done properly 
#data shd  lie between 0 and 1
summary(minmaxtaxi)
head(ctg)
head(ctg_scale)

#replace the scaled NSP with the original Y 
ctg_scale$NSP=ctg$NSP
View(ctg_scale)


#change the y to factor 
ctg_scale$NSP=as.factor(ctg_scale$NSP)
ctg$NSP=as.factor(ctg$NSP)



### EDA
# functions to check Nulls/0
checkNull=function(x) return(any(is.na(x)))
checkzero=function(x) return(any(x==0))

View(taxi$longitude_of_pickup)
# EDA
colnames(taxi)[apply(taxi,2,checkNull)]
colnames(taxi1$longitude_of_pickup)[apply(taxi1$longitude_of_pickup,2,checkzero)]

nc=names(taxi)[sapply(taxi,is.numeric)]
nc

which(is.na(taxi$longitude_of_pickup))

#h1n1$is_h1n1_risky    
table(taxi$longitude_of_pickup)
freq(taxi$longitude_of_pickup)
mean(taxi$longitude_of_pickup)
which(is.na(taxi$longitude_of_pickup))
length(which(is.na(h1n1$is_h1n1_risky)))
# identify the rows that have NULL values for abs_month
nullrow16= as.integer(rownames(taxi1[any(taxi1$)==0,]))=72
print(nullrow16)
# update the NULL values with the mean value
h1n1$is_h1n1_risky[nullrow16] = -72.50
# verify the update
h1n1$is_h1n1_risky[nullrow16]
str(h1n1$is_h1n1_risky)


# 1.) Factors- check Levels.

for(c in fc)
{
  print(c)
  
  print(levels(factor((unlist(bank_c[c])))))
  
  cat('\n')
}

bank_c$country[bank_c$country%in%c('Espanio','spain')]="Spain"

bank_c$country[bank_c$country%in%c('Ger','germany')]="Germany"

bank_c$country[bank_c$country%in%c('Fra','france')]="France"

table(bank_c$country)

bank_c$country=as.factor(bank_c$country)
str(bank_c$country)

## Gender -> merge the levels.

bank_c$gender[bank_c$gender%in%c('f','female','Female')]="F"
bank_c$gender[bank_c$gender%in%c('m','male','Male')]="M"

table(bank_c$gender)

bank_c$gender=as.factor(bank_c$gender)

str(bank_c$gender)


######## converting the Y variable into factor.
bank_c$churn=as.factor(bank_c$churn)

##### Checking the numeric columns.
# nulls
# zeros
# multicollinearity

nc
nc[apply(bank_c[nc],2,CheckZeros)]

## Check for the multicollinearity.
library(corrplot)
corr=cor(bank_c[nc])
corrplot(corr,method='number',type='lower') ## WE can see that there is no multicollinarity.

######## Now Split the data.
totalrows=nrow(bank_c)
ss=sample(seq(1,totalrows),floor(0.7*totalrows))
train=bank_c[ss,]
test=bank_c[-ss,]

colnames(train)
str(trai)


### For Decision Tree libraries are:
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

View(bank_c)

### Building the decision tree model.
# without any hyperparamter tuning.

# formula=y~x.
m1=rpart(churn~.,data=train,method="class") ## method=anova (for regression)

## Visualise the decision tree
rpart.plot(m1,type=4,extra=101,tweak = 3.0)
?rpart.plot ### check all the hyperparameters.

#### Predict
p1=predict(m1,test,type="class")
p1

#### COnfusion Matrix
confusionMatrix(test$churn,as.factor(p1),positive = "1")


table(test$churn)

### Complexity Parameter.
plotcp(m1)
m1$cptable

## Getting the minimum value of Cp corresponds to xerror

mincp=m1$cptable[which.min(m1$cptable[,"xerror"]),"CP"]

## prune the tree
m1_prune=prune(m1,mincp)

## predict on the prune model.
p1_prune=predict(m1_prune,test,type="class")

## confusion matrix on pruned predictions
confusionMatrix(test$churn,as.factor(p1_prune),positive = "1")

### check if the post-pruned model prediction are better than the pre-pruned model.

#1.) both model are same-then does not matter if the tree is pruned.

#2.) post-pruned model gives better result than the pre-pruned model-> retain the post-pruned model.

data.frame(score=m1$variable.importance)

################ NOW build the new model using hyperparameter tunning.

?rpart.control 

colnames(bank_c)
m2=rpart(churn~creditscore+country+gender+age+tenure+balance+active+salary,
         data=train,method="class",minsplit=200,
         cp=0.05,
         maxdepth=2)

## Visualise the tree
rpart.plot(m2,type=4,extra=101,tweak=1.5)

## predict
p2=predict(m2,test,type="class")

table(test$gender)
table(train$gender)


str(test$gender)
str(train$gender)
str(bank_c$gender)

## confusion matrix on pruned predictions
confusionMatrix(test$churn,as.factor(p2),positive = "1")



#implementation weekdays and weekends-------------------------------------------
View(taxi11)
names(taxi11)

table(taxi11$weekdays)

row_sat=which(taxi11$weekdays=='Saturday')
row_sun=which(taxi11$weekdays=='Sunday')
row_mon=which(taxi11$weekdays=='Monday')
row_tue=which(taxi11$weekdays=='Tuesday')
row_wed=which(taxi11$weekdays=='Wednesday')
row_thur=which(taxi11$weekdays=='Thursday')
row_fri=which(taxi11$weekdays=='Friday')



length(row_sat)
length(row_sun)
length(row_mon)
length(row_tue)
length(row_wed)
length(row_thur)
length(row_fri)

taxi11$A=""


taxi11$A[row_sat]="weekend"
taxi11$A[row_sun]="weekend"
taxi11$A[row_mon]="weekday"
taxi11$A[row_tue]="weekday"
taxi11$A[row_wed]="weekday"
taxi11$A[row_thur]="weekday"
taxi11$A[row_fri]="weekday"
View(taxi11)

str(taxi11)

table(taxi11$datetime.X2)
View(taxi11)




#If(AND(F2>06:00:00,F2<11:00:00),"Morning",if(AND(F2>11:00:00,F2<17:00:00),"Business hours",if(AND(F2>17:00:00,F2<24:00:00),"Party hours","Night hours")))







