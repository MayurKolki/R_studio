# simple linear regression
# dataset: CCSS (electric enegrgy production prediction)

path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/ccpps.csv"

ccpps=read.csv(path,header=T)
View(ccpps)
head(ccpps)
tail(ccpps)

dim(ccpps)

# check if data is all numeric
str(ccpps)

# functions to check Nulls/0
checkNull=function(x) return(any(is.na(x)))
checkZero=function(x) return(any(x==0))

# EDA
colnames(ccpps)[apply(ccpps,2,checkNull)]
colnames(ccpps)[apply(ccpps,2,checkZero)]

# check the distribution of X
hist(ccpps$temp,breaks=10,main="Histogram of Temperature",col="yellow")

# check for outliers
boxplot(ccpps$temp,horizontal=T,main="Boxplot for Temperature", col="red")

# split the data into train and test sets
totalrows = nrow(ccpps)
print(totalrows)

# generate 70% data
ss = sample(seq(1,totalrows),floor(0.7*totalrows))

# get the train and test data
train = ccpps[ss,]
test = ccpps[-ss,]
View(train)
print(paste("train=",dim(train),"test=",dim(test)))

head(train)
head(test)

# build the linear regression model
m1 = lm(elec_energy~temp, data=train)

# m1 = lm(elec_energy ~ .)
summary(m1)


#asump validatn
plot(m1)


#mean of residual is 0
mean(m1$residuals)


#test for hrtroscedasticity
#1 breush-pagan test
#install.packages("lmtest",dependencies = T)
#install.packages("car",dependencies = T)


#H0: homoscedastic
#H1: hetroscedastic
library(lmtest)
ret= bptest(m1)
if(ret$p.value<0.05)
  print('model is hetro') else
    print('model is homo')


#ncv(non constant variance) test
library(car)
ret=ncvTest(m1)
if(ret$p<0.05)
  print('model is hetro') else
    print('model is homo')


#cross validation
#k-
library(DAAG)
library(caret)

#cross validation
cv1 =cv.lm(data = train,m1,m=5)

#cal the rmse of the cv
print("RMSE of the cv")
RMSE(train$elec_energy,cv1$Predicted)



# predict the Y on the test data
p1 = predict(m1,test)
RMSE(test$elec_energy,p1)
p1[1:10]

# create a dataframe to store the actual Y and predicted Y
result=data.frame('actual'= test$elec_energy,
                  'predicted' = round(p1,2))

head(result,25)

# calculate the error/sse
result$err=result$actual-result$predicted
head(result)  
result$se = result$err^2

head(result)

# SSE
sse = sum(result$se)
sse
# MSE (mean squared errors)
mse = sse/nrow(test)
mse

# RMSE (root mean sq err)
rmse = sqrt(mse)
print(rmse)




#since the model is (m1) is hetroscedastic 
#do a box cox y variable
library(MASS)
bct= boxcox(elec_energy~temp,data=train)
print(bct)




#to get the optimal lambda value,

#1) find the max y
which(bct$y==max(bct$y))
#2) for the max y, get the corresponding lamda(x)
lambda=bct$x[which((bct$y==max(bct$y)))]

#transform the y variable  into boxcox transformation
#store the 2 value in train & test dataframe
train
train$y_bct=train$elec_energy^lambda

test
test$y_bct= test$elec_energy^lambda


head(train)
head(test)
# model 2

#x: as it is
#y: boxcox transformed value
m2=lm(y_bct~temp,data=train)





##model 2
#x: as it is
#y: boxcox transformed value
m2= lm(y_bct~temp,data=train) 
m2

summary(m2)

#check assumption
plot(m2)

ret= bptest(m2)
if(ret$p.value<0.05)
  print('model is hetro') else
    print('model is homo')


#ncv(non constant variance) test

ret=ncvTest(m2)
if(ret$p<0.05)
  print('model is hetro') else
    print('model is homo')
#predict
p2=predict(m2,test)
head(p2)

#predicted values are in boxcox format 
#need to re-transform this in the original form


p2[1]^(1/lambda)

df2=data.frame("actual"=test$elec_energy,"pred"=p2^(1/lambda))


#RMDE model M2

library(caret)
rmse2=RMSE(df2$actual,df2$pred)
cat("M1:RMSE=",rmse,"\nM2: RMSE=",rmse2)
#########
#how to tramsform x
#hypothesis test to determine normal distribution 
library(moments)

#agostaino-pearson test for normality
#h0= normal distribution
#h1= skewed distribution
at=agostino.test(ccpps$temp)
if(at$p.value<0.05)
  print("not a normal distribution") else
    print("normal distribution")

#transformation of x
#1) z-transformation
#2) min-max tras
#3) lod(n1,2,10)
#4)squareroot
#5)inverse transfn
#6) cuberoot
#7) etc.....

#steps
#1) transform daata
#2) check for normality
#3)build model +usual steps
#4)predict
#5) store RMSE(or other error)

cols=c("temp")




#transform data
trdata=data.frame(apply(train[cols],2,scale))
trdata$elec_energy= train$elec_energy

tedata=data.frame(apply(test[cols],2,scale))
tedata$elec_energy= test$elec_energy




#check the distribution
hist(trdata$temp)

at=agostino.test(trdata$temp)
if(at$p.value<0.05)
  print("not a normal distribution") else
    print("normal distributio")

m3=lm(elec_energy~temp,data=trdata)
p3=predict(m3,tedata)

rmse3=RMSE(tedata$elec_energy,p3)


cat("m1:RMSE=",rmse,"\nM2: RMSE=",rmse2,"\nM3:RMSE=",rmse3)








