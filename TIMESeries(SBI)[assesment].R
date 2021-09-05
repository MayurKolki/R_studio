###  Mayur kolki->PGA14

#Time series forecast using ARIMA
#dataset:SBI stock forecast 
library(tseries)
library(forecast)
library(zoo)

#sbi=read.csv(file.choose(), header = T)

path="C:/Users/mayur/Desktop/datascience DELL/mayurstuff/TimeSeries/Time Series/SBI_Historical_Data.csv"

sbi=read.csv(path,header=T)
View(sbi)
####Q.1 As part of EDA, perform the following tasks:
#a)Print dimensions of the data
dim(sbi)
#b)Dimensions of Dataset
colnames(sbi)
#c)Statistical Summary
summary(sbi)
#d)Converting Date
sbi$ï..Date=strptime(sbi$ï..Date,"%B %d,%Y")
sbi$ï..Date=as.Date(sbi$ï..Date)
#e)Check Data Type and Missing Value
View(sbi)
str(sbi)
head(sbi)
anyNA(sbi)

#f)Index the dataset with Date
library(dplyr )
sbi= arrange(sbi,sbi$ï..Date)
#sbi$ï..Date = sort(sbi$ï..Date)

View(sbi)



data.frame(colSums(is.na(sbi)))

#Q.2 Perform time series analysis:

price= sbi$Price
#a)Visualize time series data //
plot(price,type='l',col='red')

#convert the 'close price' into a TS format
tsCP=ts(sbi$Price,start=c(2015,1),end = c(2020,8),freq =12)
print(tsCP)
?ts

#####c)Perform decomposing
#get the individual components 
plot(decompose(tsCP))
plot(tsCP)

#b)Check Stationarity with:
####- ADF Test
#Augmented Dicky-Fuller test to determine the stationarity of the PR
pr=sbi$Price
print(pr)
adftest=adf.test(pr)
pvalue=adftest$p.value
print(pvalue)
result=ifelse(pvalue<0.05,"stationary","not stationary")
print(result)
pr[1:20]



#de-trend the cp
pr1=diff(pr)
pr1[1:20]
adftest=adf.test(pr1)
pvalue=adftest$p.value
print(pvalue)
result=ifelse(pvalue<0.05,"stationary","not stationary")
print(result)

#strong evidence against the null hypothesis(Ho), reject the null hypothesis. Data has no unit root and is stationary
#weak evidence against null hypothesis, time series has a unit root, indicating it is non-stationary 

plot(pr1,type='l')

#using the ACF and PACF graphs, determine the p,q values
pacf(pr1) #AR (P)
acf(pr1)  #MV (Q)

pacf(price)
acf(price)
#based on the correlogram p and q=0


#run the auto arima determine the best p,q,d I= integratedintegrated autucorelation moving avg  
auto.arima(pr1)
auto.arima(price)
#(p,d,q) --> 0,0,0

#best model  is determined by the least value of AIC(Akike Information Criteria)

#build the model 1
p=0;d=0;q=0

m1=arima(pr,order=c(p,d,q))
summary(m1)
#to get the AIC of the model
aic1=m1$aic
print(paste('AIC of model1=',aic1))

#the best model is the one that has the least AIC(akike info criteria) score for the given combination of p,d,q

#determine the errors for IID
m1_err=m1$residuals
hist(m1_err,col='yellow',breaks=10)

#LJung-box test on 
res.test=Box.test(m1_err,type="Ljung-Box")
res=ifelse(res.test$p.value<0.05,"Bad Model","Good Model")
print(res)

#Q.3 Forecast about the stock price using 

#forecast for the next 12 months
p1=forecast(m1,h=12)
plot(p1)

print(p1)

length(pr1)

pr1[1384]
price[1384:1385]

################################################
#pacf(pr) #AR (P)
#acf(pr)  #MV (Q)


#build the model 1
p=5;d=0;q=0

m2=arima(pr1,order=c(p,d,q))
summary(m2)
#to get the AIC of the model
aic2=m2$aic
print(paste('AIC of model1=',aic2))

#the best model is the one that has the least AIC(akike info criteria) score for the given combination of p,d,q

#determine the errors for IID
m2_err=m2$residuals
hist(m2_err,col='yellow',breaks=10)

#LJung-box test on 
res.test=Box.test(m2_err,type="Ljung-Box")
res=ifelse(res.test$p.value<0.05,"Bad Model","Good Model")
print(res)
res.test

#forecast for the next 12 months
p2=forecast(m2,h=12)
plot(p2)

print(p2)

length(pr1)

pr1[1384]
sbi$Price[1384]

price[1384:1385]




######################################################





