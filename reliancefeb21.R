#Time series forecast using ARIMA
#dataset:reliance stock forecast 
library(tseries)
library(forecast)
library(zoo)
rel=read.csv(file.choose(), header = T)
dim(rel)
View(rel)

head(rel)
anyNA(rel)
str(rel)
colnames(rel)


# na.rm = T does not consider the null values for calculations
rel$Open=as.numeric(rel$Open,na.rm=T)
rel$High=as.numeric(rel$High,na.rm=T)
rel$Low=as.numeric(rel$Low,na.rm=T)
rel$Closeprice=as.numeric(rel$Closeprice,na.rm=T)
rel$Adj..closeprice=as.numeric(rel$Adj..closeprice,na.rm=T)
rel$Volume=as.numeric(rel$Volume,na.rm=T)

str(rel)

# functions to check Nulls/0
checkNull=function(x) return(any(is.na(x)))
checkZero=function(x) return(any(x==0))

# EDA
colnames(rel)[apply(rel,2,checkNull)]
colnames(rel)[apply(rel,2,checkZero)]
colnames(rel)
data.frame(colSums(is.na(rel)))

nullrow= as.integer(rownames(rel[is.na(rel['Open']),]))
print(nullrow)

nullrow1= as.integer(rownames(rel[is.na(rel['High']),]))
print(nullrow1)


nullrow2= as.integer(rownames(rel[is.na(rel['Low']),]))
print(nullrow2)

nullrow3= as.integer(rownames(rel[is.na(rel['Closeprice']),]))
print(nullrow3)

nullrow4= as.integer(rownames(rel[is.na(rel['Adj..closeprice']),]))
print(nullrow4)

nullrow5= as.integer(rownames(rel[is.na(rel['Volume']),]))
print(nullrow5)

rel=rel[-c(195),]
View(rel)


rel$Date=as.Date(rel$Date)

str(rel)



#lets predict 'close  price'
closeprice= rel$Closeprice
plot(closeprice,type='l',col='red')

#convert the 'close price' into a TS format
tsCP=ts(rel$Closeprice,start=c(2020,2),end = c(2021,2),freq =12)
print(tsCP)
?ts

#get the individual components 
plot(decompose(tsCP))
plot(tsCP)
#Augmented Dicky-Fuller test to determine the stationarity of the  CP
cp=rel$Closeprice
print(cp)
adftest=adf.test(cp)
pvalue=adftest$p.value
print(pvalue)
result=ifelse(pvalue<0.05,"stationary","not stationary")
print(result)
cp[1:20]


#plot(rel$Closeprice)
#abline(reg=lm(rel$Closeprice~time(rel$Closeprice)))
#plot(diff(log(rel$Closeprice)))

#de-trend the cp
cp=diff(cp)
cp[1:20]
adftest=adf.test(cp)
pvalue=adftest$p.value
print(pvalue)
result=ifelse(pvalue<0.05,"stationary","not stationary")
print(result)

#strong evidence against the null hypothesis(Ho), reject the null hypothesis. Data has no unit root and is stationary
#weak evidence against null hypothesis, time series has a unit root, indicating it is non-stationary 

plot(cp,type='l')

#using the ACF and PACF graphs, determine the p,q values
pacf(cp) #AR (P)
acf(cp)  #MV (Q)

pacf(closeprice)
acf(closeprice)
#based on the correlogram p and q=0


#run the auto arima determine the best p,q,d I= integratedintegrated autucorelation moving avg  
auto.arima(cp)
auto.arima(closeprice)
#(p,d,q) --> 0,0,0

#best model  is determined by the least value of AIC(Akike Information Criteria)

#build the model 1
p=0;d=0;q=0

m1=arima(cp,order=c(p,d,q))
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

library(TTR)
Holtwinter(rel)
Holtwinters(rel)
HoltWinters(tsCP)

#forecast for the next 6 months
p1=forecast(m1,h=6)
plot(p1)

print(p1)

length(cp)

cp[249]
closeprice[249:250]

################################################
#pacf(cp) #AR (P)
#acf(cp)  #MV (Q)


#build the model 1
p=5;d=0;q=0

m1=arima(cp,order=c(p,d,q))
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
res.test

#forecast for the next 6 months
p1=forecast(m1,h=6)
plot(p1)

print(p1)

length(cp)

cp[249]
rel$Closeprice[249]
1924.3 +(-0.55)
1923.75 +5.066456
closeprice[249:250]




######################################################

#now check for best combination of p,q,d.

p = seq(1,3)
p
d = 0
q = seq(1,3)
q
for (i in c(p,d,q))
{
  model = arima(cp,order = i)
  aic = model$aic
}
?arima

for (i in c(p,d,q))
{
  model = arima(cp,order = c)
  print(model)
}

########################################

res_p = c()
res_q = c()
res_aic = c() ; d=0

for(p in seq(0,1))
{
  for(q in seq(0,3))
  {
    m2 = arima(cp , order = c(p,d,q))
    res_p = append(res_p,p)
    res_q = append(res_q,q)
    res_aic = append(res_aic , m2$aic)
  }
}


df1 = data.frame("p" = res_p,"q" = res_q , "aic" = res_aic)

View(df1)

df1

################################################






