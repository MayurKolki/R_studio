#Time series forecast using ARIMA
#dataset:MRF stock forecast 
library(tseries)
library(forecast)

mrf=read.csv(file.choose(), header = T)
dim(mrf)
View(mrf)

head(mrf)
anyNA(mrf)


path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/mrf.csv"
mrf=read.csv(path,header=T) #,stringsAsFactors = T)

View(mrf)
str(mrf)
#lets predict 'close  price'
View(mrf$Close.Price)
closeprice= mrf$Close.Price
View(closeprice)
plot(closeprice,type='l',col='red')

#convert the 'close price' into a TS format
tsCP=ts(mrf$Close.Price,start=c(2006,1),freq =12 )
print(tsCP)


#get the individual components 
plot(decompose(tsCP))

#Augmented Dicky-Fuller test to determine the stationarity of the  CP
cp=mrf$Close.Price
print(cp)
adftest=adf.test(cp)
pvalue=adftest$p.value
print(pvalue)
result=ifelse(pvalue<0.05,"stationary","not stationary")
print(result)
cp[1:20]

#de-trend the cp
cp=diff(cp)
cp[1:20]
adftest=adf.test(cp)
pvalue=adftest$p.value
print(pvalue)
result=ifelse(pvalue<0.05,"stationary","not stationary")
print(result)


plot(cp,type='l')

#using the ACF and PACF graphs, determine the p,q values
pacf(cp)
acf(cp)

pacf(closeprice)
acf(closeprice)
#based on the correlogram p and q=0


#run the auto arima determine the best p,q,d
auto.arima(cp)
#(p,d,q) --> 0,0,0

#best model  is determined by the least value of AIC(Akike Information Criteria)

#build the model 1
p=0;d=0;q=0

m1=arima(cp,order=c(p,d,q))
summary(m1)
#to get the AIC of the model
aic1=m1$aic
print(paste('AIC of model1=',aic1))

#the best model is the one that has the least AIC score for the given combination of p,d,q

#determine the errors for IID
m1_err=m1$residuals
hist(m1_err,col='yellow',breaks=10)

#LJung-box test on 
res.test=Box.test(m1_err,type="Ljung-Box")
res=ifelse(res.test$p.value<0.05,"Bad Model","Good Model")
print(res)


#forecast for the next 6 months
p1=forecast(m1,h=6)
plot(p1)

print(p1)

length(cp)

cp[124]
closeprice[124:125]

#############################################
###check for best combination p,q,d.
P=seq(0,3)
q=seq(0,3)
D=0
for(every P)
{
  m1=arima(cp,order=c(p,d,q))
  summary(m1)
  #to get the AIC of the model
  aic1=m1$aic
  print(paste('AIC of model1=',aic1))
  
}

###############################################333
  res_p=c()
  res_q=c()
  res_aic=c();d=0

  for(p in seq(0,1))
  {
    for(q in seq(0,3))
    {
      m1=arima(cp,order=c(p,d,q))
      res_p=append(res_p,p)
      res_q=append(res_q,q)
      res_aic=append(res_aic,m1$aic)
    }
      }
  
  df1=data.frame("p"=res_p,"q"=res_q,"aic"=res_aic)
  View(df1)
  
  
  
  
      
    
  

















