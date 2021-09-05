#################  Random Forest Regression #####################
##############    Dataset:TAXIFARE ##############


library(caret)
library(ggplot2)
library(pROC)
library(ROCR)
library(funModeling)
library(lubridate)
path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/Taxi Fare Prediction/Dataset/TaxiFare.csv"
taxi=read.csv(path,header=T) #,stringsAsFactors = T)
View(taxi)


#Code to split the merged values into different columns
datetime=data.frame(do.call("rbind",strsplit(as.character(taxi$date_time_of_pickup)," ",fixed = TRUE)))
View(datetime)


taxi1$datetime=datetime
View(taxi1)
colnames(taxi)

###############################################################
#spliting  weekdays and weekends

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

##########################################################################

# write the dataframe into a file (.CSV format)
write.csv(taxi1,"Taxifare11.csv",row.names=F)

# file location - in the default working directory
getwd()
path="C:/Users/mayur/Desktop/RSTUDIO 1OCT2020/Taxifare11.csv"
taxi11=read.csv(path,header=T) #,stringsAsFactors = T)
colnames(taxi11)
View(taxi11)



#############################################################

####################  EDA  #######################

###########################################################

p1=(taxi11$longitude_of_pickup)^2
View(p1)

#no. of zeros in longitude_of_pickup
length(which(p1<=0))
z1=which(p1<=0)

#taking mean of the column
m1=mean(p1)

# update the ZERO values with the mean value
p1[z1]=m1
View(p1)
p11=sqrt(p1)
View(p11)
###################################################################

p2=(taxi11$latitude_of_pickup)^2
View(p2)

#no. of zeros in longitude_of_pickup
length(which(p2<=0))
z2=which(p2<=0)

#taking mean of the column
m2=mean(p2)

# update the ZERO values with the mean value
p2[z2]=m2
View(p2)
p22=sqrt(p2)
View(p22)
###############################################################

p3=(taxi11$longitude_of_dropoff)^2
View(p3)

#no. of zeros in longitude_of_pickup
length(which(p3<=0))
z3=which(p3<=0)

#taking mean of the column
m3=mean(p3)

# update the ZERO values with the mean value
p3[z3]=m3
View(p3)
p33=sqrt(p3)
View(p33)
###################################################################

p4=(taxi11$latitude_of_dropoff)^2
View(p4)

#no. of zeros in longitude_of_pickup
length(which(p4<=0))
z4=which(p4<=0)

#taking mean of the column
m4=mean(p4)

# update the ZERO values with the mean value
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

#dealing with no_of_passanger column
taxi11$no_of_passenger=taxi$no_of_passenger
View(taxi11$no_of_passenger)
table(taxi11$no_of_passenger)
summary(taxi11$no_of_passenger)
# identify the rows that have zero values for passenger
nullrow22= as.integer(rownames(taxi11[(taxi11$no_of_passenger)==0,]))
print(nullrow22)
# update the zero values with the mode value
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




#######################################################################


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



#store predictions in dataframe for comparison
res=data.frame('actual'=testy,'rf'=round(p_rf1,2))
print(res)


#RMSE
RMSE(testy,p_rf1)










########################################





