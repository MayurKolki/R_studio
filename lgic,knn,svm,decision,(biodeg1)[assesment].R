#######Mayur kolki->PGA14
##LOGISTIC,SVM,DECISION,KNN[assesment] 
###########Dataset:biodeg1
set.seed(123)
path="C:/Users/mayur/Desktop/datascience DELL/data analyst vedios/r language/machine learning/biodeg1.csv"
biodeg = read.csv(path , header = T)
str(biodeg)
dim(biodeg)

#Drop the feature which have singularity more than 85%

biodeg$B04.C.Br. = NULL
biodeg$B01.C.Br. = NULL
biodeg$B03.C.CI. = NULL
biodeg$nArCOOR = NULL
biodeg$nN.N = NULL
biodeg$nArNO2 = NULL
biodeg$nCRX3 = NULL
biodeg$F01.N.N. = NULL
biodeg$NssssC = NULL
biodeg$N.073 = NULL
biodeg$nCrt = NULL
str(biodeg)
shuffle_index = sample(1:nrow(biodeg))
biodeg= biodeg[shuffle_index,]
head(biodeg)
tail(biodeg)

#Split the data into train and test.
biodeg$Exp.Class = as.factor(biodeg$Exp.Class)
total = nrow(biodeg)
ss = sample(seq(1,total),floor(0.7*total))
train = biodeg[ss,]
test = biodeg[-ss,]

# Build the Model with decision tree.
library(party)
library(caret)
library(partykit)




################# DECISION TREE

m1 = ctree(Exp.Class~., data = train)
plot(m1)
m1

p1 = predict(m1 , test)

cm = confusionMatrix(test$Exp.Class , as.factor(p1) , positive = 'RB')
cm
# Accurecy observed upto 80.76%

#####
library(rpart)
library(rpart.plot)
m2 = rpart(Exp.Class~., data = train , method = 'class' , control = rpart.control(cp=0.005 , xval = 10))

rpart.plot(m2 , extra = "auto", type =2)

mod = as.party(m2)
plot(mod)
mod
important_variable = as.data.frame(m2$variable.importance)
View(important_variable)
write.csv(important_variable , "Impotance Variable by Decision Tree.csv")

m2$cptable
plotcp(m2)

# Here we have to prune the tree.

p2 = predict(m2 , test , type = 'class')

cm1 = confusionMatrix(test$Exp.Class , as.factor(p2) , positive = 'RB')
cm1 

### Start the prune of tree.

mincp = m2$cptable[which.min(m2$cptable[,'xerror']),'CP']
print(mincp)

# now build the tree with mincp

m2_pr = prune(m2,mincp)
m2_pr$variable.importance
m2_pr$cptable
plotcp(m2_pr)
mod_pr = as.party(m2_pr)
mod_pr
plot(mod_pr)

# now start the prediction

p_pr = predict(m2_pr , test , type = 'class')

# Confusion matrix
cm_pr = confusionMatrix(test$Exp.Class , as.factor(p_pr),positive = "RB")
cm_pr


# but after pruning we found accuracy 83.6 and without pruning we found 83.9%.

# so, try using the Important variable.

m3 = rpart(Exp.Class~SpMax_B.m.+SpMax_L+SpMax_A+Mi+SpPosA_B.p.+nO+nN , data = train , method = 'class')
rpart.plot(m3 , extra = 101 , type = 4 , tweak = 1.5)

m3party = as.party(m3)
m3party

plot(m3party)

p3 = predict(m3 , test , type = 'class')

cm3 = confusionMatrix(test$Exp.Class , as.factor(p3) , positive = "RB")
cm3

#### Lets try again hyper tunning and take cp = 0.01

m4 = rpart(Exp.Class~., data = train , control = rpart.control(cp=0.0005 , xval = 10 , minsplit = 10 ,maxdepth = 20) , method = 'class')

m4_party = as.party(m4)
m4_party
plot(m4_party)

p4 = predict(m4 , test , type = 'class')

cm4 = confusionMatrix(test$Exp.Class , as.factor(p4) , positive = 'RB')
cm4

# After doing all type of hyper parameter tunning we found that the accurecy is not increase by 83.

#now we try to do panelty matrix for this.

panelty_matrix = matrix(c(0,20,32,0),byrow = F,nrow = 2)
panelty_matrix

m5 = rpart(Exp.Class~., data = train , method = "class",parms = list(loss = panelty_matrix) , control = rpart.control(cp=0.01 , xval = 10))

rpart.plot(m5,type = 4 , extra = 101 , tweak = 3)

m5_party = as.party(m5)
m5_party

p5 = predict(m5 , test , type = 'class')

cm5 = confusionMatrix(test$Exp.Class , as.factor(p5) , positive = "RB")
cm5

# We tried all methods now copare all model accurecy.


df1 = as.data.frame(cm1$byClass)
df1$acc = cm1$overall[1]
df1$model = "M1-DEcision tree "

df2 = as.data.frame(cm_pr$byClass)
df2
df2$acc = cm_pr$overall[1]
df2$model = "M2-proune"

df3 = as.data.frame(cm3$byClass)
df3
df3$acc = cm3$overall[1]
df3$model = "Hyper tunning"

df4 = as.data.frame(cm4$byClass)
df4$acc = cm4$overall[1]
df4$model = "Hyper tunning reducing cp value"

df5 = as.data.frame(cm5$byClass)
df5$acc = cm5$overall[1]
df5$model = "Hyper tunning applying panelty matrix"


df_dt = cbind(df1 , df2 , df3 , df4 , df5)

write.csv(df_dt , "Result by DT.csv")


################################################################################################################################################################################################

# Working on LOgistic Regression

blr = biodeg
str(blr)
num = names(Filter(is.numeric , blr))
num

# Checking for corelation
library(corrplot)
corr = cor(blr[num])
corrplot(corr , method = 'number' , type = 'lower',number.cex = 1 , number.font = 2 , )
View(corr)
# there are lots of feature where corelation are high.

#############################################
# new method to find out highly corelated feature and null it;

highly_co = findCorrelation(corr , cutoff = 0.7)
print(highly_co)
names(blr[highly_co])
#names(blr[highly_co]) = NULL # not working
str(blr)

blr[highly_co] = NULL # to drop all features which have high colinearity.

#
str(blr)

num = names(Filter(is.numeric , blr))
num

corr = cor(blr[num])
corrplot(corr, method = 'number' , type = 'lower')

########
# now start the data split procedure

blr$Exp.Class = factor(blr$Exp.Class , levels = c("RB","NRB") , labels = c(0,1))
str(blr)
totallr = nrow(blr)
sslr = sample(seq(1,totallr),floor(0.7*totallr))
trainlr = blr[sslr,]
testlr = blr[-sslr,]
dim(trainlr)
dim(testlr)

# Now build the model

m1lr = glm(Exp.Class~., data = trainlr , family = binomial(link = 'logit'))

summary(m1lr)

p1lr = predict(m1lr , testlr , type = 'response')
head(p1lr)

cutoff = 0.5

p1lrc = ifelse(p1lr>0.5 , 1 ,0)
head(p1lrc)

cm1lr = confusionMatrix(testlr$Exp.Class , as.factor(p1lrc) , positive = '0')
cm1lr

# try to decrease the cutoff 

cutoff = 0.4
p2lrcd = ifelse(p1lr>0.4 , 1,0)
cm2lrd = confusionMatrix(testlr$Exp.Class , as.factor(p2lrcd) , positive = '0')

cm2lrd


## try to decrease the cutoff 

cutoff = 0.35
p3lrcd = ifelse(p1lr>0.35 , 1,0)
cm3lrd = confusionMatrix(testlr$Exp.Class , as.factor(p3lrcd) , positive = '0')

cm3lrd


# now draw the AUC and ROC
library(pROC)
library(ROCR)

p1roc = prediction(p1lr , testlr$Exp.Class)

perflr = performance(p1roc , 'tpr' , 'fpr')


#AUC

auc = performance(p1roc , "auc")
round(unlist(slot(auc,'y.values')),3)

plot(perflr , colorize =T , main = "ROC CURVE" , ylab = 'Sensitivity' , xlab = '1-Specificity' , round(unlist(slot(auc , "y.values")),3))
abline(a=0 , b=1)

#AUC

auc = performance(p1roc , "auc")
round(unlist(slot(auc,'y.values')),3)

#The AUC on the test set indicates that the predictive ability of the model is good..

##############################################

## Now do hyper parameter tunning , use only significant feature.
summary(m1lr)

m2lr = glm(Exp.Class~SpMax_L+nHM+nCp+Mi+nCIR+TI2_L+C.026+F02.C.N.+Psi_i_A , data = trainlr , family = binomial(link = 'logit'))

summary(m2lr)

p4lr = predict(m2lr , test , type = 'response')

cutoff = 0.5

p4lrc = ifelse(p4lr>0.5 , 1, 0)

cm4lr = confusionMatrix(testlr$Exp.Class , as.factor(p4lrc) , positive = '0')

cm4lr

#so , here we found that only cm1lr gives us good accurecy and after hyper parameter tunning we found less accurecy.

###########################################
#store the result

##FOR Logistic
df1lr = as.data.frame(cm1lr$byClass)
View(df1lr)
df1lr$model = "Cutoff = 0.5"
df1lr$accuracy = cm1lr$overall[1]
View(df1lr)


##FOR Logistic2
df2lr = as.data.frame(cm2lrd$byClass)
View(df2lr)
df2lr$model = "Cutoff = 0.4"
df2lr$accuracy = cm2lrd$overall[1]
View(df2lr)


##FOR Logistic3
df3lr = as.data.frame(cm3lrd$byClass)
View(df3lr)
df3lr$model = "Cutoff = 0.35"
df3lr$accuracy = cm3lrd$overall[1]
View(df3lr)

cm4lr
##FOR Logistic2
df4lr = as.data.frame(cm4lr$byClass)
View(df4lr)
df4lr$model = "Cutoff = 0.5 with only significance variables"
df4lr$accuracy = cm4lr$overall[1]
View(df4lr)


df_lr = cbind(df1lr , df2lr , df3lr , df4lr)

write.csv(df_lr , "Result by Logistic Reg.csv")


################################################################################################################################################################################################


# Know work on KNN 

str(blr)

bknn = blr

head(bknn)
tail(bknn)
library(funModeling)
freq(bknn$F02.C.N.)

totalknn = nrow(bknn)
ssknn = sample(seq(1,totalknn),floor(0.7*totalknn))
trainknn = bknn[ssknn,]
testknn = bknn[-ssknn,]
dim(trainknn)

# now split the data into trainx and train y and same in testx/y.

dep_var = grep("Exp.Class" , names(trainknn))
dep_var

trainknn_x = trainknn[,-dep_var]
trainknn_y = trainknn[,dep_var]
View(trainknn_y )
dim(trainknn_x)

length(trainknn_y)

#
testknn_x = testknn[,-dep_var]
testknn_y = testknn[,dep_var]

#so in all upper steps we split y varibale again trainx train y , testx testy because its an parameter in model building 

# to get teh optimal value of k by cross validation
library(class)
k=seq(3,11,2)
k
cv_acc=c()
for (i in k)
{
  model=knn.cv(trainknn_x,trainknn_y,k=i)
  #predication accuracy
  acc=length(which(model==trainknn_y))/length(trainknn_y)
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
m1knn=knn(trainknn_x,testknn_x,trainknn_y,k=opt_k)
?knn
#prediction is 
#p1=m1 beacues its a 
#simaultaneous process in it

#confusion matrix
cm1knn=confusionMatrix(as.factor(testknn_y),as.factor(m1knn),positive = "0")
cm1knn

# Store the result
##For KNN
df1knn = as.data.frame(cm1knn$byClass)
View(df1knn)
df1knn$model = "KNN model with op_k = 9"
df1knn$accuracy = cm1knn$overall[1]
View(df1knn)

write.csv(df1knn , 'K-NN Result.csv')

################################################################################################################################################################################################

# SVM Algo...........

bsvm = blr
str(bsvm)

#  here we have to scale the data then use for svm.

minmax = function(x)return((x-min(x))/ (max(x)-min(x)))

# now extract all numeric data from dataset.
num = names(Filter(is.numeric , bsvm))
num

scale_svm = as.data.frame(lapply(bsvm[num], minmax))
head(scale_svm)

# now replace the target variable

scale_svm$Exp.Class = bsvm$Exp.Class
freq(scale_svm$Exp.Class)
str(scale_svm)

# now split the data into train and test
totalsvm = nrow(scale_svm)
sssvm = sample(seq(1,totalsvm) , floor(0.7*totalsvm))
trainsvm = scale_svm[sssvm ,]
testsvm = scale_svm[-sssvm ,]
dim(trainsvm)
dim(testsvm)

library(e1071)

#Build the svm models using the different kernels.
#1) linear
#2) Radial
#3) sigmoid
#4) polynomial.

ker = 'linear'
c_list = c(0.001,0.01,0.1,1,10,50,100)


# do  the CV to get the best C parameter

cv1 = tune(svm,Exp.Class~.,data = trainsvm,kernel = ker,ranges = list(cos=c_list))
cv1

optc = unlist(cv1$best.parameters)
optc


# Build the model
m1svm = svm(Exp.Class~.,data = trainsvm, kernel=ker,cost=optc)
summary(m1)

# Predict 
str(trainsvm)
str(testsvm)
p1svm = predict(m1svm,testsvm)
head(p1svm)

# Confusion Matrix and classification  report.

cm1svm = confusionMatrix(testsvm$Exp.Class,p1svm , positive = '0')
cm1svm


## Visualize
names(bsvm)
plot(m1svm,testsvm,SpMax_L~nHM)



# MODEL  2 ;, kernel = radial
ker1 = "radial"
c_list = c(0.001,0.01,0.1,1,10,50,100)
g_list = c(0.005,0.05,0.02,0.01,0.1,1)

# CV to get the optimal C and Gamma combination

cv2 = tune(svm,Exp.Class~.,data = trainsvm,kernel = ker1,ranges = list(cos=c_list,gamma=g_list))
print(cv2$best.parameters)




otpc1 = unlist(cv2$best.parameters[1])
otpg1 = unlist(cv2$best.parameters[2])

# RBF model

m2svm = svm(Exp.Class~.,data = trainsvm,kernel = ker1,cost = otpc1,gamma = otpg1)

#3 Predict
p2svm = predict(m2,testsvm)

# Confusion MAtrix

cm2svm = confusionMatrix(testsvm$Exp.Class,p2svm , positive = '0')
cm2svm



# MODEL  3 ;, kernel = Sigmoid

ker2 = "sigmoid"
c_list = c(0.001,0.01,0.1,1,10,50,100)
g_list = c(0.005,0.05,0.02,0.01,0.1,1)

# cross validation to get the optimal C and gamma combination.

cv3svm = tune(svm,Exp.Class~.,data = trainsvm,kernel = ker2,ranges = list(cos = c_list,gamma = g_list))

print(cv3svm$best.parameters)

otpc2 = unlist(cv3svm$best.parameters[1])
otpg2 = unlist(cv3svm$best.parameters[2])

# Sigmoid model

m3svm = svm(Exp.Class~.,data = trainsvm,kernel = ker2,cost = otpc2,gamma = otpg2)

#3 Predict
p3svm = predict(m3svm,testsvm)

# Confusion MAtrix

cm3svm = confusionMatrix(testsvm$Exp.Class,p3svm)
cm3svm


### MODEL 4
# MODEL  4 ;, kernel = Polynomial
ker3 = "polynomial"
c_list = c(0.001,0.01,0.1,1,10,50,100)
g_list = c(0.005,0.05,0.02,0.01,0.1,1)

# cross validation to get the optimal C and gamma combination.

cv4svm = tune(svm,Exp.Class~.,data = trainsvm,kernel = ker3,ranges = list(cos = c_list,gamma = g_list))

print(cv4svm$best.parameters)

otpc3 = unlist(cv4svm$best.parameters[1])
otpg3 = unlist(cv4svm$best.parameters[2])

# Polynomial model

m4svm = svm(Exp.Class~.,data = trainsvm,kernel = ker3,cost = otpc3,gamma = otpg3)

# Predict
p4svm = predict(m4svm,testsvm)

# Confusion MAtrix

cm4svm = confusionMatrix(testsvm$Exp.Class,p4svm)
cm4svm

################
###############################################
# Compare the model and store it in one data frame
# Analys the DF to pick the best performing moodel.

##FOR M1
df1 = as.data.frame(cm1svm$byClass)
View(df1)
df1$model = "Linear"
df1$accuracy = cm1svm$overall[1]
View(df1)

##FOR M2
df2 = as.data.frame(cm2svm$byClass)
View(df2)
df2$model = "Radial"
df2$accuracy = cm2svm$overall[1]
View(df2)

##FOR M3
df3 = as.data.frame(cm3svm$byClass)
View(df3)
df3$model = "Sigmoid"
df3$accuracy = cm3svm$overall[1]
View(df3)

##FOR M4
df4 = as.data.frame(cm4svm$byClass)
View(df4)
df4$model = "Polynomial"
df4$accuracy = cm4svm$overall[1]
View(df4)


### now combine all the DF to check.

svmdf = cbind(df1,df2,df3,df4)
View(svmdf)
## Remove unwanted coloumn
svmdf$Prevalence = NULL
svmdf$`Detection Prevalence`=NULL
svmdf$`Detection Rate` = NULL

write.csv(svmdf , "SVM Result.csv")

###############################################################################################################################################################################################

# now work on Random Forest.

brf = b
str(brf)

#Drop the feature which have singularity more than 85%

brf$B04.C.Br. = NULL
brf$B01.C.Br. = NULL
brf$B03.C.CI. = NULL
brf$nArCOOR = NULL
brf$nN.N = NULL
brf$nArNO2 = NULL
brf$nCRX3 = NULL
brf$F01.N.N. = NULL
brf$NssssC = NULL
brf$N.073 = NULL
brf$nCrt = NULL

shuffle_index = sample(1:nrow(brf))
brf = brf[shuffle_index,]
head(brf)
tail(brf)

summary(brf)


# Now split the data into train and test..

totalrf = nrow(brf)
ssrf = sample(seq(1,totalrf) , floor(0.7*totalrf))

trainrf = brf[ssrf,]
testrf = brf[-ssrf,]

paste('trainrf=',dim(trainrf) , 'testrf=' , dim(testrf))

rfd = which(names(brf)=="Exp.Class")
print(rfd)
trainrfx=trainrf[,-rfd]
trainrfy=trainrf[,rfd]

testrfx=testrf[,-rfd]
testrfy=testrf[,rfd]

dim(trainrfx)
dim(testrfx)

length(trainrfx)
length(testrfy)

trainrfy = as.factor(trainrfy)
library(randomForest)
m1rf=randomForest(trainrfx,trainrfy) #,method ='class')
plot(m1rf)
summary(m1rf)

p1rf = predict(m1rf,testrfx , type = 'class')
library(caret)
cm1rf = confusionMatrix(as.factor(testrf$Exp.Class) , as.factor(p1rf) , positive = 'RB')
cm1rf

m1rf$importance
m1rf$importanceSD
m1rf$localImportance
m1rf$localImportance
m1rf$proximity
m1rf$ntree
m1rf$mtry

# here we found that the number of tree are 500 , and  mtry = 5 we got the accuracy of 84% .

# so we try to increase the the number of tree a nd mtry.

m2rf=randomForest(trainrfx,trainrfy,ntree = 600 , mtry = 8) #,method ='class')
plot(m2rf)
summary(m2rf)

varImpPlot(m2rf) # CHecking GINImean

p2rf = predict(m2rf,testrfx , type = 'class')
cm2rf = confusionMatrix(as.factor(testrf$Exp.Class) , as.factor(p2rf) , positive = 'RB')
cm2rf

#Here we found that if we increase the number of tree and mtry then we got a good accurecy. 
# we can stop here and final result.

# now store the data in format.

##FOR M1
df1 = as.data.frame(cm1rf$byClass)
View(df1)
df1$model = "ntree = 500"
df1$accuracy = cm1rf$overall[1]
View(df1)

##FOR M2
df2 = as.data.frame(cm2rf$byClass)
View(df2)
df2$model = "ntree = 600"
df2$accuracy = cm2rf$overall[1]
View(df2)

rfdf = cbind(df1,df2)
View(rfdf)
## Remove unwanted coloumn
rfdf$Prevalence = NULL
rfdf$`Detection Prevalence`=NULL
rfdf$`Detection Rate` = NULL

write.csv(rfdf , "Random Forest Result.csv")

#################################################################################################################################################################################################