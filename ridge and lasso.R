##lasso rigdge linear regression



library(caret)
library(MASS)
library(corrplot)
path="C:/Users/mayur/Desktop/datascience DELL/data analyst vedios/r language/machine learning/concrete.csv"

conc=read.csv(path,header=T)

View(conc)
head(conc)
dim(conc)

# check if data is all numeric
str(conc)

#EDA

#split the data into train/ test
total =nrow(conc)
ss=sample(seq(1,total),0.7*total)
train=conc[ss,]
test=conc[-ss,]

dim(train)
dim(test)

#to oredict ccss(concrete compressive strength)

#build M1 and predict
m1=lm(CCS~.,data=train)
m1$coefficients

library(glmnet)
#split the train into trainx/trainy and convert trainx into a matrix

ndx=grep("CCS",colnames(train))
trainx=as.matrix(train[,-ndx])
trainy=train[,ndx]

#to run the lasso/ridge regularisation function, 'alpha' parameter determines lasso/ridge 
#alpha=0 ->Ridge
#alpha=1 ->Lasso

#Ridge
#Cross-validation to get the best lambda
ridge_cv=cv.glmnet(trainx,trainy,alpha=0)
plot(ridge_cv)

#to find the optimal lambda
ridge_lambda =ridge_cv$lambda.min

#get the coefficient of the linear model for the Ridge model
m_ridge=glmnet(trainx,trainy,alpha=0)
m_ridge_coef=predict(m_ridge,type='coefficients',s=ridge_lambda)

coef1 = matrix(m_ridge_coef,byrow = T)


#lasso
#cross-validation to get the best Lasso
lasso_cv=cv.glmnet(trainx,trainy,alpha=1)
plot(lasso_cv)

#get the coefficients of the lasso model
lasso_lambda=lasso_cv$lambda.min

#get the optimal lambda
m_lasso = glmnet(trainx,trainy,alpha=1)
m_lasso_coef=predict(m_lasso,type ='coefficients', s=lasso_lambda)

m_lasso_coef

coef2=matrix(m_lasso_coef,byrow = T)

#dataframe to store the coefficients of ridge /Lasso
cols=colnames(trainx)
cols=append(cols,'intercept',after=0)
cols

df1=data.frame("X"= cols,
                 'actual'=m1$coefficients,
                'ridge'= coef1,
                'lasso'=coef2)
df1

df1$rownames=NULL
df1

#predictions of all models
p1=predict(m1,test)

#for lasso and ridge predictions, use the formula
#Y=a+bx
p2=round(coef1[1]+as.matrix(test[-ndx])%*%coef1[2:9],2)
p2

p3=round(coef2[1] + as.matrix(test[-ndx])%*%coef2[2:9],2)
p3

#store prediction in dataframe 
results=data.frame('actual'=test$CCS,
                   'model1'=p1,
                   'ridge'=p2,
                   'lasso'=p3)
head(results,20)

#RMSE
e1=RMSE(results$actual,results$model1)
e2=RMSE(results$actual,results$ridge)
e3=RMSE(results$actual,results$lasso)

cat('RMSE\nMOdel1=',e1,"\nRidge =",e2,
    "\nLasso =",e3)





