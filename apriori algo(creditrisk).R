#apriori  aglo
#dataser:credit risk
#libraries for Market Basket Analysis(apriori)
library(arules)
library(arulesViz)
library(cluster) #cluster

path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/creditrisk.csv"

crisk=read.csv(path,header=T,colClasses = "factor") #,stringsAsFactors = T)
str(crisk)

#form the first set of rules
rules1=apriori(crisk)

#view the rules
inspect(rules1)

#summary the rules
summary(rules1)
#convert  the rules into dataframe
r1=as(rules1,"data.frame")
View(r1)

#customize the rules 
rules2=apriori(
        crisk,parameter=list(maxlen=3,supp=0.4,conf=0.7),
               appearance=list(rhs=c("CreditScore=Bad"),default="lhs")
                )

r2=as(rules2,"data.frame")
View(r2)

#coverage?

rule3=apriori(crisk,
              parameter = list(minlen=2,maxlen=4,supp=0.3,conf=0.6),
              appearance = list(rhs=c('CreditScore=Bad'),
                                lhs=c('Saving.accounts=little'),
                                default="none")
             )

r3=as(rule3,"data.frame")
View(r3)

#project:Grocery dataset
#inbuilt: Groceries

