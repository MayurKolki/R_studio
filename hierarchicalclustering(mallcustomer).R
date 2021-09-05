#hierarchical clustering
#dataser:mallcustomer 

library(cluster) #cluster

path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/mallcustomers.csv"

mall=read.csv(path,header=T) #,stringsAsFactors = T)
#file.choose()
View(mall)
colnames(mall)
#rename the column names 
names(mall)[names(mall)=="Annual.Income..k.."]='income'
names(mall)[names(mall)=="Spending.Score..1.100."]='score'

names(mall)
#build the first clustering on the features 'income'
 #and 'score'
 mall1=mall[,c('income','score')]
 head(mall1)
 
 #plot the dendrogram to determine the optimal clusters
den=hclust(dist(mall1,method='euclidean'),method='ward.D')

 ?hclust
#plot the dendrogram
plot(den,col='blue',main='Dendrogram:Mall Customers',
     xlab='Customers',
     ylab='Distance')
#based on the dendrogram ,take optimal cluster 4 or 5
c=5
#model
model=hclust(dist(mall1,method='euclidean'),
             method='ward.D')
clusters=cutree(model,c)
#visualise the clusters
clusplot(mall1,clusters,lines=0,labels=0,
         span=T,
         shade=T,
         main="Customer Segmentation",
         xlab="Income",
         ylab="Score",
         )

#store the clusters in the data frame
mall$clusters=clusters
head(mall1,50)
mall$cluster=clusters
View(mall)

library(ggplot2)
ggplot(mall1)+geom_point(aes(x=income,y=score,
                             color=as.factor(clusters)),
                         size=1.8)+
        scale_color_brewer(palette="Dark2")

?geom_point

#get the customers data who belon to C1

#1) try with 4 clusters
c1=4
#model
model1=hclust(dist(mall1,method='euclidean'),
             method='ward.D')
clusters1=cutree(model1,c1)
#visualise the clusters
clusplot(mall1,clusters1,lines=0,labels=0,
         span=T,
         shade=T,
         main="Customer Segmentation",
         xlab="Income",
         ylab="Score",
)






#2)build the customer segmentation using Age and income 
colnames(mall)
#build the first clustering on the features 'income'
#and 'score'
mall11=mall[,c('Age','income')]
head(mall11)

#plot the dendrogram to determine the optimal clusters
den11=hclust(dist(mall11,method='euclidean'),method='ward.D')

?hclust
#plot the dendrogram
plot(den11,col='blue',main='Dendrogram:Mall Customers',
     xlab='Customers',
     ylab='Distance')
#based on the dendrogram ,take optimal cluster 4 or 5
c11=6
#model
model11=hclust(dist(mall1,method='euclidean'),
             method='ward.D')
clusters11=cutree(model11,c11)
#visualise the clusters
clusplot(mall1,clusters11,lines=0,labels=0,
         span=T,
         shade=T,
         main="Customer Segmentation",
         xlab="Income",
         ylab="Score",
)



#store the clusters in the data frame
mall$clusters=clusters
head(mall1,50)
mall$cluster=clusters
View(mall)





















