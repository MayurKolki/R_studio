#k means clustering
#dataser:wholesale 

library(cluster) #cluster

path="C:/Users/mayur/Desktop/data analyst vedios/r language/machine learning/wholesale.csv"

wholesale=read.csv(path,header=T) #,stringsAsFactors = T)
#file.choose()
View(wholesale)
head(wholesale)
tail(wholesale)
sale1=wholesale[,c('Fresh','Milk')]
#minmax   optional step
minmax=function(x) return((x-min(x))/(max(x)-min(x)))
sale1=as.data.frame(lapply(sale1,minmax))
View(sale1)

#find the optimal clusters
#finding the least within cluster square errors from each cluster

nrow(sale1)
wcss=c()
#try with 10 clustersd
for(k in seq(1,10))
{
  
 ss= sum(kmeans(sale1,k)$withinss)
  wcss=append(wcss,ss)
  
}
print(wcss)

#plot the WCSS against the number of clusters
plot(1:10,wcss,main="WCSS vs Clusters",
     xlab="Clusters",ylab="WCSS",
     type="b",col='blue') # "b"=>small circles
#based on the graph , the best cluster=4
optk=5
model=kmeans(sale1,optk,
             iter.max=300,#how many centriods will be created
             nstart = 10) #nstart to the best cluster ,which are  well sperate good  clusters,best centriods ,best speration of clusters
#it selects the best centriod based on least errors

#associate the data with the clusters
model$cluster

View(sale1)
#visualise the clusters
clusplot(sale1,model$cluster,lines=0,color=T,span=F,main="Clusters",
         labels=0,xlab="Fresh",ylab = "Milk")


#add the cluster to the data set
sale1$cluster=model$cluster
sale1$cluster=NULL
