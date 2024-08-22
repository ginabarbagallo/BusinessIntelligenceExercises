install.packages("cluster")
library(cluster)
install.packages("factoextra")
library(factoextra)
data = USArrests
summary(data)
data_scale=scale(data)
summary(data_scale)


fviz_nbclust(data_scale,kmeans,method="wss")
fviz_nbclust(data_scale,kmeans,method="silhouette")
#the best number of clusters with the elbow method is 1
#the best number of clusters with the silhouette method is 2


km = kmeans(data_scale,centers=2,nstart=25)
km
#The centroids of cluster 1 is -0.669956 for murder, -0,6758849 for assualt,
#-0.1317235 for urbanpop, -0,5646433 for rape.
#The centroids of cluster 2 is 1.004934 for murder, 1.0138274for assualt,
#0.1975853for urbanpop, 0.8469650 for rape.
kme = pam(data_scale,k=2,nstart=25)
kme
#plot
fviz_cluster(km,data=data_scale,choose.vars=c("Murder","UrbanPop"))
fviz_cluster(kme,data=data_scale,choose.vars=c("Murder","UrbanPop" ))

km=kmeans(data_scale,centers=4,nstart=25)
km
fviz_cluster(km,data=data_scale,choose.vars=c("Murder","UrbanPop"))


d = dist(data_scale, method="euclidean")
hc1 =hclust(d,method="complete")
plot(hc1)
sub_group=cutree(hc1,k=4)
fviz_cluster(list(data=data_scale,cluster=sub_group),choose.vars=c("Murder","UrbanPop"))
fviz_nbclust(data_scale,hcut,method="wss")
#The optimal number of clusters using hierarchical clustering elbow method is 4
fviz_nbclust(data_scale,hcut,method="silhouette")
#The optimal number of clusters using hierarchical clustering silhouette method is 2
