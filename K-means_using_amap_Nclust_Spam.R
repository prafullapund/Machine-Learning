a=read.csv("spam.csv")
b=a[-58]
str(b)
summary(b)

normalize=function(x)
{
  (x-min(x))/(max(x)-min(x))
}
apply(b[,c(-1:-55)],2,normalize)
wtss=c()
btss=c()
for(i in 2:20)
{
  spamcluster1=kmeans(b[,1:57],i,nstart=20)
  wtss=c(wtss,spamcluster1$tot.withinss)
  btss=c(btss,spamcluster1$betweenss)
}

plot(2:20,wtss)
plot(2:20,btss)

#K-means using Amap

library(amap)

wtss1=c()
btss=c()
for(i in 2:20)
{
  km=Kmeans(b,i,iter.max=50,nstart=20,method="euclidean")
  wtss1=c(wtss1,km$withinss)
}

plot(2:20,wtss1)

# k-mean using Nbclust

library(NbClust)


res=NbClust(b,diss = NULL,distance = "euclidean",min.nc = 6,method="kmeans",index="all")
res$All.index
res$Best.nc
res$Best.partition
ndist=dist(b,method="euclidean")

