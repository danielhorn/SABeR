clusterFunctionKMeans <- function(clust.data, 
                                  distMethod = "euclidean", 
                                  clusterMethod = "ward.D"){
  
  # Bestimmt oprimale Clustergroesse durch gapStatistik
  nb <- NbClust::NbClust(as.matrix(clust.data), method = "kmeans", 
                index = "gap", alphaBeale = 0.1)
  opt.Clusteranzahl <- nb$Best.nc[1]
  
  kmeans.cluster <- kmeans(clust.data,opt.Clusteranzahl,nstart = 20)
  
  clusters <- data.frame(.expID = names(kmeans.cluster$cluster), 
                         .clustID = kmeans.cluster$cluster, row.names = NULL)
  
  return(clusters)
}


####
# Was spuckt alles 2 aus: gap, ch, frey, mcclain, dunn
# Was spuckt 3 zurueck: duda, pseudot2, ball
# Was spuckt 4 zurueck: tau
# Was spuckt 5 zurueck: ratkowskiy
library(NbClust)
nb <- NbClust::NbClust(as.matrix(clust.data), method = "kmeans", 
                       index = "dunn", alphaBeale = 0.1)
(opt.Clusteranzahl <- nb$Best.nc[1])

####
library(factoextra)
fviz_nbclust(clust.data, kmeans, method = "wss")
###

library(cluster)
k.max <- 10
data <- clust.data
sil <- numeric(k.max)
for(i in 2:k.max){
  km.res <- kmeans(data, centers = i, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(data))
  sil[i] <- mean(ss[,3])
}


plot(1:k.max, sil, type = "b", pch = 19,frame = FALSE, 
     xlab = "Number of clusters k")
abline(v = which.max(sil), lty = 2)



set.seed(123)
km.res <- kmeans(data, 2, nstart = 25)

# Visualize k-means clusters
fviz_cluster(km.res, data = data, geom = "point",
             stand = FALSE, frame.type = "norm")
