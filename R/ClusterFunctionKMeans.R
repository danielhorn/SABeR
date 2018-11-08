clusterFunctionKMeans <- function(clust.data, 
                                  distMethod = "euclidean", 
                                  clusterMethod = "ward.D",
                                  idx = "dunn"){
  
  # Bestimmt oprimale Clustergroesse durch gapStatistik
  nb <- NbClust::NbClust(as.matrix(clust.data), method = "kmeans", 
                index = idx, alphaBeale = 0.1)
  opt.Clusteranzahl <- nb$Best.nc[1]
  
  kmeans.cluster <- kmeans(clust.data,opt.Clusteranzahl,nstart = 20)
  
  clusters <- data.frame(.expID = names(kmeans.cluster$cluster), 
                         .clustID = kmeans.cluster$cluster, row.names = NULL)
  
  return(clusters)
}

