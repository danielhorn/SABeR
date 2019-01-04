clusterFunctionKMeans = function(clust.data, 
                                  distMethod = "euclidean", 
                                  idx = "dunn",
                                  max.nc = 5){
  
  # Bestimmt oprimale Clustergroesse durch gapStatistik
  nb = NbClust::NbClust(as.matrix(clust.data), method = "kmeans", 
                index = idx, alphaBeale = 0.1, max.nc = max.nc)
  opt.Clusteranzahl = nb$Best.nc[1]
  
  kmeans.cluster = kmeans(clust.data,opt.Clusteranzahl,nstart = 20)
  
  clusters = data.frame(.expID = names(kmeans.cluster$cluster), 
                         .clustID = kmeans.cluster$cluster, row.names = NULL)
  
  return(clusters)
}

