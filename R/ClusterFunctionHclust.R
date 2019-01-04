# Funktion - Implementierung eines Hierachisches Clusterverfahren
# Eingabe: 
# - data: Eine data.frame mit den Performanzwerten und zugehoerigen
#         Experimenten ID's
# - distMethod: Welche distanzmethode wird die Abstaende der Daten in R genutzt
# - clusterMethod: Welche hierarchische Clustermethode soll genutzt werden
# Rueckgabe: 
# Eine Liste der Laenge der optimalen Clustergroesse in der in jedem Element 
# alle dieser Clustergruppe zugehoerigen Elemente (in Form ihrer Indizes) stehen.

clusterFunctionHclust = function(clust.data, 
                                  distMethod = "euclidean", 
                                  clusterMethod = "ward.D",
                                  idx = "all",
                                  max.nc = 5){
  
  # hierachisches Clustern der Performancewerte.
  h.cluster = hclust(dist(clust.data, method = distMethod), method = clusterMethod)
 
  nb = NbClust::NbClust(as.matrix(clust.data), method = clusterMethod, 
    index = idx, alphaBeale = 0.1, max.nc = max.nc)
  opt.Clusteranzahl = nb$Best.nc[1]
  
  # Erstelle data.frame mit Spalten .expID und .clustID 
  clusters = cutree(h.cluster, opt.Clusteranzahl)
  clusters = data.frame(.expID = names(clusters), .clustID = clusters, row.names = NULL)
  
  return(clusters)
}

