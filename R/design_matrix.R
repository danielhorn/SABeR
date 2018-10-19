# Funktion - 
# Eingabe:
# - pValues: Die vorliegenden p-Wert-Matrix zur vorliegenden Clustergruppe
# - connectMatrix: Die Basis Connection Matrix (dim = count_algos^2)
# - order: Rangordnung der Performance zu den gegebenen Algorithmen in Form
#          einer Indizierug auf algo.name (Namen aller Algorithmen) 
# - testNiveau: zu welchem Niveau sollen edges hergestellt werden
# Rueckgabe:
# connectionMatrix: wenn an Stelle [i,j] eine 1 steht dann wird der Algorithmus
#                   in der i-ten Zeile in Richung des Algorithmusin der j-ten 
#                   Spalte verbunden
design_matrix <- function(pValues, 
                          connectMatrix,
                          order,
                          testNiveau){
# Ordne die Zeilen der Rangordnung der Performance nach
  connectMatrix <- connectMatrix[order,]
  rNames <- rownames(pValues)
  cNames <- colnames(pValues)
  # Weise jeder Verbindung von Algorithmen eine 1 zu, wenn die jeweiligen 
  # Unterschiede zum gegebenen testNiveau signifikant sind. Gehe dabei vom
  # besten zum schlechtesten Algorithmus vor
  for(i in rownames(connectMatrix)){
    # Fallunterscheidung, da die Zeilen- bzw Spaltennamen von pValues nicht alle 
    # Algorithmennamen enthalten.
    # -> Es wird die Zeile und / oder die Spalte zu dem jeweiligen Algorithmus
    #    durchgegangen. Falls pValues[i,j] <- testNiveau so weise der passenden
    #    Stelle in der connectMatrix eine 1 zu
    
    # Entsprechende Zeile durchgehen:
    if(i %in% rNames){
      j <- 1
      while(!is.na(pValues[i, cNames[j]])){
        if(pValues[i, cNames[j]] < testNiveau){
          if(connectMatrix[cNames[j],i] == 0){
            connectMatrix[i,cNames[j]] <- 1
          }
        }
        j <- j + 1
        if(j > ncol(pValues)){
          break
        }
      }
    }
    # Entsprechende Spalte durchgehen:
    if(i %in% cNames){
      k <- nrow(pValues)
      while(!is.na(pValues[rNames[k],i])){
        if(pValues[rNames[k],i] < testNiveau){
          if(connectMatrix[rNames[k],i] == 0){
            connectMatrix[i,rNames[k]] <- 1
          }
        }
        k <- k - 1
        if(k == 0){
          break
        }
      }
    }
  }
  return(connectMatrix)
}

