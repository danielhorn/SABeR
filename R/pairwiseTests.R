# Funktion - 
# Eingabe:
# - sectionObjects: Eine Liste bei der in jedem Element der zu dieser Gruppe 
#                   zugehoerige Teil der Daten in Form eines data.frame stehen
# - perfName: Welcher Parameter soll als Performanzindikator genutzt werden
#             (character)
# - confName: Der Name der Spalte in dem die Algorithmen stehen (Character)
# - algo.Name: Zu welchen Algorithmen sind in data Daten vorhanden
#              (Vektor von Charactern)
# Rueckgabe:
# Eine Liste der Laenge 2:
# - results: Eine Liste mit den geweiligen Matrizen zu den jeweiligen Cluster-
#            gruppen, in denen die jeweiligen p-Werte der einzelnen paarweisen
#            Tests stehen
# - rankMatrix: Eine Matrix in dem in Zeile i und Spalte j der Durchschnittsrang
#               der Performance des j-ten Algorithmus in der i-ten Clustergruppe
#               ist
pairwiseTests <- function(data.clusterd, perfName, algoName, replName, algo.Name){
  
  # pairwaise tests with nemenyi test
  ## TODO: Warum dist = Chisq?
  results <- lapply(data.clusterd, function(d)
    posthoc.kruskal.nemenyi.test(d[, perfName], d[, algoName], dist = "Chisq")
  )
  
  # Calculate mean ranks for each algorithm in each cluster
  
  # First: function to get ranks for a single experiment
  getRanks <- function(s) {
    # make sure data has a well-defined order and calculate ranks
    s <- s[order(s[, algoName]), ]
    ranks <- rank(s[, perfName])
    names(ranks) <- s[, algoName]
    ranks
  }
  
  # Second: function to get mean ranks for a complete cluster
  getMeanRanks <- function(d) {
    # split data into single experiments and calculate mean ranks
    blocks <- apply(d[, c(".expID", replName)], 1, paste, collapse = "_")
    rowMeans(sapply(split(d, blocks), getRanks))
  }

  rank.matrix <- sapply(data.clusterd, getMeanRanks)
  
  return(list(p.values = results, rank.matrix = rank.matrix))
}
