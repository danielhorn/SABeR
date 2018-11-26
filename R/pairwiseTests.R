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
  
  results <- lapply(data.clusterd, function(d) {
    d$.exp.repl.id <- apply(d[, c(".expID", replName)], 1, paste, collapse = "_")
    pairwiseNemenyiTests(d[, perfName], groups =  d[, algoName],
      blocks = d[, ".exp.repl.id"])
  })
  
  
  p.values <- extractSubList(results, "statistic", simplify = FALSE)
  rank.matrix <- extractSubList(results, "rank.means", simplify = TRUE)
  
  return(list(p.values = p.values, rank.matrix = rank.matrix))
}
