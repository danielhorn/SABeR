# Funktion - Entfernt alle Verbindungen zwischen zwei Algorithmen, welche 
#            bereits fuer ein kleineres Signifikanzniveau signifikant sind
# Eingabe:
# - connections: Eine Liste von connection Matrizen zu den gegebenen 
#                Testniveaus
# Rueckgabe:
# Eine Liste von connection Matrizen zu den gegebenen Testniveaus ohne ueber-
# fluessige Verbindungen aufgrund der Niveaus (Transitivitaet noch nicht
# ausgenutzt)
transform_connections <- function(connections){
  if(length(connections) == 1){
    return(connections)
  }
  actual_edges <- connections[[1]]
  for(i in 2:length(connections)){
    connections[[i]][actual_edges == connections[[i]]] <- 0
    actual_edges[actual_edges != connections[[i]]] <- 1
  }
  return(connections)
}
