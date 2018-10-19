# Funktion - Erzeugt einen Vektor in dem alle Verbindungen zu der gegebenen
#            Connectionmatrix stehen
# Eingabe:
# - c_matrix: Die gegebene Connectionmatrix
# Rueckgabe:
# Ein Vektor der Form ("Algo A", "Algo C", "Algo C", "Algo E", usw...)
#   -> Algo A ist mit Algo C und Algo C mit Algo E verbunden
design_edges <- function(c_matrix){
  con_string <- NULL
  nr <- nrow(c_matrix)
  rnames <- rownames(c_matrix)
  cnames <- colnames(c_matrix)
  for(i in 1 : nr){
    for(j in 1 : nr){
      # Die Verbindung wird hinzugefuegt falls eine 1 an der entsprechenden
      # Stelle steht
      if(c_matrix[i,j] == 1){
        con_string <- c(con_string, rnames[i], cnames[j] )
      } 
    }
  }
  return(con_string)
}

