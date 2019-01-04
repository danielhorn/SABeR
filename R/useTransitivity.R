# TODO: function from NJ. It does what it should do!
# However, probably must be refactored


# Funktion - Entfernt alle durch die Transitivitaetseigenschaft ueberfluessigen
#            Verbindungen
# Eingabe: 
# - cmatrix: Eine gegebene Connectionmatrix
# - searchForTransitivity: Eine Funktion, welche durch Transitivitaet 
#                          ueberfluessigen Verbindungen anhand einer cmatrix
#                          erkennt
# Rueckgabe:
# Eine Connectionmatrix ohne ueberfluessige Verbindungen
useTransitivity <- function(cmatrix,
                            searchForTransitivity){
  cnames <- colnames(cmatrix)
  rnames <- rownames(cmatrix)
  nc <- length(cnames)
  nr <- length(rnames)
  for(i in 1:(nr)){
    for(j in 1:nc){
      if(cmatrix[i,j] == 1){
        transi <- searchForTransitivity(
          result = c(),
          cmatrix = cmatrix,
          cnames = cnames,
          start = rnames[i],
          ziel = cnames[j],
          first = TRUE)
        if(transi){
          cmatrix[i,j] <- 0
        }
      }
    }
  }
  return(cmatrix)
}
