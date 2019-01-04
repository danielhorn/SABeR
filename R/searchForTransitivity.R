# TODO: function from NJ. It does what it should do!
# However, probably must be refactored


# Funktion - erkennt welche durch Transitivitaet  ueberfluessigen Verbindungen 
             # anhand einer cmatrix
# Eingabe:
# - result: Zu Beginn leer. Nur zur rekursiven Formulierung da (wie es gefuellt
#           wird, wird innerhalb der Funktion erklaert)
# - cmatrix: Die jeweils aktuellste Connectionmatrix
# - cnames: Sind im Grunde nur die Namen aller Algorithmen
# - start: Von welchem Algorithmus geht der Pfeil aus
# - ziel: Auf welchen Algorithmus zeigt er Pfeil  (start und ziel spiegeln die zu
#         untersuchende Verbindunge wieder)
# first: Gibt an ob es sich um den ersten Aufruf der Funktion handelt (auch hier
#        wieder fuer die rekursive Formulierung notwendig)
# Rueckgabe:
# TRUE, falls die zu untersuchende Verbindung durch andere Verbindungen wieder-
# gespielgent werden kann. Ansonsten wird FALSE zurueckgegeben
searchForTransitivity <- function(result = c(),
                                  cmatrix, 
                                  cnames, 
                                  start, 
                                  ziel,
                                  first = TRUE){
  # Wenn der Anfang gleich das Ende ist, so ist eine aus transitiver Sicht 
  # gesehenen Verbindung unnoetig und es wird TRUE zurueckgegeben 
  if(start == ziel) { return( TRUE )}
  # Welche Verbindungen gehen von dem Startpunkt aus:
  goTo <- cnames[cmatrix[start,] == 1]
  # Falls wir im ersten Schritt sind darf die zu untersuchende Verbindunge nicht
  # als Moeglichkeit fuer eine weitere Verbindung hinzugezogen werden. 
  if( first ){
    goTo <- goTo[goTo != ziel]
    first <- FALSE
  }
  # Fahre weiter fort, falls mindestens eine Verbindung zusaetzlich zu der zu
  # untersuchenden Verbindung existiert
  if( 
    (sum(cmatrix[start,]) > 0 && first == FALSE) ||
    (sum(cmatrix[start,]) > 1 && first == TRUE)
  ){
    # Gehe in alle Verbindungen hinein und schreibe das jeweilige Resultat in
    # den Result Vektor
    for(i in goTo){
      r <- searchForTransitivity(result, cmatrix, cnames, i, ziel, first = FALSE)
      result <- c(result, r)
    }
  }
  # Ist in dem Resultvektor mindestens eine 1 gefunden worden, so ist die zu
  # untersuchende Verbindung ueberfluessig und es wird TRUE zurueckgegeben 
  # (ansonsten wird FALSE zurueckgegeben)
  return(sum(result) > 0)
}