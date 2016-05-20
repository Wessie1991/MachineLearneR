# Zorgt ervoor dat er log transformatie uitgevoerd kan worden op elke waarde.
# Een log transformatie kan niet worden uitgevoerd op 0 waardes dus deze
# functie veranderd 0 waardes in iets anders.

range01 <- function(x){
  return(x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
}
