# function for calculation modus
# Let op bij categoriale waarden, of bij een aantal waarden wat oneven is, dan kan je een double als modus krijgen.
# Zoek nog even uit of deze functie wel wordt gebruikt!
modus <- function(x){
  return (x[which.max(tabulate(match(x, unique(x))))])
}

