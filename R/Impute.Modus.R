# function for calculation modus
# Let op bij categoriale waarden, of bij een aantal waarden wat oneven is, dan kan je een double als modus krijgen.

impute.modus <- function(x){
  return(replace(x, is.na(x), modus(x)))
}

