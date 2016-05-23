# Data transformatie.
# Afhankelijk van de gegeven skew, wordt er een log of sqrt transformatie
# toegepast.
transformData = function(x, skew) {
  gc(reset=T);mallinfo::malloc.trim(pad=mem_used())
  if(!is.na(skew)){
    if (skew>5.00) {
      return(log(range01(x)+1))
      #print("Applying log transformation.")
    } else if (skew<5.00) {
      return(sqrt(range01(x)+1))
      #print("Applying square root transformation.")
    }
  }

  return(x)
}


