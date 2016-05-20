# Data transformatie.
# Afhankelijk van de gegeven skew, wordt er een log of sqrt transformatie
# toegepast.
transformData = function(x, skew) {

  if(!is.na(skew)){
  if (skew>5.00) {
    x=log(range01(x)+1)
    #print("Applying log transformation.")
  } else if (skew<5.00) {
    x=sqrt(range01(x)+1)
    #print("Applying square root transformation.")
  } else {
    x=x
    #print("No transformation applied.")
  }}
  return(x)

}


