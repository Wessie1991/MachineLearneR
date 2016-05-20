# ff afmaken!
# o.b.v. classe imputen

onclass <- function(df,impClass, func){
  ### sla deze waardes per classe op zodat deze niet opnieuw moet worden berekend
  ### colSums(df[,analyticalVariables], na.rm = T)
  temp <- print(zoo::na.aggregate(df, by=impClass, na.rm = T,FUN=eval(parse(text=func))))
  if (any(is.na(temp))){
    print("Nu dus")
  }
  return(zoo::na.aggregate(df, by=impClass, na.rm = T,FUN=eval(parse(text=func)))) # modus, mean, median.

}
