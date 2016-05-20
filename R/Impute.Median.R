# Function for using Median
impute.median=function(x){
return(replace(x, is.na(x), median(x, na.rm = TRUE)))

}
