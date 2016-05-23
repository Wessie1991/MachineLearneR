#' @title  Finding Outliers
#' @description temp
#' @export
findOutlier <- function(mydata, cutoff) {
  ## Calculate the sd
  sds <- apply(mydata, 2, sd, na.rm = TRUE)
  ## Identify the cells with value greater than cutoff * sd (column wise)
  outliers <- mapply(function(d, s) {
    which(d > cutoff * s)
  }, mydata, sds)
  mydata.Outlier.Removed <- removeOutlier(mydata, outliers)
  return(mydata.Outlier.Removed)
}

#' @title  removing Outliers
#' @description temp
#' @export
removeOutlier <- function(mydata, outliers) {
  result <- mapply(function(d, o) {
    res <- d
    res[o] <- NA
    return(res)
  }, mydata, outliers)
  return(as.data.frame(result))
}
