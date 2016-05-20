#' @title  Function writing a data frame to an .Rdata File
#' @description
#' \code{Filesplit} generates a number, based on the splitCol parameter, which it used to create an .Rdata file of the input data frame, xSet, which it saves.
#' @param xSet A dataframe
#' @param splitCol A String, specifying a column of the xSet dataframe, which is used to generate the filename.
#' @return fileName A String, specifying the name of the file created by the function.
#' @examples FileSplit(mydata, splitCol="Col1")
Filesplit <- function(xSet, splitCol){

  splitnumber=xSet[1, splitCol]

  fileName=paste("subset", "_", stringr::str_pad(splitnumber, 3, pad = "0"), ".Rdata", sep="")

  save(xSet, file=fileName)
  rm(xSet)

  return(fileName)
}

