#' @title  Function for loading input for the ML package
#' @description Inside the \code{\link{ML}} function this function is responsible for setting the number of cores to be used during parallel processing, based on the input parameters. Based on the sampleSize parameter, the function generates a stratified sample of the mydata data frame. The function \code{\link{ExcludedVariablesModel}} is called on the sample, which reduces the number of variables(columns). The \code{\link{ExcludedVariablesModel}} function returns a Vector with columnnames that should be analyzed, analyticalVariables, which this function uses to reduce the number of variables in de mydata data frame. In parallel processing, this function then splits the (big) mydata data frame, to smaller subsets in .Rdata files by calling the \code{\link{Filesplit}} function in combination with the \code{\link[plyr]{dlply}} function.
#' @param mydata A dataframe
#' @param dir A location specifying the working directory. This is where the .Rdata files of the subsets will be saved.
#' @param subsetCutoff A number, specifying the minimal number of records that must be in a subset.
#' @param splitCol A string, specifying the column on which the data set should be splitted.
#' @param usedCores A number, specifing the number of cores to be used in parallel processing, this cannot be more than the maximum number of cores availible on the system.
#' @param sampleSize A number, in range 0-1, specifying the fraction of the mydata dataframe to be used as sample data set. Default is 0.05 (5\%)
#' @param selectedGarbageCutoff A number, in range 0-1, specifying the fraction of records in a variable(column) that is accepted to be NA. Default is 0.10(10\%).
#' @param selectedCorrelationCutoff A number, in range 0-1, specifying the fraction of the maximum accepted ammount of correlation between two variables.
#' @param removeCata A boolean, enabling/disabling the removal of catagorical variables(columns) from the mydata data frame. Default is TRUE, catagorical varibiables are removed.
#' @param metaVariables A character vector, containing the names of the variables(columns) that should NOT be analyzed, but should remain in de mydata data frame as describing variables.
#' @export

GetInput <- function(mydata, dir, subsetCutoff,splitCol, usedCores,
                     sampleSize, selectedGarbageCutoff, selectedCorrelationCutoff, removeCata,
                     metaVariables, controlVariable,classifierClass)
{
  # set directory for output files.
  setwd(dir)
  ###### Impute all NAN, and Inf / -Inf values with NA's #####
  # Check for maximal nr. of cores.
  maxCores <- parallel::detectCores()[1]

  # check if usedcores is not NULL, check if usedcores is smaller than the availble cores
  cores <- if(length(usedCores) > 0 )(if(usedCores > maxCores) maxCores else usedCores) else maxCores

  # check if subset content per core is above threshold
  cores <- if(nrow(mydata)/cores <= subsetCutoff) ceiling(nrow(mydata)/subsetCutoff) else cores

  # Check if the number of sets when selecting on specific column is smaller
  # than the allowed number of cores.
  # Check if there are NA's in the splitcol.
  if(length(splitCol > 0) && !any(is.na(mydata[,splitCol]))){
    nColUniq = length(unique(mydata[,splitCol]))
    cores <- if (nColUniq < cores) nColUniq else cores
  }else{
    mydata$subSet <- seq.int(nrow(mydata))%%cores
    splitCol <- "subSet"
  }

  metaVariables <- if(length(metaVariables) > 0 ) union(metaVariables, splitCol) else splitCol
  metaVariables <- if(length(controlVariable) > 0 ) union(metaVariables, controlVariable) else metaVariables
  metaVariables <- if(length(classifierClass) > 0 ) union(metaVariables, classifierClass) else metaVariables


  # Generate stratafied sample
  StrataVect <- round(as.numeric(table(mydata[splitCol]) * sampleSize),0)
  mydata <- mydata[order(mydata[splitCol]),]
  gc(reset=T);mallinfo::malloc.trim(pad=pryr::mem_used())
  # Create sample:
  samp <- sampling::strata(mydata,stratanames = c(splitCol), size=StrataVect,
                           method = "srswr")
  samp <- sampling::getdata(mydata,samp)

  ## Protect MetaVariables from Exclusion
  analyticalVariables <- names(samp)[! names(samp) %in% metaVariables]
  garbageList <- ExcludedVariablesModel(samp,analyticalVariables, selectedCorrelationCutoff=selectedCorrelationCutoff,
                                        selectedGarbageCutoff=selectedGarbageCutoff, removeCata=removeCata)
  #####################
  x <- append(garbageList$analyticalVariables, metaVariables)
  samp <- samp[,x]
  mydata <- mydata[,x]
  gc(reset=T);mallinfo::malloc.trim(pad=pryr::mem_used())
  # Split the files parallel.
  #doMC::registerDoMC(cores=cores)
  cl <- snow::makeCluster(cores, outfile="")
  doSNOW::registerDoSNOW(cl)
  files <- as.character(plyr::dlply(mydata, match(splitCol, names(mydata)), Filesplit, splitCol, .parallel = T))
  snow::stopCluster(cl)
  returnList <- list(samp=samp,files=files,splitCol=splitCol,analyticalVariables=garbageList$analyticalVariables,cores=cores,
                     garbagedf=garbageList$garbageDf, metaVariables=metaVariables, colSumsBigdata=colSums(mydata[,garbageList$analyticalVariables],na.rm=T))
  return(returnList)

}

#' @title  Writing data frame subsets to an .Rdata File
#' @description This function is used inside the \code{\link{GetInput}} function to split a data frame into subsets by using \code{\link[plyr]{dlply}}.\code{Filesplit} generates a number, based on the splitCol parameter, which it uses to create an .Rdata file of the input data frame, xSet, which it saves.
#' @param xSet A dataframe
#' @param splitCol A String, specifying a column of the xSet dataframe, which is used to generate the filename.
#' @examples FileSplit(mydata, splitCol="Col1")
#' @export
Filesplit <- function(xSet, splitCol){

  splitnumber=xSet[1, splitCol]
  fileName=paste("subset", "_", stringr::str_pad(splitnumber, 3, pad = "0"), ".Rdata", sep="")
  print(class(xSet))
  save(xSet, file=fileName)
  rm(xSet);gc(reset=T);mallinfo::malloc.trim(pad=pryr::mem_used())

  return(fileName)
}


