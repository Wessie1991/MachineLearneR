#' @title  Data manipulation
#' @description Inside the \code{\link{ML}} function this function is responsible for manipulating the data in the dataset. This function can be used separately to manipulate data for any given dataframe.
#' @param mydata a dataframe containing the data to be analysed
#' @param analyticalVariables A vector containing the names of the variables in the dataset that should be analysed
#' @param selectedNormalization A string specifying the method for normalizing the data
#' @param selectedAverage A string describing the basis for data normalization, can be mean, median or modus
#' @param selectedTransformation A string specifying the method for transforming the data
#' @param selectedStandardization A string specifying the method for standardizing the data
#' @param removeOutliers A boolean indicating if outliers should be removed in data normalization
#' @param splitCol A string specifying the name of the variable on which the dataset is splitted in subsets
#' @param controlVariable A string specifying the variable on which the data should be normalized
#' @param controlValue A number specifying the value of the controlVariable on which the data should be normalized
#' @param multiThreadFase A boolean indicating if this function is excecuted in parallel processing
#' @param skew A list containing the sample-calculated skewness per variable, only required if multiThreadFase=TRUE
#' @param kurto A list containing the sample-calculated kurtosis per variable, only required if multiThreadFase=TRUE
#' @examples
#' require("data.table")
#' data <- fread("data.csv")
#' data <- data.frame(data)
#' dMM(mydata=data, analyticalVariables=c("var1", "var2"), selectedNormalization=normalize.onClass, controlVariable='var2', controlValue=22, splitCol='var1')
#' @export
dMM <- function(mydata, analyticalVariables, selectedNormalization, selectedAverage,
                selectedTransformation, selectedStandardization, removeOutliers,
                splitCol, controlVariable, controlValue, multiThreadFase, kurto, skew)
{


  #################
  # Normalization #
  #################
  # Mydata aanvoer moet aangepast worden zodat de classe kolom en splitCol kolom hier ook bij zitten
  # als deze niet in analyticalVariables zitten.
  colsum <- colSums(mydata[,analyticalVariables])
  missingFieldsBefore=sum(is.na(mydata[,analyticalVariables]))
    gc(reset=T);mallinfo::malloc.trim(pad=pryr::mem_used())
  print(paste("in de manupulatie", (pryr::mem_used()/1024)/1024, "mb used.", sep=' '))
  if (selectedNormalization=="normalize") {
    print(paste("Applying", selectedAverage, "normalization."))
    averageData <- plyr::aaply(mydata[,analyticalVariables],2,  function(x) eval(parse(text=paste(selectedAverage, "(x[,1], na.rm=TRUE)", sep=""))))
    #print(paste("The following", selectedAverage, "average has been calculated for each column:"))
    #print(paste(analyticalVariables, ":", averageData))
    mydata[,analyticalVariables] <- sapply(analyticalVariables, function(x) normalize(x, mydata[,x], averageData))
    logDF<- as.data.frame(averageData)
    colnames(logDF) <- "normalizationValue"

  } else if (selectedNormalization=="normalize.splitCol") {
    print(paste("Applying", selectedAverage, "normalization on splitCol", splitCol))
    splitColValues <- unique(mydata[,splitCol])
    print(paste("The dataset will be split into", length(splitColValues), "subsets based on the column", splitCol))
    print(paste("The following", selectedAverage, "average has been calculated for each column for each splitcol subset:"))
    #mydata[splitColRows,analyticalVariables]
    logDF <- data.frame(row.names = analyticalVariables)
    listData <- lapply(splitColValues, function(Value) splitColNormalize(Value, mydata, splitCol, selectedAverage, analyticalVariables, logDF) )
    mydata[,analyticalVariables] = do.call(rbind, lapply(1:length(listData), function(x) return(listData[[x]]$mydata)))
    logDF <- do.call(cbind,lapply(1:length(listData), function(x) return(listData[[x]]$logDF)))
    colnames(logDF) <- paste("normalizationSplitColValue", splitColValues, sep = "-" )

  } else if (selectedNormalization=="normalize.onClass") {
    print(paste("Applying", selectedAverage, "normalization on class", controlVariable))
    classSubset <- mydata[mydata[controlVariable] == controlValue,][,analyticalVariables]
    averageClassSubset <- apply(classSubset, 2, eval(parse(text=selectedAverage)), na.rm=TRUE)
    print(paste("The following", selectedAverage, "average has been calculated for each column in the class", controlVariable, "with value", controlValue))
    mydata[,analyticalVariables] <- sapply(analyticalVariables, function(x) normalize(x, mydata[,x], averageClassSubset))
    rowNames <- paste(analyticalVariables,"OnControlValue", controlValue, sep = "-" )
    logDF <- data.frame(averageClassSubset, row.names = rowNames)
    colnames(logDF) <- "normalization_value"

  } else if (selectedNormalization == "normalize.onClass.splitCol") {

    print(length(analyticalVariables))
    print(paste("Applying", selectedAverage, "normalization on splitCol", splitCol, "and class", controlVariable))
    splitColValues <- unique(mydata[,splitCol])
    print(paste("The dataset has been split into", length(splitColValues), "subsets based on the column", splitCol))
    rowNames <- paste(analyticalVariables,"OnControlValue" ,controlValue, sep = "-" )
    logDF <- data.frame(row.names = rowNames)
    listData <- lapply(splitColValues, function(value) splitColOnClassNormalize(value, mydata, splitCol,
                                                                                selectedAverage, analyticalVariables,
                                                                                logDF,controlValue, controlVariable, colsum))
    logDF <- do.call(cbind,lapply(1:length(listData), function(x) return(listData[[x]]$logDF)))
    mydata[,analyticalVariables] = do.call(rbind, lapply(1:length(listData), function(x) return(listData[[x]]$mydata)))
    splitColValues <- paste(splitCol, splitColValues, sep = ":" )
    colnames(logDF) <- splitColValues

  } else {
    print("No normalization applied.")
  }

  if( selectedNormalization %in% c("normalize", "normalize.splitCol", "normalize.onClass", "normalize.onClass.splitCol")){
    missingFieldsAfter=sum(is.na(mydata[,analyticalVariables]))
    if (missingFieldsBefore==missingFieldsAfter) {
      percentageNA=0
    } else {
      percentageNA=round(100*(missingFieldsAfter/missingFieldsBefore), 2)
    }
    if (percentageNA!=0) {
      print(paste(missingFieldsAfter-missingFieldsBefore, " missing fields created after normalization ", percentageNA, "%", sep=""))
    }
  }
  ##################
  # Transformation #
  ##################
  gc(reset=T);mallinfo::malloc.trim(pad=pryr::mem_used())
  if (selectedTransformation) {
    print("Transforming data")
    missingFieldsBefore=sum(is.na(mydata[,analyticalVariables]))
    if(!multiThreadFase){
      kurto <- list()
      skew <- list()
      skew[analyticalVariables]=apply(mydata[,analyticalVariables], 2, moments::skewness, na.rm = TRUE)
      kurto[analyticalVariables]=apply(mydata[,analyticalVariables], 2, moments::kurtosis, na.rm=TRUE)
    }
    print("Transforming Skeww")
    mydata[,analyticalVariables] = mapply(transformData, mydata[,analyticalVariables], skew)
    print("klaar")
    missingFieldsAfter=sum(is.na(mydata[,analyticalVariables]))

    if (missingFieldsBefore==missingFieldsAfter) {
      percentageNA=0
    } else {
      percentageNA=round(100*(missingFieldsAfter/missingFieldsBefore), 2)
    }

    if (percentageNA!=0) {
      print(paste(missingFieldsAfter-missingFieldsBefore, " missing fields created after transformation ", percentageNA, "%", sep=""))

    }
  }
  ###################
  # Standardization #
  ###################
  missingFieldsBefore=sum(is.na(mydata[,analyticalVariables]))
  # Traditional Z score standardization.
  gc(reset=T);mallinfo::malloc.trim(pad=pryr::mem_used())
  if (selectedStandardization=='traditionalZscore'){
    print("Standardizing data with a tradition Z-score")
    mydata[,analyticalVariables]=apply(mydata[,analyticalVariables], 2, function(x) x=x+10)
    mydata[,analyticalVariables]=apply(mydata[,analyticalVariables], 2, scale)
  } else if (selectedStandardization=='robustZscore') {
    print("Standardizing data with a robust Z-score")
    mydata[,analyticalVariables]=apply(mydata[,analyticalVariables], 2, function(x) x=x+10)
    mydata[,analyticalVariables]=apply(mydata[,analyticalVariables], 2, robustZscore)
  } else {
    print("No standardization applied")
  }

  if(selectedStandardization %in% c("traditionalZscore", "robustZscore")){
    missingFieldsAfter=sum(is.na(mydata[,analyticalVariables]))
    if (missingFieldsBefore==missingFieldsAfter) {
      percentageNA=0
    } else {
      percentageNA=round(100*(missingFieldsAfter/missingFieldsBefore), 2)
    }

    if (percentageNA!=0) {
      print(paste(missingFieldsAfter-missingFieldsBefore, " missing fields created after Standardization ", percentageNA, "%", sep=""))
    }
  }

  if (removeOutliers) {
    mydata[,analyticalVariables] <- findOutlier(mydata[,analyticalVariables], 8)
  }

  returned <- list(mydata=mydata, kurto=kurto, skew=skew, dfNormalize=logDF)
  return(returned)
}


#' @title Normalizing data the data
#' @description Inside the \code{\link{dMM}} function this function is responsible for normalizing the dataset.
#' @param currentVariable temp
#' @param currentVariableValues temp
#' @param averageData temp
#' @export
normalize <- function(currentVariable, currentVariableValues, averageData) {
  subset <- which(names(averageData)==currentVariable)
  normalizedData <- currentVariableValues/averageData[subset]
  return(normalizedData)
}

#' @title Checking the range of a vector
#' @description temp
#' @param x A vector
#' @export
range01 <- function(x){
  return(x-min(x, na.rm=TRUE))/(max(x, na.rm=TRUE)-min(x, na.rm=TRUE))
}

#' @title Transforming data the data
#' @description Inside the \code{\link{dMM}} function this function is responsible for log transforming the variables based on their skewness.
#' @param x A vector
#' @param skew A vector
#' @export
transformData = function(x, skew) {
  gc(reset=T);mallinfo::malloc.trim(pad=pryr::mem_used())
  if(!is.na(skew)){
    if (skew>5.00) {
      return(log(range01(x)+1))
    } else if (skew<5.00) {
      return(sqrt(range01(x)+1))
    }
  }
  return(x)
}

#' @title  RobustZscore standardisation.
#' @description Inside the \code{\link{dMM}} function this function is responsible for standardizing the variables based on their robust Z score.
#' @param x A vector
#' @export
robustZscore=function (x) {
  meanAD=function (x) {
    mean=mean(x, na.rm=TRUE)
    y=mean(abs((x-mean)), na.rm=TRUE)
    return(y)
  }

  med <- median(x,na.rm=TRUE)
  nas<-sum(!is.na(x))
  if (nas !=0){
    mad = mad(x, na.rm=TRUE)
  }
  else {
    mad=0
  }

  meanADscore <- meanAD(x)

  if (mad==0) {
    y <- (x-med)/(1.253314*meanADscore)
  } else {
    y <- (x-med)/(1.4826*mad)
  }
  return(as.matrix(y))
}

