#' @title  Data manipulation
#' @description Inside the ML function this function is responsible for manipulating the data in the dataset. This function can be used seperately to manipulate data for any given dataframe.
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
  if (selectedNormalization=="normalize") {
    print(paste("Applying", selectedAverage, "normalization."))
    averageData <- apply(mydata[,analyticalVariables], 2, eval(parse(text=selectedAverage)), na.rm=TRUE)
   # print(paste("The following", selectedAverage, "average has been calculated for each column:"))
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
    for (value in splitColValues) {
      splitColRows <- which(mydata[,splitCol]==value)
      averageSubset <- apply(mydata[splitColRows,analyticalVariables], 2,  eval(parse(text=selectedAverage)), na.rm=TRUE)
      mydata[splitColRows,analyticalVariables] <- sapply(analyticalVariables, function(x) normalize(x, mydata[splitColRows,x], averageSubset))
      logDF <- cbind(logDF,as.data.frame(averageSubset))
    }
    splitColValues <- paste(splitCol, splitColValues, sep = ":" )
    colnames(logDF) <- splitColValues
  } else if (selectedNormalization=="normalize.onClass") {
    print(paste("Applying", selectedAverage, "normalization on class", controlVariable))
    classSubset <- mydata[mydata[controlVariable] == controlValue,][,analyticalVariables]
    averageClassSubset <- apply(classSubset, 2, eval(parse(text=selectedAverage)), na.rm=TRUE)
    print(paste("The following", selectedAverage, "average has been calculated for each column in the class", controlVariable, "with value", controlValue))
    mydata[,analyticalVariables] <- sapply(analyticalVariables, function(x) normalize(x, mydata[,x], averageClassSubset))
    rowNames <- paste(analyticalVariables,"OnControlValue" ,controlValue, sep = "-" )
    logDF <- data.frame(averageClassSubset, row.names = rowNames)
    colnames(logDF) <- "normalization_value"

    } else if (selectedNormalization == "normalize.onClass.splitCol") {
    print(paste("Applying", selectedAverage, "normalization on splitCol", splitCol, "and class", controlVariable))
    splitColValues <- unique(mydata[,splitCol])
    print(paste("The dataset has been split into", length(splitColValues), "subsets based on the column", splitCol))
    rowNames <- paste(analyticalVariables,"OnControlValue" ,controlValue, sep = "-" )
    logDF <- data.frame(row.names = rowNames)
    for (value in splitColValues) {
      splitColRows <- which(mydata[,splitCol]==value)
      classSubset <- mydata[mydata[controlVariable] == controlValue,][splitColRows, analyticalVariables]
      averageClassSubset <- apply(classSubset, 2, eval(parse(text=selectedAverage)), na.rm=TRUE)

      #print(paste("The following", selectedAverage, "average has been calculated for each column in the class", controlVariable,
          #        "with value", controlValue, "for the splitCol subset", value, ":"))
      naCols <- which(is.na(averageClassSubset))
      averageClassSubset[naCols] <- colsum[naCols]
      logDF <- cbind(logDF,as.data.frame(averageClassSubset))
      mydata[splitColRows,analyticalVariables] <- sapply(analyticalVariables, function(x) normalize(x, mydata[splitColRows,x], averageClassSubset))
    }
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
  if (selectedTransformation) {


    print("Transforming data")
    missingFieldsBefore=sum(is.na(mydata[,analyticalVariables]))
    if(!multiThreadFase){
      kurto <- list()
      skew <- list()
      skew[analyticalVariables]=apply(mydata[,analyticalVariables], 2, moments::skewness, na.rm = TRUE)
      kurto[analyticalVariables]=apply(mydata[,analyticalVariables], 2, moments::kurtosis, na.rm=TRUE)
    }

    mydata[,analyticalVariables] = mapply(transformData, mydata[,analyticalVariables], skew)

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


  # Data to be returned.
  returned <- list(mydata=mydata, kurto=kurto, skew=skew, logDMMdf=logDF)
  return(returned)
}
