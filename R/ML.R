#' @title  Main function for running the ML package
#' @description
#' \code{ML}
#' @param bigdata A dataframe or flatfile containing a dataset.
#' @param dir A location specifying the working directory. Default is \code{\link{getwd}}
#' @param subsetCutoff A number, specifying the minimal number of records that must be in a subset. Default is 10000
#' @param splitCol A string, specifying the column on which the data set should be splitted.
#' @param usedCores A number, specifing the number of cores to be used in parallel processing.
#' @param sampleSize A number, in range 0-1, specifying the fraction of the mydata dataframe to be used as sample data set. Default is 0.05 (5\%).
#' @param selectedGarbageCutoff A number, in range 0-1, specifying the fraction of records in a variable(column) that is accepted to be NA. Default is 0.10(10\%).
#' @param selectedCorrelationCutoff A number, in range 0-1, specifying the fraction of the maximum accepted ammount of correlation between two variables.
#' @param selectedMissingData A String, specifying the method to be used when calling the imputation model function; cMIM which should be one of the following methods:
#' \itemize{
#'    \item "CWD"
#'    \item "impute.modus"
#'    \item "impute.median"
#'    \item "impute.mean"
#'    \item "onclass.modus"
#'    \item "onclass.median"
#'    \item "onclass.mean"
#'    \item "regression"
#'  }
#' @param removeCata A boolean, enabling/disabling the removal of catagorical variables(columns) from the mydata data frame. Default is TRUE, catagorical variables are removed.
#' @param
#'rm
#' nnet, rpart, survival
#'



ML <- function(bigdata, dir=default.dir, subsetCutoff=default.subsetCutoff, splitCol=default.splitCol,
               usedCores=default.usedCores, sampleSize=default.sampleSize, createPlots = default.createPlots,
               selectedGarbageCutoff=default.selectedGarbageCutoff, selectedCorrelationCutoff=default.selectedCorrelationCutoff,
               selectedMissingData=default.selectedMissingData, classifierClass=default.classifierClass, removeCata=default.removeCata,
               metaVariables=default.metaVariables, selectedNormalization=default.selectedNormalization, selectedTransformation=default.selectedTransformation,
               selectedAverage=default.selectedAverage, selectedStandardization=default.selectedStandardization, controlVariable=default.controlVariable,
               controlValue=default.controlValue, removeOutliers=default.removeOutliers, selectedDataReductionMethod=default.selectedDataReductionMethod,
               selectedNumberOfDimensions=default.selectedNumberOfDimensions,faRotate=default.faRotate,
               faMethodScores=default.faMethodScores, pcMethod=default.pcMethod, selectedTrainingSize=default.selectedTrainingSize) {

  ########################
  ## Check input data  ##
  #######################
  if(is.data.frame(bigdata)){
    mydata <- data.frame(bigdata)
    rm(bigdata)
    gc(reset=T)
  }else if (file.exists(bigdata)){
    mydata <- data.table::fread(bigdata)
    mydata <- data.frame(mydata)
    rm(bigdata)
    gc(reset=T)
  }else {
    stop("Invalid input, input must be a dataframe or a file.")
  }


  ControlParameters(as.list(environment())[-1], colnames(mydata))


  #############################################
  ## Get data + Create & Apply Garbage model ##
  #############################################
  varList <- GetInput(mydata=mydata,dir=dir,subsetCutoff=subsetCutoff,splitCol=splitCol,
                      usedCores=usedCores, sampleSize=sampleSize,selectedGarbageCutoff=selectedGarbageCutoff,
                      selectedCorrelationCutoff=selectedCorrelationCutoff,removeCata=removeCata, metaVariables=metaVariables,
                      controlVariable=controlVariable, classifierClass=classifierClass)


  rm(mydata)
  gc(reset=T)
  mallinfo::malloc.trim()

  ###################################
  ## Apply Data Manipulation Model ##
  ###################################
  analyticalVariables <- varList$analyticalVariables
  metaVariables <- varList$metaVariables
  splitCol <- varList$splitCol
  files <- varList$files
  temp <- dMM(mydata=varList$samp, analyticalVariables = analyticalVariables, selectedNormalization=selectedNormalization,
              selectedAverage=selectedAverage, selectedTransformation=selectedTransformation, selectedStandardization=selectedStandardization,
              removeOutliers=removeOutliers, splitCol=splitCol, controlVariable=controlVariable, controlValue=controlValue, multiThreadFase=F)




  varList$samp <- data.frame(temp$mydata,varList$samp)
  colnames(varList$samp) <- gsub(".1", ".raw", colnames(varList$samp), fixed = T)
  kurto <- list(temp$kurto)
  skew <- list(temp$skew)
  varList['logDMMdf'] <- temp$logDMMdf

  rm(temp)
  gc(reset=T)
  mallinfo::malloc.trim()


  ########################################
  # Apply Multiple Imputation on Sample ##
  ########################################
  if(any(is.na(varList$samp))){
       varList$samp <- cMIM(mydata=varList$samp,analyticalVariables=analyticalVariables,
                             selectedMissingData=selectedMissingData,metaVariables=metaVariables,
                             classifierClass=classifierClass, removeCata=removeCata)
  }else{
    print("No NA in Sample Dataset")
  }


  factorList <- createFactorLoadingModel(varList$samp, analyticalVariables=analyticalVariables,
                                   selectedNumberOfDimensions=selectedNumberOfDimensions,
                                   selectedDataReductionMethod=selectedDataReductionMethod, removeCata=removeCata,
                                   faRotate=faRotate,faMethodScores=faMethodScores, pcMethod=pcMethod, selectedCorrelationCutoff=selectedCorrelationCutoff)


  varList$samp = data.frame(varList$samp[,metaVariables], varList$samp[, !(names(varList$samp) %in% metaVariables)], factorList$mydata)
  factors <- factorList$analyticalVariables
  classModel <- createClassModel(varList$samp, selectedTrainingSize=selectedTrainingSize, classifierClass=classifierClass, analyticalVariables=factors,
                           cores=varList$cores, createPlots = createPlots, parallelIter='sample',multiThreadFase=F)



  ################################
  # Thread package on all data ##
  ################################


  require("pryr")
  doMC::registerDoMC(varList$cores)
  rm(varList)
  gc(reset=T)
  mallinfo::malloc.trim()
  require(foreach)
  test=foreach::foreach(y=seq(1:length(files))) %dopar% {
    print(paste("in PARA ", (mem_used()/1024)/1024," mb used.", sep=''))
    applyModels(y,files, analyticalVariables, selectedMissingData,
      selectedNormalization, metaVariables, selectedTransformation,
      selectedStandardization, splitCol, classifierClass,
      removeCata, factorList, faMethodScores,selectedAverage,
      removeOutliers,controlVariable,controlValue, classModel,
      skew, kurto,selectedTrainingSize,createPlots,factors)
  }


  return (T)
  #return (varList)
}
