#' @title  Main function for running the MachinLearneR Process
#' @description #' \code{ML} executes the entire MachineLearneR (See manual) process on any given dataset, which can be provided as data frame or as flatfile, a classifierClass should also be provided, all other parameters are optional.
#' @param bigdata A dataframe or flatfile containing a dataset.
#' @param dir A location specifying the working directory. Default is \code{\link{getwd}}
#' @param subsetCutoff A number, specifying the minimal number of records that must be in a subset. Default is 10000
#' @param splitCol A string, specifying the column on which the data set should be splitted
#' @param usedCores A number, specifing the number of cores to be used in parallel processing
#' @param sampleSize A number, in range 0-1, specifying the fraction of the mydata dataframe to be used as sample data set. Default is 0.05 (5\%)
#' @param selectedGarbageCutoff A number, in range 0-1, specifying the fraction of records in a variable(column) that is accepted to be NA. Default is 0.10(10\%)
#' @param selectedCorrelationCutoff A number, in range 0-1, specifying the fraction of the maximum accepted ammount of correlation between two variables
#' @param selectedMissingData A String, specifying the method to be used when calling the imputation model function; \code{\link{cMIM}}. Default is impute.mean
#' @param classifierClass A string specifying the name of the variable on which the dataset should be classified
#' @param removeCata A boolean, enabling/disabling the removal of catagorical variables(columns) from the mydata data frame. Default is TRUE, catagorical variables are removed.
#' @param metaVariables A vector containing the names of the variables which should not be analysed, but should still remain in the dataset as describing variables
#' @param selectedTransformation A string specifying the method for transforming the data
#' @param selectedStandardization A string specifying the method for standardizing the data
#' @param controlVariable A string specifying the variable on which the data should be normalized
#' @param controlValue A string specifying the value of the controlVariable on which the data should be normalized
#' @param removeOutliers A boolean indicating if outliers should be removed in data normalization
#' @param selectedDataReductionMethod A string, specifying the method of data reduction to be applied, can be PCA or factor analysis
#' @param selectedNumberOfDimensions the number of dimensions to be created, the number of produced factors. If 0, the number of dimensions is calculated by using the eigenvalues
#' @param faRotate A string specifying the method to be used in rotation, default: oblimin
#' @param faMethodScores A string describing the method used for calculating the factors
#' @param pcMethod A string specifying the factoring method the be used
#' @param selectedTrainingSize A number between 1-100, describing the size of the training set as a percentage of the total dataset. Default is 80
#' @examples
#' require('MachineLearneR')
#' log <- ML('/home/testData.csv',dir='/home/output/', classifierClass = 'wellname', usedCores=6)
#' @export
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
    rm(bigdata); gc(reset=T);mallinfo::malloc.trim(pad=pryr::mem_used())
  }else if (file.exists(bigdata)){
    mydata <- data.table::fread(bigdata)
    mydata <- data.frame(mydata)
    rm(bigdata); gc(reset=T);mallinfo::malloc.trim(pad=pryr::mem_used())
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


  rm(mydata); gc(reset=T);mallinfo::malloc.trim(pad=pryr::mem_used())

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

  rm(temp); gc(reset=T);mallinfo::malloc.trim(pad=pryr::mem_used())


  ########################################
  # Apply Multiple Imputation on Sample ##
  ########################################
  if(any(is.na(varList$samp))){
       varList$samp <- cMIM(mydata=varList$samp,analyticalVariables=analyticalVariables,
                             selectedMissingData=selectedMissingData,metaVariables=metaVariables,
                             classifierClass=classifierClass, removeCata=removeCata)
  }else{
    print("No NA's in Sample Dataset")
  }


  factorList <- createFactorLoadingModel(varList$samp, analyticalVariables=analyticalVariables,
                                   selectedNumberOfDimensions=selectedNumberOfDimensions,
                                   selectedDataReductionMethod=selectedDataReductionMethod, removeCata=removeCata,
                                   faRotate=faRotate,faMethodScores=faMethodScores, pcMethod=pcMethod, selectedCorrelationCutoff=selectedCorrelationCutoff)


  gc(reset=T);mallinfo::malloc.trim(pad=pryr::mem_used())
  varList$samp = data.frame(varList$samp[,metaVariables], varList$samp[, !(names(varList$samp) %in% metaVariables)], factorList$mydata)
  factors <- factorList$analyticalVariables
  classModel <- createClassModel(varList$samp, selectedTrainingSize=selectedTrainingSize, classifierClass=classifierClass, analyticalVariables=factors,
                           cores=varList$cores, createPlots = createPlots, parallelIter='sample',multiThreadFase=F)



  ################################
  # Thread package on all data ##
  ################################

  #doMC::registerDoMC(varList$cores)
  cl <- snow::makeCluster(varList$cores, outfile="parralleloutput.txt")
  doSNOW::registerDoSNOW(cl)
  rm(varList); gc(reset=T);mallinfo::malloc.trim(pad=pryr::mem_used())
  require(foreach)
  parallelResult=foreach::foreach(y=seq(1:length(files))) %dopar% {
    print(paste("in PARA ", (pryr::mem_used()/1024)/1024," mb used.", sep=''))
    applyModels(y,files, analyticalVariables, selectedMissingData,
      selectedNormalization, metaVariables, selectedTransformation,
      selectedStandardization, splitCol, classifierClass,
      removeCata, factorList, faMethodScores,selectedAverage,
      removeOutliers,controlVariable,controlValue, classModel,
      skew, kurto,selectedTrainingSize,createPlots,factors)
  }
  snow::stopCluster(cl)


  return (parallelResult)
  #return (varList)
}
