#' @title  Using the random forests classifier
#' @description  Creates a Test and a Training data set from the mydata input dataframe, and trains and applies a random forests classifier to predict the data.
#' @param mydata A dataframe
#' @param selectedTrainingSize A number between 1-100, describing the size of the training set as a percentage of the total dataset. Default is 80.
#' @param classifierClass A string, containing the class to classify
#' @param analyticalVariables A character vector, containing the names of the variables in the dataset to analyze.
#' @param cores A number, describing the number of cores to use in parallel processing.
#' @return file A filename, the file contains a plot of the different variables.
#' @examples
#' require("data.table")
#' data <- fread("data.csv")
#' data <- data.frame(data)
#' classModel(mydata=data, selectedTrainingSize = 90,classifierClass="ExampleClass", analyticalVariables = names(mydata), cores=6)
#' classModel(mydata=data, classifierClass="ExampleClass", analyticalVariables = names(mydata), cores=8)
#' @export


createClassModel <- function(mydata, selectedTrainingSize, classifierClass, analyticalVariables , cores, createPlots,
                             multiThreadFase, fit, parallelIter)
{



  selectedTrainingIterations <-  if( ceiling(length(analyticalVariables) / 3) > 128) 128 else  ceiling(length(analyticalVariables) / 3)
  print("Creating training set")

  #TrainData = droplevels(mydata[mydata$wellname!="SAMPLE",]) # Verwijderd alle welnames met Sample erin
  TrainData <- mydata
  trainingSize=nrow(TrainData)   # number of records
  sampling=sample.int(trainingSize, round(selectedTrainingSize/100*trainingSize))
  TestData=TrainData[-sampling,]
  TrainData=TrainData[sampling,]

  TestData=droplevels(TestData[complete.cases(TestData),])# remove missing records NA, NaN
  TrainData=droplevels(TrainData[complete.cases(TrainData),])# remove missing records NA, NaN


  classModelMetadata <- list(
  TotalSize=nrow(mydata),
  TrainingSize=nrow(TrainData),
  TestSize=nrow(TestData),
  uniqueClasses=unique(TrainData[classifierClass]))

  rm(mydata)
  gc()
  if(!multiThreadFase){

    selectedIterations=ceiling(selectedTrainingIterations/cores)
    print("Training Random Forest model")

    bestmtry=try(randomForest::tuneRF(TrainData[,analyticalVariables],as.factor(TrainData[,classifierClass]),
                                  ntreeTry=100, stepFactor=1.5, trace=TRUE, plot=FALSE, dobest=FALSE))
    if (class(bestmtry)=="try-error") {
      mtry=3
    } else {
      mtry=as.numeric(bestmtry[1,1])
    }
    doMC::registerDoMC(cores)
    require(foreach)

    fit=foreach::foreach(y=seq(cores), .combine=randomForest::combine, .packages='randomForest') %dopar% {
          set.seed(y) # not really needed
          rf=randomForest::randomForest(TrainData[,analyticalVariables],as.factor(TrainData[,classifierClass]),
                                        ntree=selectedIterations, mtry=mtry, keep.forest=TRUE, importance=TRUE)
    }
  }

  print("Testing accuracy Random Forest model")
  TestData$prediction=predict(fit, TestData)
  TestData$probability=predict(fit, TestData, type='prob')
  print(paste("Percentage Correct Predictions TestData (", round(length(which(TestData[,classifierClass] == TestData$prediction)
                                  ) / nrow(TestData) * 100 , 3), "%)", sep=""))



  classificationAccuracy=round(100*(nrow(TestData[which(TestData[,classifierClass]==TestData[,"prediction"]),])/nrow(TestData)), 2)
  print(paste("Accuracy Test set: ", classificationAccuracy, " %"))

  distinctLabels=unique(TrainData[,classifierClass])

  maxClass=data.frame(ncol=2, nrow=length(distinctLabels))
  colnames(maxClass)=c("classIndex", "nrow")
  for (i in 1:length(distinctLabels)) {
    maxClass[i,"classIndex"]=as.numeric(i)
    maxClass[i,"nrow"]=as.numeric(nrow(subset(TestData, TestData[,classifierClass]==distinctLabels[i])))
  }
  baseline=round(100*(max(maxClass$nrow, na.rm=TRUE)/nrow(TestData)), 2)

  print(paste("Baseline: ", round(baseline, 2), "% (", distinctLabels[subset(maxClass, maxClass$nrow==max(maxClass$nrow, na.rm=TRUE))$classIndex], ")", sep=""))
  print(paste("The model adds ", round(classificationAccuracy-baseline, 3), "% of predictive power", sep=""))

  if(createPlots){
    variableImportancePlot(fit, classifierClass, analyticalVariables, parallelIter)
  }
  return(fit)
}

