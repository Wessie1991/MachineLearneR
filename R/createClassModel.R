#' @title  Using the random forests classifier
#' @description  Inside the \code{\link{ML}} function this function is used to perform a random forests classification on a given data frame. It creates a Test and a Training data set from the mydata input dataframe, and trains and applies a random forests classifier to predict the data.
#' @param mydata A dataframe
#' @param selectedTrainingSize A number between 1-100, describing the size of the training set as a percentage of the total dataset. Default is 80
#' @param classifierClass A string, containing the class to classify
#' @param analyticalVariables A character vector, containing the names of the variables in the dataset to analyze.
#' @param cores A number, describing the number of cores to use in parallel processing.
#' @examples
#' require("data.table")
#' data <- fread("data.csv")
#' data <- data.frame(data)
#' classModel(mydata=data, selectedTrainingSize = 90,classifierClass="ExampleClass", analyticalVariables = names(mydata), cores=6)
#' classModel(mydata=data, classifierClass="ExampleClass", analyticalVariables = names(mydata), cores=8)
#' @importFrom foreach %dopar%
#' @import randomForest 
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

  rm(mydata);gc(reset=T);mallinfo::malloc.trim(pad=pryr::mem_used())
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
  TestData$prediction=randomForest:::predict.randomForest(fit, TestData)
  TestData$probability=randomForest:::predict.randomForest(fit, TestData, type='prob')
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
  rm(TestData)
  rm(TrainData);gc(reset=T);mallinfo::malloc.trim(pad=pryr::mem_used())
  return(fit)
}

#' @title Plotting Variable importance
#' @description Inside the \code{\link{createClassModel}} function this function is responsible for
#' @export
variableImportancePlot <- function(fit, classifierClass, analyticalVariables, parallelIter){
  variableImportance=data.frame(fit$importance)
  variableImportance$variables=row.names(variableImportance)
  variableImportance=variableImportance[order(variableImportance$MeanDecreaseGini,decreasing=FALSE),]
  variableImportance=cbind(1:nrow(variableImportance), variableImportance)
  variableImportance=data.frame(variableImportance)
  colnames(variableImportance)[1]="id"


  file=paste( "ML_variableImportance_", classifierClass,"_subset_",parallelIter, ".png", sep="")
  varImpPlot=ggplot2::ggplot(variableImportance, ggplot2::aes(as.numeric(id), as.numeric(MeanDecreaseGini), label=analyticalVariables))
  varImpPlot=varImpPlot+ggplot2::geom_point(size = 6, shape=20, fill="black", colour="black")
  varImpPlot=varImpPlot+ggplot2::geom_line(size = 0.5, colour="black", linetype=1)
  varImpPlot=varImpPlot+ggplot2::geom_text(size=3, vjust=-1.5)
  varImpPlot=varImpPlot+ggplot2::scale_colour_hue(l=50)
  varImpPlot=varImpPlot+ggplot2::coord_cartesian()
  varImpPlot=varImpPlot+ggplot2::ggtitle("Variable importance")
  varImpPlot=varImpPlot+ggplot2::labs(x = "")
  varImpPlot=varImpPlot+ggplot2::xlab("Components/variables")
  varImpPlot=varImpPlot+ggplot2::ylab("Importance")
  varImpPlot=varImpPlot+ggplot2::theme_bw(base_size = 12, base_family = "")
  varImpPlot=varImpPlot+ggplot2::theme(legend.position="top", legend.background = ggplot2::element_rect(colour = "black"),
                                       plot.title = ggplot2::element_text(size = ggplot2::rel(1.5), colour = "black"),
                                       axis.text.y  = ggplot2::element_text(size=ggplot2::rel(1.5)),
                                       axis.text.x  = ggplot2::element_text(angle=90, vjust=0.5,
                                                                            size=ggplot2::rel(1.5)), panel.background = ggplot2::element_rect(fill = "white"),
                                       panel.grid.major = ggplot2::element_line(colour = "grey40"), panel.grid.minor = ggplot2::element_blank())
  ggplot2::ggsave(varImpPlot, file=file, width=15, height=7)
  return(file)

}



