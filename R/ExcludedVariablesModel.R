#' @title  Reducing the number of variables
#' @description blabalba
#' @param mydata a dataframe containing the data to be analysed
#' @param analyticalVariables A vector containing the names of the variables in the dataset that should be analysed
#'
#' @export

ExcludedVariablesModel <- function(mydata, analyticalVariables,
                         selectedGarbageCutoff,
                         selectedCorrelationCutoff, removeCata
){

  #verwijder alle niet numerieke variablen.
  listRm <- sapply(mydata[,analyticalVariables], removeNonNumeric)
  listdata <- listRm[!sapply(listRm, is.null)]
  analyticalVariables <- analyticalVariables [! analyticalVariables %in% names(listdata)]
  # verwijder alle variabelen die teveel missing values hebben.
  listRm <- sapply(mydata[,analyticalVariables], removeMissing, selectedGarbageCutoff )
  listdata <- append(listdata, listRm[!sapply(listRm, is.null)])
  analyticalVariables <- analyticalVariables [! analyticalVariables %in% names(listdata)]
  if(removeCata == TRUE){
  # verwijder alle Categorical
  listRm <- sapply(mydata[,analyticalVariables], removeCategorical)
  listdata <- append(listdata, listRm[!sapply(listRm, is.null)])
  analyticalVariables <- analyticalVariables [! analyticalVariables %in% names(listdata)]
  }
  # verwijder alle uniformedistributie variablen
  listRm <- sapply(mydata[,analyticalVariables], uniformdistribution)
  listdata <- append(listdata, listRm[!sapply(listRm, is.null)])
  analyticalVariables <- analyticalVariables [! analyticalVariables %in% names(listdata)]



  # _______vanaf hier is het aangepast__________ : Desmond
  # eliminateHighestMissingVariableInCorrelationCutoff is een  nieuwe functie
  # er wordt een corelatie matrix  gereturn
  # en  de variablen met een hoge corelatie waar dan ook de meeste missing value in zitten.
  updateVariables <- eliminateHighestMissingVariableInCorrelationCutoff(mydata, analyticalVariables,
                                                                        selectedCorrelationCutoff, removeCata)
  corData <- updateVariables$corData
  updateVariables <- updateVariables$updateVariables
  analyticalVariables <- analyticalVariables [! analyticalVariables %in% updateVariables]
  ###update listdata #####
  # de updateCorrelationVariable is een functie die een dataframe maakt.
  tempDf <- t(sapply(unique(updateVariables), updateCorrelationVariable, corData, mydata, nrow(mydata)))


  df <- sapply(names(listdata), removedVariableDataFrame, listdata)
  if(ncol(tempDf) == ncol(t(df))){
    garbageDf <- rbind(t(df), tempDf)
  }
  else{
    garbageDf <- t(df)
  }
  colnames(garbageDf) <- c("RemoveByFunction", "DataType", "Percentage",
                                "NumberOfMissingValue", "StandardDeviation","NumberOfCategoricalValue",
                                "SumPunfPnorm", "correlation")

  returnList <- list(garbageDf=garbageDf, analyticalVariables=analyticalVariables)



  return(returnList)

}


