#' @title  Reducing the number of variables
#' @description @description Inside the \code{\link{ML}} function this function is responsible for reducing the number of variables(columns) in the dataset by removing irrelevant variables. This function can be used separately to reduce the ammount of variables in any given data frame.
#' @param mydata a dataframe containing the data to be analysed
#' @param analyticalVariables A vector containing the names of the variables in the dataset that should be analysed
#' @param selectedGarbageCutoff A number in range 0-1 specifying the maximum ammount of missing values a variable is allowed to contain
#' @param selectedCorrelationCutoff A number in range 0-1 specifying the maximum ammount of correlation between variables
#' @param removeCata A boolean indicating if categorical variables should be removed from the dataset
#' @examples
#' require("data.table")
#' data <- fread("data.csv")
#' data <- data.frame(data)
#' ExcludedVariablesModel(mydata=data, analyticalVariables=c("var1", "var2"), selectedGarbageCutoff=0.2, selectedCorrelationCutoff=0.90, removeCata=T)
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

#' @title  Removing Catagorical
#' @description Inside the \code{\link{ExcludedVariablesModel}} function this function is responsible for
#' @param x A vector
#' @export
removeCategorical <- function(x) {
  if (length(unique(x))<5) {
    return(c(length(unique(x)), (sum(is.na(x))/length(x)*100), sum(is.na(x)), "removeCategorical"))
  }
}

#' @title  Removing missing
#' @description Inside the \code{\link{ExcludedVariablesModel}} function this function is responsible for
#' @param x A vector
#' @export
removeMissing <- function(x, selectedGarbageCutoff){
  stdev <- sd(x, na.rm=TRUE)
  fractionMissing <- sum(is.na(x))/length(x)
  if(is.na(stdev)) {
    stdev <- 0
  }
  if (fractionMissing>=selectedGarbageCutoff ) {
    return(c(fractionMissing, (fractionMissing*length(x)*100), stdev, "removeMissing"))
  }else{
    if(stdev==0 | is.na(stdev)){
      return(c(fractionMissing, (fractionMissing*length(x)*100), stdev, "removeMissing"))
    }
  }
}

#' @title  Removing Non-numeric
#' @description Inside the \code{\link{ExcludedVariablesModel}} function this function is responsible for
#' @param x A vector
#' @export
removeNonNumeric=function(x) {
  class=class(x)
  if (class!="numeric" & class!="integer" ) {
    return(c(class, (sum(is.na(x))/length(x)*100), sum(is.na(x)) ,"removeNonNumeric"))
  }
}

#' @title  Uniform distribution
#' @description Inside the \code{\link{ExcludedVariablesModel}} function this function is responsible for
#' @param x A vector
#' @export
uniformdistribution <- function(x){

  average=mean(x, na.rm = T)
  stdev=sd(x, na.rm = T)
  minv=min(x, na.rm = T)
  maxv=max(x, na.rm = T )

  # probability normal
  pnorm<-try(dnorm(x, average, stdev))
  pnorm<-try(sum(log(pnorm), na.rm = T))
  # probability uniform
  punif<-try(dunif(x, minv, maxv))
  punif<-try(sum(log(punif), na.rm = T))
  uniFormDistribution=try(punif-pnorm)
  if (uniFormDistribution > 0){
    return(c(uniFormDistribution,(sum(is.na(x))/length(x)*100), sum(is.na(x)), stdev, "uniFormDistribution"))
  }
}







