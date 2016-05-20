#' @title Multiple Imputation
#' @description Inside the ML function this function is responsible for imputing the missing values in the dataset. This function can be used seperately to imputa NA values for any given dataframe.
#' @param mydata a dataframe containing the data to be analysed
#' @param analyticalVariables A vector containing the names of the variables in the dataset that should be analysed
#' @param selectedMissingData A string specifying the method for imputing missing data
#' @param metaVariables A vector containing the names of the variables which should not be analysed, but should still remain in the dataset as describing variables
#' @param classifierClass A string specifying the name of the variable on which the dataset should be classified
#' @param removeCata A boolean indicating if categorical variables should be removed from the dataset
#' @examples
#' require("data.table")
#' data <- fread("data.csv")
#' data <- data.frame(data)
#' cMIM(mydata=data, analyticalVariables=c("var1", "var2"), selectedMissingData='onclass.mean', metaVariables=c("varX", "varY"), classifierClass='var1', removeCata=F)
#' @export
cMIM <- function(mydata, analyticalVariables,
                 selectedMissingData, metaVariables, classifierClass, removeCata){
  numberOfRecordsBefore=nrow(mydata[,analyticalVariables])
  numberOfColumnsBefore=ncol(mydata[,analyticalVariables])

      if (removeCata == FALSE){
        listRm <- sapply(mydata[,analyticalVariables], removeCategorical)
        CataVariables <- names(listRm[!sapply(listRm, is.null)])
        analyticalVariables <- analyticalVariables [! analyticalVariables %in%  CataVariables ]

        if (length(CataVariables > 0)){
          if(!is.null(classifierClass) & substring(selectedMissingData, 1, 1) == 'o'){
            print('Fill categorical by modus of similar cases')
            mydata[,CataVariables] <- onclass(mydata[,CataVariables], classifierClass, "modus")

          }else{
            print("Fill categorical by modus")
            print(length(CataVariables))
            mydata[,CataVariables]= apply(mydata[,CataVariables],2, impute.modus)
          }
        }
      }




      # Check if impute function is called:
      if (substring(selectedMissingData, 1, 1) == 'i'){
        print(paste("Impute missings by", strsplit(selectedMissingData,"[.]")[[1]][2],sep=' '))
        mydata[,analyticalVariables]=apply(mydata[,analyticalVariables], 2, eval(parse(text=selectedMissingData)))
      } # Check for data deletion.
      else if (selectedMissingData == "CWD"){
        print("Missing data is removed (row wise)")
        mydata=subset(mydata, complete.cases(mydata[,analyticalVariables])) # Omit missings (required for factor analysis)
#         analyticalVariables=colnames(subset(mydata, select = !names(mydata) %in% metaVariables))
#
#         numberOfRecordsAfter=nrow(mydata[,analyticalVariables])
#         numberOfColumnsAfter=ncol(mydata[,analyticalVariables])
#         print(paste("Processing missing data, ", numberOfRecordsBefore-numberOfRecordsAfter, " records removed (", round(100-(100*(numberOfRecordsAfter/numberOfRecordsBefore)), 2), "%)", sep=""))
#         print(paste("Processing missing data, ", numberOfColumnsBefore-numberOfColumnsAfter, " variables removed (", round(100-(100*(numberOfColumnsAfter/numberOfColumnsBefore)), 2), "%)", sep=""))
      }# only other method is regresion
      else if (selectedMissingData == "regression"){
        # Desmond: deze require moet weg
        require(mice)
        # desmond: als het goed is moet die nu naar behoren werken.
        print("Impute missings by regression")
        ini=mice::mice(mydata[,analyticalVariables], maxit=0, pri=F)
        # Desmond: omdat tijdens de complete function er altijd wordt uit gehaald
        mydataMeta <- mydata[,metaVariables]
        pred=ini$pred
        meth=ini$meth
        imp=mice::mice(mydata[,analyticalVariables], m=1, maxit=10, printFlag=TRUE, pred=pred, meth=meth, seed=1234)
        mydata <- complete(imp, action = "long", include = FALSE)[,analyticalVariables]
        mydata <- cbind(mydata, mydataMeta)
      }else if(!is.null(classifierClass) & substring(selectedMissingData, 1, 1) == 'o'){
        print(paste("Impute missings of similar cases by ", strsplit(selectedMissingData,"[.]")[[1]][2],sep=' '))
        argOnclass <- strsplit(selectedMissingData,"[.]")[[1]][2]
        mydata[,analyticalVariables] = onclass(mydata[,analyticalVariables], classifierClass, argOnclass)
      }


      ### colSums(mydata[,analyticalVariables], na.rm = T)


      return (mydata)

}
