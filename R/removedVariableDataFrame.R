removedVariableDataFrame <- function(x, listdata){
  Function <- tail(listdata[x][[1]], n=1)
  var <- (listdata[x][[1]])
  switch (Function,
          removeNonNumeric = {
            varRound2 <- round(as.numeric(var[2]), digits = 3)
            varRound3 <- round(as.numeric(var[3]), digits = 3)
            df <- c(Function, var[1], varRound2, varRound3,NA ,NA, NA,NA)

            },
          removeMissing = {
            varRound1<- round(as.numeric(var[1]), digits = 3)
            varRound3 <- round(as.numeric(var[3]), digits = 3)
            df <- c(Function,NA ,varRound1, var[2], varRound3,NA, NA, NA)

            },
          removeCategorical = {
            varRound2<- round(as.numeric(var[2]), digits = 3)
            varRound3 <- round(as.numeric(var[3]), digits = 3)
            df <- c(Function, NA,varRound2, varRound3,NA,var[1], NA, NA)

            },

          uniFormDistribution = {
            varRound2<- round(as.numeric(var[2]), digits = 3)
            varRound3 <- round(as.numeric(var[3]), digits = 3)
            varRound1<- round(as.numeric(var[1]), digits = 3)
            varRound4<- round(as.numeric(var[4]), digits = 3)
            df <- c(Function,NA,varRound2, varRound3,varRound4,NA ,varRound1 ,NA)

          }, correlationCutoffFilter = {
            varRound2<- round(as.numeric(var[2]), digits = 3)
            varRound3 <- round(as.numeric(var[3]), digits = 3)
            varRound4 <- round(as.numeric(var[4]), digits = 3)
            df <- c(Function, NA, varRound2, varRound3,varRound4, NA, NA,var[1])

          }

      )
  return(cbind(t(df)))
}

