#' @title  Filtering on correlation
#' @description temp
#' @export
correlationCutoffFilter=function(x, corData,
                                 selectedCorrelationCutoff) {

  if(sum(abs(as.numeric(corData[,x]))>=selectedCorrelationCutoff)>1) {
    rowNames <- row.names(corData[(corData[,x]>selectedCorrelationCutoff), ])
    return(rowNames)

  }
}


