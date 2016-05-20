correlationCutoffFilter=function(x, corData,
                                 selectedCorrelationCutoff) {

  if(sum(abs(as.numeric(corData[,x]))>=selectedCorrelationCutoff)>1) { # standaard 0.99 deze cuttof

    rowNames <- row.names(corData[(corData[,x]>selectedCorrelationCutoff), ])
    return(rowNames)

  }
}


#
