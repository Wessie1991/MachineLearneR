
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

