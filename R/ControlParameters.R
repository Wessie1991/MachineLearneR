#' @title  Checking parameters
#' @description This function is used inside the \code{\link{ML}} function to check if the given input parameters are in the correct format
#' @param Parameters A vector containing the names of al the parameters used inside the ML function
#' @param bigdatanames A vector containing the names of al the variables in the input dataset
#' @export
ControlParameters <- function(Parameters, bigdatanames){
  if(!file_test("-d", Parameters[1][[1]])) stop("Provided directory doesn't exist")
  noCheck <- c("splitCol", 'metaVariables', 'classifierClass','controlVariable','controlValue','dir')
  lapply(head(noCheck, -1), function(x) if(!is.null(Parameters[x][[1]])) if(
    !Parameters[x][[1]] %in% bigdatanames) stop(paste("Given ", x, " not in the dataset!")))
  if(length(Parameters['classifierClass']) == 0) stop("No classifier class was given")
  for(i in (names(Parameters)[!(names(Parameters) %in% noCheck)])){
    Param <- names(Parameters[i])
    default <- eval(parse(text=paste("default", Param, sep='.')))
    if (!is.null(Parameters[i][[1]]) && !is.null(default)){
      if(Parameters[i][[1]] != default){
        range <- eval(parse(text=paste("range", Param, sep='.')))
        if (length(range) == 2 && is.numeric(range)){
          if (!findInterval(Parameters[i][[1]], range ) == 1){
            stop(paste(Param, ' is not in the correct range it should be in: ', paste(range, collapse='-')))
          }
        }else if(!Parameters[i][[1]] %in% range){
          stop(paste(Param, ' is not in the correct range it should be in: ',  paste(range, collapse=', ')))
        }
      }
    }
  }
}


