# Function to normalize on the given control.
# The control should be a subset which has been normalized by mean or median.
# Used with a proper sapply function, currentVariable will be a column
# of the bigger dataset and currentVariableValues, the values of this column.
# The data in this column will be normalized by dividing it with the matching
# column in the control dataset.
normalize <- function(currentVariable, currentVariableValues, averageData) {
  subset <- which(names(averageData)==currentVariable)
  normalizedData <- currentVariableValues/averageData[subset]
  return(normalizedData)
}
