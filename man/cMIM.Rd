% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/createMultipleImpMod.R
\name{cMIM}
\alias{cMIM}
\title{Multiple Imputation}
\usage{
cMIM(mydata, analyticalVariables, selectedMissingData, metaVariables,
  classifierClass, removeCata)
}
\arguments{
\item{mydata}{a dataframe containing the data to be analysed}

\item{analyticalVariables}{A vector containing the names of the variables in the dataset that should be analysed}

\item{selectedMissingData}{A string specifying the method for imputing missing data}

\item{metaVariables}{A vector containing the names of the variables which should not be analysed, but should still remain in the dataset as describing variables}

\item{classifierClass}{A string specifying the name of the variable on which the dataset should be classified}

\item{removeCata}{A boolean indicating if categorical variables should be removed from the dataset}
}
\description{
Inside the \code{\link{ML}} function this function is responsible for imputing the missing values in the dataset. This function can be used separately to imputa NA values for any given dataframe.
}
\examples{
require("data.table")
data <- fread("data.csv")
data <- data.frame(data)
cMIM(mydata=data, analyticalVariables=c("var1", "var2"), selectedMissingData='onclass.mean', metaVariables=c("varX", "varY"), classifierClass='var1', removeCata=F)
}

