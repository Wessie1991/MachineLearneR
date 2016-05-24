########################
### Default Variables ##
########################
# This file contains the default values for all input parameters and their ranges

#### Loader + Excluded Variables(garbage)
default.dir= "/home/first-human/bpsys/desmond/"

default.subsetCutoff=10000
range.subsetCutoff=c(100,100000) # Check

default.splitCol=NULL

default.usedCores=NULL
range.usedCores=c(1,100000) # Check

default.metaVariables=NULL

default.sampleSize = 0.05
range.sampleSize=c(0,0.5) # Check

default.selectedGarbageCutoff=0.1
range.selectedGarbageCutoff=c(0,1)

default.selectedCorrelationCutoff = 0.99
range.selectedCorrelationCutoff=c(0,1)

default.removeCata = TRUE # Wanneer deze false is -> dan moet de method voor de correlatie matrix methode op: spearman
range.removeCata = c(TRUE,FALSE)

#### ImputationModel
default.selectedMissingData="impute.mean"
range.selectedMissingData <- c("CWD", "impute.modus","impute.median","impute.mean",
  "onclass.modus", "onclass.median","onclass.mean","regression")

default.classifierClass = NULL


#### ManipulationModel
default.selectedNormalization = "normalize" # unlabeled
range.selectedNormalization=c( "normalize" ,"normalize.splitCol", "normalize.onClass",
                               "normalize.onClass.splitCol", 'NoNormalization')

default.selectedAverage = "mean"
range.selectedAverage = c("mean", "median", "modus")

default.selectedTransformation = TRUE
range.selectedTransformation = c(TRUE,FALSE)

default.selectedStandardization = "traditionalZscore"
range.selectedStandardization = c("traditionalZscore","robustZscore", "NoStandardization")

default.removeOutliers = FALSE
range.removeOutliers = c(FALSE,TRUE)

default.controlVariable = NULL
default.controlValue = NULL

#### Factor loading model
default.selectedDataReductionMethod = "FA"
range.selectedDataReductionMethod = c("FA", "PCA", "NoDataRed" )

default.selectedNumberOfDimensions = 0
range.selectedNumberOfDimensions = c(0,1000) ## Check

default.faRotate = "oblimin"
range.faRotate = c("none", "varimax", "quartimax", "bentlerT",
                   "equamax", "varimin", "geominT" , "bifactor" ,
                   "promax", "oblimin", "simplimax", "bentlerQ",
                   "geominQ" , "biquartimin", "cluster")

default.faMethodScores = 'regression'
range.faMethodScores = c('regression', "Thurstone","tenBerge",
                         "Anderson","Bartlett")

default.pcMethod = 'ml'
range.pcMethod = c("minres", "wls", "gls", "pa", "ml","minchi")

#### Classificatie model
default.selectedTrainingSize = 80
range.selectedTrainingSize = c(1,99)

default.createPlots = FALSE
range.createPlots = c(FALSE,TRUE)

