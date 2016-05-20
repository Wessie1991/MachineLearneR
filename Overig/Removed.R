# bigdata = De main data frame -> alleen in de main functie heet deze zo

# mydata = Subsets van Bigdata, in de functies zoals bij excluded vars, wordt input data zo genoemd

# xSet = een gesplitted subset van bigdata, op basis van de splicol gesplit, wordt alleen gebruikt in de apply models.

# df = Tijdelijke data frame

# x = Tijdelijke vector

# dir = Working directory

# subsetCutoff = Minimale hoeveelheid records in een subset.

# splitCol = De kolom waarop wordt gesplit bij het creeren van de subsets.

# usedCores = Het aantal cores wat de gebruiker maximaal wenst te gebruiken tijdens een run.

# sampleSize = De grote van de sample 0.05 is 5%

# selectedGarbageCutoff = Percentage missing values wat is toegestaan.
#                         Bij bijvoorbeeld 10 is wanneer een kolom meer dan 10% missing values heeft wordt die kolom verwijderd.

# selectedCorrelationCutoff = Maximale toegestane correlatie tussen variablen

# removeCata = TRUE = remove catagorical FALSE = Dont remove catagorical

# selectedMissingData = De methode die wordt gebruikt voor het imputen van de data

# impClass = Als de selectedMissingData onclass(o.b.v. een classe) is, dan moet de class worden meegegeven. (impute class)

# metaVariables = De variable die niet moeten worden genalyseerd, maar wel gebruik moeten worden om de record te identificeren!
#                 Beschrijvende kollomen zoals Barcode e.d.

# returnList = Naam van de list wanneer meerdere variabele moeten worden geretuned


# varList = De lijst met variablen die in de main functie worden gebruikt.
#           Vanuit de GetInput functie krijgt deze list:
#               $ samp  = sample data set van n%
#               $ files = De namen van de Rdata files die zijn aangemaakt door de split functies
#               $ metaVariables = de variabelen die niet worden geanalyseerd,
#               $ analyticalVariables = De variabelen die wel worden geanalyseerd.
#               $ cores = Het aantal cores dat is vastgesteld door de splitinput functie
#               $ garbagedf = De data frame met daarin de variabelen die door het garbage model zijn verwijderd + de reden van verwijderen.

# listRm = lijst die vanuit sapply wordt gereturned

# listdata = listRm zonder de NA's

# corData = Correlatiematrix van de analyticalVariables

# fractionMissing = het aantal na's/ totaal aantal rows in een vector/kolom.

# selectedNormalization =

# selectedTransformation =

# selectedStandardization =

# skew = list van de skewness per analytic variable

# kurto = list van de kurtosis per analytic variable




## Alternatieven van parrallel processing


# require(snowfall)
  # sfInit(parallel=T, cpus=varList$cores)
  #
  # # require("pryr")
  # for(i in c(files, analyticalVariables, selectedMissingData,
  #             selectedNormalization, metaVariables, selectedTransformation,
  #             selectedStandardization, splitCol, classifierClass,
  #             removeCata, factorList, faMethodScores,selectedAverage,
  #             removeOutliers,controlVariable,controlValue, classModel,
  #             skew, kurto,selectedTrainingSize,createPlots,factors)){
  #   print(paste(i), (object.size(i)/1024)/1024, "mb used.", sep=' '))
  # }
  #
  # snowfall::sfExport("files", "analyticalVariables", "selectedMissingData",
  #                   "selectedNormalization", "metaVariables", "selectedTransformation",
  #                   "selectedStandardization", "splitCol", "classifierClass",
  #                   "removeCata", "factorList", "faMethodScores", "selectedAverage",
  #                   "removeOutliers","controlVariable","controlValue", "classModel",
  #                   "skew", "kurto","selectedTrainingSize","createPlots","factors")
  # plyr::l_ply(seq(1:length(files)),.fun=applyModels, files, analyticalVariables, selectedMissingData,
  #             selectedNormalization, metaVariables, selectedTransformation,
  #             selectedStandardization, splitCol, classifierClass,
  #             removeCata, factorList, faMethodScores,selectedAverage,
  #             removeOutliers,controlVariable,controlValue, classModel,
  #             skew, kurto,selectedTrainingSize,createPlots,factors, .parallel = T)
  #
  # ## haal dit weg
  # # import library
  # snowfall::sfLibrary(zoo)
  # snowfall::sfLibrary(mice)
  # snowfall::sfLibrary(ML)
  # snowfall::sfLibrary(psych)
  # snowfall::sfLibrary(robustfa)
  # snowfall::sfLibrary(rrcov)
  # snowfall::sfLibrary(randomForest)
  #
  # # apply supervised learning model to dataset
  # snowfall::sfLapply(1:length(files), applyModels)
  # sfStop()



