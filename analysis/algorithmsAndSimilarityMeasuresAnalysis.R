AlgorithmsAndSimilarityMeasuresAnalysis <- setRefClass("AlgorithmsAndSimilarityMeasuresAnalysis", 
                                      
  fields = list(
    ftc="ANY",
   dfm="ANY",
   graphBuilder = "ANY",
   yearAnalizator = "ANY"
  ),
 
 methods = list(
   initialize = function() {
     dfm <<- DataFrameMaipulator$new()
     ftc <<- FrequencyTableCreator$new()
     yearAnalizator <<- YearAnalizator$new()
     graphBuilder <<- GraphBuilderGGplot$new()
     
     graphBuilder$xAxisTitle <<- "Dataset Type"
     graphBuilder$yAxisTitle <<- "Number of Articles"
   },
                                        
    numberOfArticlesUseOrDescribeAlgorithm = function(mydata) {
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"AlgorithmsDescribedOrUsed","Articles that do not use and describe an algorithm")
    },
    
    numberOfArticlesUsingWhatSimilarityMeasure = function(mydata,similarityMeasureNames) {
      ftc$createFrequencyTableForListOfVarsByVarValues(mydata,similarityMeasureNames,"Similarity Measure")
    },
    
    numberOfArticlesUsingWhatKindOfAlgorithmAndOtherTypeOfAlgorithm = function(mydata,algorithmClasification)  {
      dataFrameKindsOfAlgorithms <- numberOfArticlesUsingWhatKindOfAlgorithm(mydata,algorithmClasification)
      
      mydata <- mydata[!is.na(mydata$UsedOtherAlgorithm),]
      dataFrameKindsOfAlgorithmsAndOther <- numberOfArticlesUsingWhatKindOfAlgorithm(mydata,algorithmClasification)
      dataFrameKindsOfAlgorithmsAndOther <- dfm$changeColumnNameInDataFrame(dataFrameKindsOfAlgorithmsAndOther, "All", "All Num of Articles using also other category algorihtm")
      dataFrameKindsOfAlgorithmsAndOther <- dfm$changeColumnNameInDataFrame(dataFrameKindsOfAlgorithmsAndOther, "Tokenized", "Tokenized Num of Articles using also other category algorihtm")
      
      dfm$joinListOfDataFrames(list(dataFrameKindsOfAlgorithms,dataFrameKindsOfAlgorithmsAndOther),"Algorithm_or_Algorithm_Type")
    },
    
    numberOfArticlesUsingWhatKindOfAlgorithm = function(mydata,algorithmClasification) {
      resultAll <- ftc$createFrequencyTableForListOfVarsByVarValues(mydata,algorithmClasification,"Algorithm_or_Algorithm_Type")
      resultAll <- dfm$changeColumnNameInDataFrame(resultAll,"X1","All")
      
      mydataTok <- mydata[!is.na(mydata$Tokenized),]
      resultTokenized <- ftc$createFrequencyTableForListOfVarsByVarValues(mydataTok,algorithmClasification,"Algorithm_or_Algorithm_Type")
      resultTokenized <- dfm$changeColumnNameInDataFrame(resultTokenized,"X1","Tokenized")
      
      resultJoined <- dfm$joinListOfDataFrames(list(resultAll,resultTokenized),"Algorithm_or_Algorithm_Type")
      resultJoined <- dfm$replaceAllNaValuesWithValue(resultJoined,0)
      resultJoined <- dfm$orderDataFrameValuesByColumns(resultJoined,"All",TRUE)
      
      return(resultJoined)
    },
   
   getTableMinAndMaxYearUsingAlgorithm = function(mydata, algorithmClasification){
     freqTable <- numberOfArticlesUsingWhatKindOfAlgorithm(mydata,algorithmClasification)
     minMax <- yearAnalizator$calculateMinAndMaxYearUsedByVariable(mydata,algorithmClasification, "Algorithm_or_Algorithm_Type")
     result <- dfm$joinListOfDataFrames(list(minMax,freqTable),"Algorithm_or_Algorithm_Type")
     result <- dfmHelp$orderDataFrameValuesByColumns(result,c("All"))
     return(result)
   },
    
    numberOfArticlesThatUseTokenizationInAlgorithm = function(mydata) {
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"Tokenized","Articles that do not use tokenization or it is not mentioned in article")
    },
    
    numberOfArticlesThatUseOtherAlgorithmsAndTypesOfAlgorithms = function(mydata) {
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"UsedOtherAlgorithm","Articles do not use any algorithm or use standard ones")
    },
   
   listArticlesThatUseAlgorithms = function(mydata,algorithms){
     dfm$createDataFrameOfPrintColumnValuesForColumns(mydata,algorithms,"AnalysisID")
   }
  )
)