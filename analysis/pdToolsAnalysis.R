PlagiarismDetectionToolsAnalysis <- setRefClass("PlagiarismDetectionToolsAnalysis", 
  fields = list(
    ftc="ANY",
    dfm="ANY",
    graphBuilder = "ANY",
    yearAnalizator = "ANY"),
  
  methods = list(
    initialize = function() {
      dfm <<- DataFrameMaipulator$new()
      ftc <<- FrequencyTableCreator$new()
      yearAnalizator <<- YearAnalizator$new()
      graphBuilder <<- GraphBuilderGGplot$new()
      
      graphBuilder$xAxisTitle <<- "Year"
      graphBuilder$yAxisTitle <<- "Number of Articles"
    }, 
                                         
    listArticlesThatUseTool = function(mydata,tool){
      toolData <- mydata[,c("AnalysisID","Year",tool)]
      toolData <- toolData[(toolData[,tool] == "d" 
                           | toolData[,tool]== "c"
                           | toolData[,tool]=="u") & !is.na(toolData[,tool]),]
      return(toolData)
    },
    
    listArticlesThatUseTools = function(mydata,tools){
      dfm$createDataFrameOfPrintColumnValuesForColumns(mydata,tools,"AnalysisID")
    },
    
    numberOfArticlesUsingAToolByType = function(mydata,tool){
      toolUsageList <- listArticlesThatUseTool(mydata,tool)
      ftc$createFrequencyTable(toolUsageList,tool)
    },
    
    
    numberOfArticlesUsingToolsIncludingUsageTypes = function(mydata,tools,types){
      toolsFreq <- ftc$createFrequencyTableForListOfVarsByVarValues(mydata,tools,"ToolWithType")
      toolsFreq <- dfm$replaceAllNaValuesWithValue(toolsFreq,0)
      toolsFreq <- dfm$addTotalColumnAtTheEndForColumns(toolsFreq,types,"Total")
      return(toolsFreq)
    },
    
    numberOfArticlesUsingToolsPerUsageType = function(mydata,tools){
      ftc$createFrequencyTableForListOfVarsByVarValues(mydata,tools,"Usage Type")
    },
    
    numberOfToolsDevelopedPerYear = function(mydata){
      numberOfDevelopedToolsPerYear <- ftc$createFrequencyTableForListOfVarsInComboWithVars(mydata,"ToolDeveloped","Year")
      numberOfDevelopedToolsPerYear <- dfm$addTotalRowAtTheBottomOfDataFrame(numberOfDevelopedToolsPerYear,TRUE)
      return(numberOfDevelopedToolsPerYear)
    },
    
    createToolsDevelopedPerYearBarChart = function(mydata){
      freqTable <- ftc$createFrequencyTableForListOfVarsInComboWithVars(mydata,"ToolDeveloped","Year")
      graph <- .self$graphBuilder$copy()
      graph$graphTitle = "Tools Developed per Year"
      graph$yAxisTitle = "Number of tools"
      graph$showSomekindOfPerYearDottedChart(freqTable,"ToolDeveloped")
    },
    
    getTableOfFreqTopToolsUsagePerYear = function(mydata,tools){
      resultDF <- ftc$createFrequencyTableForListOfVarsInComboWithVars(mydata,tools,"Year")
      dfm$addTotalRowAtTheBottomOfDataFrame(resultDF,TRUE)
    },
    
    getTableMinAndMaxYearUsingATool = function(mydata, tools){
      freqTable <- numberOfArticlesUsingToolsIncludingUsageTypes(mydata,tools,c("c","u","d"))
      freqTable <- dfm$changeColumnNameInDataFrame(freqTable,"ToolWithType","Tools")
      minMax <- yearAnalizator$calculateMinAndMaxYearUsedByVariable(mydata,tools, "Tools")
      result <- dfm$joinListOfDataFrames(list(minMax,freqTable),"Tools")
      result <- dfmHelp$orderDataFrameValuesByColumns(result,c("lastYear","Total"))
      return(result)
    },
    
    numberOfAtriclesDevelopingTool = function(mydata){
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"ToolDeveloped","Not developing a tool")
    },
    
    numberOfArticlesTahatDevelopedOrUsedTool = function(mydata){
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"ToolUsedOrDeveloped","Not developing or using a tool")
    },
    
    numberOfArticlesComparingTools = function(mydata){
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"ToolComparison","Not comparing tools")
    },
    
    numberOfArticlesComparingToolsAndDeveopingNewTool = function(mydata){
      mydata <- mydata[!is.na(mydata$ToolDeveloped),]
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"ToolComparison","Not comparing tools but developing tool")
    },
    
    numberOfArticlesOnlyComparingTools = function(mydata){
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"OnlyToolComparisons","Articles that are not only comparing tools")
    },
    
    numberOfArticlesComparingToolsAndDeveopingNewToolOrUsingTool = function(mydata){
      mydata <- mydata[!is.na(mydata$ToolUsedOrDeveloped),]
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"ToolComparison","Not comparing tools but developing or using tool")
    },
    
    numberOfArticlesComparingToolsAndNotDeveopingNewToolButUsingTools = function(mydata){
      mydata <- mydata[is.na(mydata$ToolDeveloped) & !is.na(mydata[,"ToolUsedOrDeveloped"]),]
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"ToolComparison","Not comparing tools but using and not developing tool")
    },
    
    numberOfArticlesBenchmarkToolDeveloping = function(mydata){
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"BenchmarkToolsTesting","Not benchmarking tools")
    }
))