ArticleDatasetAnalysis <- setRefClass("ArticleDatasetAnalysis", 
                                                
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
      
      graphBuilder$xAxisTitle <<- "Dataset Type"
      graphBuilder$yAxisTitle <<- "Number of Articles"
    }, 
    
    numberOfArticlesUsingDatasetType = function(mydata) {
      ftc$createFrequencyTable(mydata,"DatasetType")
    },
    
    numberOfArticlesUsingDatasetTypeGrouped = function(mydata, usedDatasetTypes) {
      mydata$OtherDataset <- dfm$changeFactorToUnaryNumericType(mydata$OtherDataset,1)
      ftc$createFrequencyTableForListOfVarsByVarValues(mydata,usedDatasetTypes,"DatasetType")
    },
    
    createBarChartNumberOfArticlesPerDatasetTypeAndDifferentArticlesIncluded = function(mydata,usedDatasetTypes,combineWithList){
      changeRowNameForColumn <- function(dataFrame,columnName,oldRowName, newRowName){
        dataFrame <- dfm$addNewFactorLevel(dataFrame,columnName,newRowName)
        dataFrame[dataFrame[,columnName] == oldRowName,columnName] <- newRowName 
        dataFrame <- dfm$removeUnusedFactorLevels(dataFrame)
        return(dataFrame)
      }
      
      articlesPerDataSetTypesJoined <- numberOfArticlesUsingDatasetsTypeByDifferentArticlesIncluded(mydata,usedDatasetTypes,combineWithList)
      
      articlesPerDataSetTypesJoined <- changeRowNameForColumn(articlesPerDataSetTypesJoined,"DatasetType", "PersonalGenOrStudDataset","PersonalGenOrStud")
      articlesPerDataSetTypesJoined <- changeRowNameForColumn(articlesPerDataSetTypesJoined,"DatasetType", "PersonalGeneratedDataset","PerosnalGenerated")
      articlesPerDataSetTypesJoined <- changeRowNameForColumn(articlesPerDataSetTypesJoined,"DatasetType", "PersonalStudentDataset","PerosnalStudent")
      articlesPerDataSetTypesJoined <- changeRowNameForColumn(articlesPerDataSetTypesJoined,"DatasetType", "OtherDataset","Other")
      
      articlesPerDataSetTypesJoined <- dfmHelp$changeColumnNameInDataFrame(articlesPerDataSetTypesJoined,"ToolUsedOrDeveloped","UsingTools")
      articlesPerDataSetTypesJoined <- dfmHelp$changeColumnNameInDataFrame(articlesPerDataSetTypesJoined,"ToolDeveloped","DevelopingTool")
      articlesPerDataSetTypesJoined <- dfmHelp$changeColumnNameInDataFrame(articlesPerDataSetTypesJoined,"ToolCompariosonOrOnlyComparison","ComparingTools")
      
      graph <- .self$graphBuilder$copy()
      graph$graphTitle = "Number of articles per Dataset Type"
      graph$legendTitle = "Articles included"
      graph$xAxisLabelAngle = 35
      graph$showBarChart(articlesPerDataSetTypesJoined, "DatasetType", c("All","ToolUsedOrDeveloped","ToolDeveloped","ToolCompariosonOrOnlyComparison"))
      
    },
    
    numberOfArticlesUsingDatasetsTypeByDifferentArticlesIncluded = function(mydata,usedDatasetTypes,combineWithList) {
      mydata$OtherDataset <- dfm$changeFactorToUnaryNumericType(mydata$OtherDataset,1)
      ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(mydata,usedDatasetTypes,"DatasetType",combineWithList)
    },

    numberOfArticlesUsingHowManyCoursesForTest = function(mydata) {
      ftc$createFrequencyTable(mydata,"CoursesNumber") 
    },
    
    numberOfArticlesUsingHowManyAssignementsInTotal = function(mydata) {
      mydata <- ftc$createFrequencyTable(mydata,"TotalAsignementNumber")
      mydata[order(mydata[,"freq"]),]
    },
    
    numberOfArticlesUsingHowManyTotalSubmissions = function(mydata) {
      ftc$createFrequencyTable(mydata,"TotalSubmissionNumber")
    },
    
    numberOfArticlesUsingHowManyTotalSubmissionsGrouped = function(mydata) {
      mydata <- ftc$createFrequencyTable(mydata,"TotalSubmissionNumberGrouped")
      mydata[order(mydata[,"freq"]),]
    },
    
    numberOfAriclesUsingPlagDetectionOnWhichLevel = function(mydata,academicLevels){
      ftc$createFrequencyTableForListOfVarsByVarValues(mydata,academicLevels,"AcademicLevels")
    },
    
    numberOfAriclesUsingPlagDetectionOnWhichLevelPerMaxProgramLength = function(mydata.filtered,academicLevels){
      ftc$createFrequencyTableForListOfVarsInComboWithVarsIncludingTheirFrequency(mydata,academicLevels,"ProgramMAXLengthRange","Total For All Levels")
    },
    
    numberOfArticlesUsingDatasetsCommingFromWhichInstitution = function(mydata) {
      ftc$createFrequencyTable(mydata,"FromInstitution")
    },
    
    numberOfArticlesUsingWhichProgrammingLanguageMinMaxYear = function(mydata,programmingLanguages) {
      joinedDF <- list()
      joinedDF[[2]] <- numberOfAriclesUsingPWhichProgrammingLanguage(mydata,programmingLanguages)
      joinedDF[[1]] <- yearAnalizator$calculateMinAndMaxYearUsedByVariable(mydata,programmingLanguages,"Programming Language")
      dfm$joinListOfDataFrames(joinedDF,"Programming Language")
    },
    
    numberOfAriclesUsingPWhichProgrammingLanguage = function(mydata,programmingLanguages){
      mydata$SupportsAlsoOthers <- dfm$changeFactorToUnaryNumericType(mydata$SupportsAlsoOthers,1)
      freqTable <- ftc$createFrequencyTableForListOfVarsByVarValues(mydata,programmingLanguages,"Programming Language")
      dfm$orderDataFrameValuesByColumns(freqTable,"X1",TRUE)
    },
    
    numberOfArticlesUsingWhichProgrammingLanguagePerYear = function(mydata,programmingLanguages){
      resultDf <- ftc$createFrequencyTableForListOfVarsInComboWithVars(mydata,programmingLanguages,"Year")
      dfm$addTotalRowAtTheBottomOfDataFrame(resultDf,TRUE)
    },

    numberOfArticlesUsingMAXProgramLength = function(mydata) {
      ftc$createFrequencyTable(mydata,"ProgramMAXLengthRange")
    },
    
    numberOfAriclesUsingPWhichProgrammingLanguagePerMAXProgramLength = function(mydata,programmingLanguages){
      ftc$createFrequencyTableForListOfVarsInComboWithVarsIncludingTheirFrequency(mydata,programmingLanguages,"ProgramMAXLengthRange","Total For All Languanges")
    }
  )
)