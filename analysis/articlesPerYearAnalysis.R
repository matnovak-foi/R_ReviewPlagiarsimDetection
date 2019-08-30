
ArticlePerYearAnalysis <- setRefClass("ArticlePerYearAnalysis", 
  fields = list(
    listm = "ANY", 
    graphBuilder = "ANY", 
    ftc="ANY",
    dfm="ANY"),
  
  methods = list(
    initialize = function() {
     listm <<- ListManipulator$new()
     dfm <<- DataFrameMaipulator$new()
     ftc <<- FrequencyTableCreator$new()
     graphBuilder <<- GraphBuilderGGplot$new()
     
     graphBuilder$xAxisTitle <<- "Year"
     graphBuilder$yAxisTitle <<- "Number of Articles"
    }, 
  
    createTableOfAllAnalysisArticles = function(mydata) {
      reviewArticlesTable <- mydata[,c("AnalysisID","Year","ArticleReferenceListName")]
      reviewArticlesTable <- dfm$changeColumnNameInDataFrame(reviewArticlesTable,"AnalysisID","ID")
      reviewArticlesTable <- dfm$changeColumnNameInDataFrame(reviewArticlesTable,"ArticleReferenceListName","Article")
      return(reviewArticlesTable)
    },
    
    createArticlesPerYearDottedChart = function(mydata) {
      freqTable <- ftc$createFrequencyTable(mydata,"Year")
      
      graph <- .self$graphBuilder$copy()
      graph$graphTitle = "Articles per Year"
      graph$showSomekindOfPerYearDottedChart(freqTable,"freq")
    },

    createArticlesInDatabasePerYearDottedChart = function(mydata,database) {
      freqTable <- ftc$createFreqencyTableWithoutNAValues(mydata,c("Year",database))
      freqTable <- freqTable[,c("Year","freq")]
      
      graph <- .self$graphBuilder$copy()
      graph$graphTitle = paste0("Articles per Year in ",database)
      graph$showSomekindOfPerYearDottedChart(freqTable,"freq")
    },
    
    createTopCitedArticlesPerYearDottedChartLogarimic = function(mydata) {
      graph <- createTopCitedArticlesPerYearDottedChart(mydata)
      graph$chart + scale_y_log10()
    },
    
    createTopCitedArticlesPerYearDottedChart = function(mydata) {
      citedArticles <- mydata[,c("Year","ScolarCitationSum")]
      
      graph <- .self$graphBuilder$copy()
      graph$graphTitle = "Citation rate per Year"
      graph$yAxisTitle = "Citation Rate"
      graph$showSomekindOfPerYearDottedChart(citedArticles,"ScolarCitationSum")
      return(graph)
    },
    
    createArticlesInDatabasesPerLastXYearsBarChart = function(yearDatabases,databases,yearFrom,yearTo) {
      yearDatabases$Year <- as.numeric(as.character(yearDatabases$Year))
      yearDatabases <- yearDatabases[yearDatabases$Year > yearFrom & yearDatabases$Year < yearTo ,]
      yearDatabases$Year <- as.factor(as.character(yearDatabases$Year))
      
      createArticlesInDatabasesPerYearBarChart(yearDatabases, databases)
    },
    
    createArticlesInDatabasesPerYearBarChart = function (yearDatabases,databases){
      freqByDatabases <- ftc$createFrequencyTableForListOfVarsInComboWithVars(yearDatabases,databases,"Year")

      graph <- .self$graphBuilder$copy()
      graph$graphTitle = "Number of Articles in Databases per Year"
      graph$legendTitle = "Database"
      graph$showBarChart(freqByDatabases,"Year",databases)
    },
    
    getFreqTableForArticlesInDatabasesWithMissings = function(yearDatabases,databases){
      yearDatabasesFreq <- getFreqTableForArticlesInDatabases(yearDatabases,databases)
      
      for(d in databases){
        yearDatabasesFreq[,paste0(d,"Missing")] <- yearDatabasesFreq$ArticlesPerYear - yearDatabasesFreq[,d]
      }
      
      return(yearDatabasesFreq)
    },
    
    getFreqTableForArticlesInDatabases = function (yearDatabases,databases) {
      yearDatabasesFreq <- ftc$createFrequencyTableForListOfVarsInComboWithVarsIncludingTheirFrequency(yearDatabases,databases,"Year","ArticlesPerYear")
      yearDatabasesFreq <- dfm$replaceAllNaValuesWithValue(yearDatabasesFreq,0)
      yearDatabasesFreq <- dfm$addTotalRowAtTheBottomOfDataFrame(yearDatabasesFreq,TRUE)
      
      return(yearDatabasesFreq)
    },
    
    getNumberOfArticlesPerDatabase = function(mydata, databases){
      ftc$createFrequencyTableForListOfVarsByVarValues(mydata,databases,"DatabaseName")
    },
    
    getNumberOfArticlesInDatabaseCombination = function(mydata, databases){
      ftc$createFreqencyTableWithoutNAValues(mydata,databases)
    },
    
    articlesInDatabaseCombination = function(mydata, databases){
      for(d in databases){
        mydata <- mydata[!is.na(mydata[,d]),c("AnalysisID","Year","ScolarCitationSum",databases)] 
      }
      mydata <- dfm$orderDataFrameValuesByColumns(mydata,"ScolarCitationSum",TRUE)
      return(mydata)
    },
    
    getNumberOfUniqueArticlesPerDatabase = function(yearDatabases, databases) {
      
      createCommandToGetNumberOfUniqueArticlesPerDatabase <- function(yearDatabases, databases) {
        
        createCommandValuesPartForOneDatabase <- function(commandValues, databases, database) {
          
          createTheUniquePartOfTheCommandValues <- function(commandValues,databases, database){
            for(d in databases) 
              if(database != d)
                commandValues=paste0(commandValues,"is.na(yearDatabases$",d,")&");
              
            return(commandValues)
          }
          
          commandValues=paste0(commandValues,"sum(yearDatabases[")
          commandValues=createTheUniquePartOfTheCommandValues(commandValues,databases, database)
          commandValues=substr(commandValues, 1, nchar(commandValues)-1)
          commandValues=paste0(commandValues,",'",d,"'])")
          commandValues=paste0(commandValues,",")
          
          return(commandValues)
        }
        
        commandNames="r <- data.frame(DatabaseName=c("
        commandValues="Value=c("
        for(d in databases){
          commandNames=paste0(commandNames,"'",d,"',")
          commandValues <- createCommandValuesPartForOneDatabase(commandValues, databases, d)
        }
      
        commandNames=substr(commandNames, 1, nchar(commandNames)-1)
        commandNames=paste0(commandNames,"),")
        commandValues=substr(commandValues, 1, nchar(commandValues)-1)
        commandValues=paste0(commandValues,"))")
        finalCommand=paste0(commandNames,commandValues)
      }
      
      finalCommand <- createCommandToGetNumberOfUniqueArticlesPerDatabase(yearDatabases, databases)
      eval(parse(text=finalCommand)) 
      
      return(r)
    },
    
    uniqueArticlesPerDatabseWithCitations = function(mydata,databases) {

      getOnlyUniqueArticlesForTheDatabase <- function(resultOneDB,databases,database){
        for(d2 in databases){
          if(database != d2){
            resultOneDB <- resultOneDB[is.na(resultOneDB[,d2]),]
          }
        }
        
        return(resultOneDB)
      }
      
      prepareResultsToBeJoinable <- function(resultsOneDB,database){
        resultOneDB[,database] <- database
        resultOneDB <- dfm$changeColumnNameInDataFrame(resultOneDB,database,"DatabaseName")
        resultOneDB <- resultOneDB[,c("AnalysisID","Year","DatabaseName","ScolarCitationSum")]
        resultOneDB <- dfm$orderDataFrameValuesByColumns(resultOneDB,"ScolarCitationSum",TRUE)
        return(resultOneDB)
      }
      
      listOfArticlesForDatabases <- list()
      for(d in databases){
        resultOneDB <- mydata[!is.na(mydata[,d]),]
        resultOneDB <- getOnlyUniqueArticlesForTheDatabase(resultOneDB, databases, d);
        resultOneDB <- prepareResultsToBeJoinable(resultsOneDB, d)
        listOfArticlesForDatabases <- listm$addElementToList(listOfArticlesForDatabases,resultOneDB)
      }
      
      result <- dfm$joinListOfDataFrames(listOfArticlesForDatabases,c("AnalysisID","ScolarCitationSum"))
      result[,"DatabaseName"] <- as.factor(result[,"DatabaseName"])
      
      rownames(result) <- NULL
      
      return(result)
    },
    
    getArticlesSortedAndFilteredByCitation = function(mydata,largerThan=-1,lessThan=100000000){
      topCitedArticles <- getArticlesSortedByCitation(mydata)
      topCitedArticles <- topCitedArticles[topCitedArticles$ScolarCitationSum>largerThan 
                                           & topCitedArticles$ScolarCitationSum<lessThan,]
      numberOfArticles <- nrow(topCitedArticles)
      topCitedArticles <- rbind(topCitedArticles,data.frame(AnalysisID=NA,Year="Number of articles",ScolarCitationSum=numberOfArticles))
      topCitedArticles <- dfm$removeUnusedFactorLevels(topCitedArticles)
      return(topCitedArticles)
    },
    
    getArticlesSortedByCitation = function(mydata) {
      topCitedArticles <- mydata[,c("AnalysisID","Year","ScolarCitationSum")]
      topCitedArticles <- dfm$orderDataFrameValuesByColumns(topCitedArticles,"ScolarCitationSum",descending = TRUE)
      return(topCitedArticles)
    }
))