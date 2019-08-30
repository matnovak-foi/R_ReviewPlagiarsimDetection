ArticleTypesAnalysis <- setRefClass("ArticleTypesAnalysis", 
                                                
 fields = list(
   ftc="ANY",
   graphBuilder = "ANY"
 ),
 
 methods = list(
   initialize = function() {
     ftc <<- FrequencyTableCreator$new()
     graphBuilder <<- GraphBuilderGGplot$new()
     
     graphBuilder$xAxisTitle <<- "ResearchType"
     graphBuilder$yAxisTitle <<- "Number of Articles"
   },
                                                                                    
    createArticleByTypeBarChart = function(mydata){
      freqByArticleType <- ftc$createFrequencyTable(mydata,"ResearchType")
      
      graph <- .self$graphBuilder$copy()
      graph$graphTitle = "Number of Articles per Type"
      #graph$legendTitle = ""
      graph$showBarChart(freqByArticleType,"ResearchType")
    },
    
    numberOfReviewArticles = function(mydata){
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"ReviewArticle","Not a review article")
    },
    
    numberOfArticlesPerformingSurvey = function(mydata) {
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"Survay","Not doing any survay")
    },
    
    numberOfArticlesWithVisualization = function(mydata) {
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"Vizualization","Not doing any vizualization")
    },
    
    numberOfArticlesMoreFocusedOnEthics = function(mydata) {
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"Ethics","Not focused on ethics")
    },
    
    numberOfArticlesPerResearchMethod = function(mydata) {
      ftc$createFrequencyTable(mydata,"ResearchMethod")
    }
 )
)