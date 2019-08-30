OBandPPMethodsAnalysis <- setRefClass("OBandPPMethodsAnalysis", 
                                      
  fields = list(
    ftc="ANY",
    graphBuilder="ANY"
  ),
  
  methods = list(
    initialize = function() {
      ftc <<- FrequencyTableCreator$new()
      graphBuilder <<- GraphBuilderGGplot$new()
      
      graphBuilder$xAxisTitle <<- "Year"
      graphBuilder$yAxisTitle <<- "Number of Articles"
    }, 
    
    numberOfArticlesMentioningObfucationMethods = function(mydata) {
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"ObfucationMethod","Articles that are not mentioning any OB methods")
    },
    
    numberOfArticlesUsingWhatKindOfObfuscationMethod = function(mydata,obMethodsList) {
      ftc$createFrequencyTableForListOfVarsByVarValues(mydata,obMethodsList,"ObfucationMethod")
    },
    
    numberOfArticlesUsingWhatKindOfObfuscationMethodPerYear = function(mydata,obMethodsList) {
      ftc$createFrequencyTableForListOfVarsInComboWithVars(mydata,obMethodsList,"Year")
    },
    
    createMentioingOfObfuscationMethodPerYearBarChart = function (mydata,obMethodsList){
      result <- numberOfArticlesUsingWhatKindOfObfuscationMethodPerYear(mydata,obMethodsList)
      graph <- .self$graphBuilder$copy()
      graph$graphTitle = "Number of Articles Mentioning Obfuscation method Per Year"
      graph$legendTitle = "Obfuscation Method"
      graph$showBarChart(result,"Year",obMethodsList)
    },
    
    createMentioingOfObfuscationMethodBarChart = function (mydata,obMethodsList){
      result <- numberOfArticlesUsingWhatKindOfObfuscationMethod(mydata,obMethodsList)
      graph <- .self$graphBuilder$copy()
      graph$xAxisLabelAngle = 70
      graph$graphTitle = "Number of Articles Mentioning Obfuscation Method"
      graph$xAxisTitle = "Obfuscation Mehtod"
      graph$showBarChart(result,"ObfucationMethod")
    },
    
    numberOfArticlesUsingWhatKindOfOBMethodtehniqueIncombinationWithOtherOBMehtod = function(mydata, obMethodsList){
      ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(mydata,obMethodsList,"Obfucation Methods",list("OM_05_L","OM_07_S","OM_08_S","OM_12_AS",c("OM_08_S","OM_12_AS"),c("OM_07_S","OM_05_L")))
    },
    
    numberOfArticlesUsingPPtechniques = function(mydata) {
      ftc$createFrequencyTableForUnaryValueVariable(mydata,"PPtechniques","Articles that are not using PP techniques")
    },
    
    numberOfArticlsUsingWhatKindOfPPtehnique = function(mydata,ppTehniques){
      ftc$createFrequencyTableForListOfVarsByVarValues(mydata,ppTehniques,"Preprocessing Technique")
    },
    
    numberOfArticlesUsingWhatKindOfPPtehniqueIncombinationWithOtherTehniques = function(mydata, ppTechniques){
      ftc$createFrequnecyTableForListOfVarsWithUnaryVarValuesInCobmoWithVars(mydata,ppTechniques,"Preprocesing Techniques",list("ReorderCode","UpperCaseLettersToLowerCase","NoComments","NoWhiteSpacesAndNewLines",c("NoComments","NoWhiteSpacesAndNewLines"),"NoProfessorsCode","StringReformat","RemovePartsOfCode","OtherPP"))
    }
  )
)