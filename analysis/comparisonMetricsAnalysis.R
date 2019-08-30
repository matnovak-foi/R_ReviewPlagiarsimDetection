CompariosnMetricsAnalysis <- setRefClass("CompariosnMetricsAnalysis", 
 fields = list(
   ftc="ANY",
   dfm="ANY",
   yearAnalizator = "ANY"),
 
 methods = list(
   initialize = function() {
     dfm <<- DataFrameMaipulator$new()
     ftc <<- FrequencyTableCreator$new()
     yearAnalizator <<- YearAnalizator$new()
   }, 
                                        
  numberOfArticlesUsingComparisonMetric = function(mydata,metricName){
    ftc$createFrequencyTableForUnaryValueVariable(mydata,metricName,"Not using this metric")
  },
  
  numberofArticlesUsingTopcomparisonMetrics = function(mydata,metricsNames) {
    ftc$createFrequencyTableForListOfVarsByVarValues(mydata,metricsNames,"MetricName")
  },
  
  numberOfMetricsUsedPerYear = function(mydata,metricNames){
    ftc$createFrequencyTableForListOfVarsInComboWithVarsIncludingTheirFrequency(mydata,metricNames,"Year","Total Number of Articles")
  },
  
  getMinAndMaxYearUsedByMetric = function(mydata,metricNames) {
    yearAnalizator$calculateMinAndMaxYearUsedByVariable(mydata,metricNames,"Comparison Metric")
  },
  
  numberOfArticlesWithMinAndMaxYearComapringAndDevelopig = function(){
    topMetricsDev <- cma$numberofArticlesUsingTopcomparisonMetrics(mydata.filtered.devTool,topMetrics)
    topMetricsDev <- dfmHelp$changeColumnNameInDataFrame(topMetricsDev,"MetricName", "Comparison Metric")
    topMetricsDev <- dfmHelp$changeColumnNameInDataFrame(topMetricsDev,"X1", "Developing")
    topMetricsComp <- cma$numberofArticlesUsingTopcomparisonMetrics(mydata.filtered.compTools,topMetrics)
    topMetricsComp <- dfmHelp$changeColumnNameInDataFrame(topMetricsComp,"MetricName", "Comparison Metric")
    topMetricsComp <- dfmHelp$changeColumnNameInDataFrame(topMetricsComp,"X1", "Comparing")
    minMaxComparions <- cma$getMinAndMaxYearUsedByMetric(mydata.filtered.compTools,topMetrics)
    comparisonMetricTable <- dfmHelp$joinListOfDataFrames(list(minMaxComparions,topMetricsComp,topMetricsDev),"Comparison Metric")
    comparisonMetricTable <- dfmHelp$orderDataFrameValuesByColumns(comparisonMetricTable,c("lastYear","Comparing"))
    return(comparisonMetricTable)
  },

  numberOfArticlesUsingSomeOtherMessure = function(mydataOrg){
    nrow(ftc$createFrequencyTable(mydataOrg,"OtherComparisonMetric"))-1
  },
  
  numberOfArticlesUsingPrecisionAndRecallCombinations = function(mydataOrg) {
    numberOfArticlesUsingPrecisionAndRecall <- function(mydata){
      mydata <- ftc$createFreqencyTableWithoutNAValues(mydata,c("Precision","Recall"))
      colnames(mydata)[1] <- "Combination"
      mydata <- mydata[,c("Combination","freq")]
      mydata <- changeCombinationRowName(mydata,"Precision and Recall")
      return(mydata)
    }
    
    numberOfArticlesUsingWhat<- function(mydata,what){
      mydata<-ftc$createFreqencyTableWithoutNAValues(mydata,what)
      colnames(mydata)[1] <- "Combination"
      mydata <- changeCombinationRowName(mydata,what)
      return(mydata)
    }
    
    numberOfArticlesUsingPrecisionAndNoRecall <- function(mydata){
      mydata <- mydata[is.na(mydata$Recall),]
      mydata<-ftc$createFreqencyTableWithoutNAValues(mydata,"Precision")
      colnames(mydata)[1] <- "Combination"
      mydata <- changeCombinationRowName(mydata,"Precision and No Recall")
      return(mydata)
    }
    
    numberOfArticlesUsingNoPrecisionAndRecall <- function(mydata){
      mydata <- mydata[is.na(mydata$Precision),]
      mydata<-ftc$createFreqencyTableWithoutNAValues(mydata,"Recall")
      colnames(mydata)[1] <- "Combination"
      mydata <- changeCombinationRowName(mydata,"Recall and No Precision")
      return(mydata)
    }
    
    numberOfArticlesUsingPrecisionAndRecallNoFbeta <- function(mydata){
      mydata <- mydata[is.na(mydata$Fbeta),]
      mydata <- ftc$createFreqencyTableWithoutNAValues(mydata,c("Precision","Recall"))
      colnames(mydata)[1] <- "Combination"
      mydata <- mydata[,c("Combination","freq")]
      mydata <- changeCombinationRowName(mydata,"Recall and Precision No F-beta")
      return(mydata)
    }
    
    numberOfArticlesUsingPrecisionAndRecallAndFbeta <- function(mydata){
      mydata <- mydata[!is.na(mydata$Fbeta),]
      mydata <- ftc$createFreqencyTableWithoutNAValues(mydata,c("Precision","Recall"))
      colnames(mydata)[1] <- "Combination"
      mydata <- mydata[,c("Combination","freq")]
      mydata <- changeCombinationRowName(mydata,"Recall and Precision and F-beta")
      return(mydata)
    }
    
    changeCombinationRowName <- function(dataFrame,rowName) {
      if(nrow(dataFrame)>0){
        dataFrame[,"Combination"] <- rowName
      } else {
        dataFrame <- data.frame(Combination=rowName,freq=0)
      }
      return(dataFrame)
    }
    
    precisionRecallFrequencyList <- list()
    precisionRecallFrequencyList[[1]]<- numberOfArticlesUsingWhat(mydataOrg,"Precision")
    precisionRecallFrequencyList[[2]]<- numberOfArticlesUsingWhat(mydataOrg,"Recall")
    precisionRecallFrequencyList[[3]]<- numberOfArticlesUsingWhat(mydataOrg,"Fbeta")
    precisionRecallFrequencyList[[4]]<-numberOfArticlesUsingPrecisionAndRecall(mydataOrg)
    precisionRecallFrequencyList[[5]]<-numberOfArticlesUsingPrecisionAndNoRecall(mydataOrg)
    precisionRecallFrequencyList[[6]]<-numberOfArticlesUsingNoPrecisionAndRecall(mydataOrg)
    precisionRecallFrequencyList[[7]]<-numberOfArticlesUsingPrecisionAndRecallNoFbeta(mydataOrg)
    precisionRecallFrequencyList[[8]]<-numberOfArticlesUsingPrecisionAndRecallAndFbeta(mydataOrg)
    
    return(dfm$joinListOfDataFrames(precisionRecallFrequencyList,"Combination"))
  }

))