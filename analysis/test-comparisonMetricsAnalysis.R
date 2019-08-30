context("comparisonMetricsAnalysis")
cma <- CompariosnMetricsAnalysis$new()

describe(" - when - numberOfArticlesUsingPrecisionAndRecallCombinations",{
  context(" - when - numberOfArticlesUsingPrecisionAndRecallCombinations")
  resultDF <- data.frame(Combination=c("Precision","Recall","Fbeta","Precision and Recall","Precision and No Recall","Recall and No Precision","Recall and Precision No F-beta","Recall and Precision and F-beta"))
  
  test_that("givenOkDataFrameWithPrecisionRecallAndFbetaColumns_returnCombinations",{
    mydata <- data.frame(Precision=c(1,1,1,NA,1,NA,1,1,1,1),Recall=c(1,1,1,NA,1,NA,NA,NA,NA,NA),Fbeta=c(1,1,NA,NA,NA,NA,NA,NA,NA,NA))
    resultDF$freq <- c(8,4,2,4,4,0,2,2) 
    expect_that(cma$numberOfArticlesUsingPrecisionAndRecallCombinations(mydata),is_identical_to(resultDF))
    
    mydata <- data.frame(Precision=c(1,1,1,NA,1,NA,NA,NA,NA,NA),Recall=c(1,1,1,NA,1,NA,NA,NA,NA,NA),Fbeta=c(1,1,NA,NA,NA,NA,NA,NA,NA,NA))
    resultDF$freq <- c(4,4,2,4,0,0,2,2)
    expect_that(cma$numberOfArticlesUsingPrecisionAndRecallCombinations(mydata),is_identical_to(resultDF))
  })
})