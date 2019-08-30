context("articlesPerYearAnalysis")
apya <- ArticlePerYearAnalysis$new()

describe(" - when - getNumberOfUniqueArticlesPerDatabase",{
  context(" - when - getNumberOfUniqueArticlesPerDatabase")
  
  test_that("givendataFrameWithYearsAndDatabesesAndCorrectListOfDatabases_showFrequencies",{
    
    yearDatabasesDF <- data.frame(Year=c("2000","2002","2003","2004","2006","2007"),Scopus=c(1,NA,1,1,1,1),ACM=c(NA,1,1,NA,NA,NA))
    databases <- c("Scopus","ACM") 
    resultDF <- data.frame(DatabaseName=c("Scopus","ACM"),Value=c(4,1))
    expect_that(apya$getNumberOfUniqueArticlesPerDatabase(yearDatabasesDF,databases),is_identical_to(resultDF))
    
    yearDatabasesDF <- data.frame(Year=c("2000","2002","2003","2004","2006","2007"),Scopus=c(1,NA,1,1,1,NA),ACM=c(NA,1,1,NA,NA,NA),WOS=c(NA,NA,NA,NA,NA,1))
    databases <- c("Scopus","ACM","WOS") 
    resultDF <- data.frame(DatabaseName=c("Scopus","ACM","WOS"),Value=c(3,1,1))
    expect_that(apya$getNumberOfUniqueArticlesPerDatabase(yearDatabasesDF,databases),is_identical_to(resultDF))
  })
})


describe(" - when - uniqueArticlesPerDatabseWithCitations",{
  context(" - when - uniqueArticlesPerDatabseWithCitations")
  
  test_that("givendataFrameWithArticlesDatabesesAndScollarCitationAndCorrectListOfDatabases_showUiqueAritclesPerDatabaseWithCitation",{
    mydata <- data.frame(AnalysisID=c(1,2,3,4,5,6),Year=c("2000","2002","2003","2004","2006","2007"),ScolarCitationSum=c(500,100,25,20,6,5),Scopus=c(1,NA,NA,NA,NA,NA))
    databases <- c("Scopus") 
    resultDF <- data.frame(AnalysisID=c(1),Year=c("2000"),DatabaseName=c("Scopus"),ScolarCitationSum=c(500))
    expect_that(apya$uniqueArticlesPerDatabseWithCitations(mydata,databases),is_identical_to(resultDF))
    
    
    mydata <- data.frame(AnalysisID=c(1,2,3,4,5,6),Year=c("2000","2002","2003","2004","2006","2007"),ScolarCitationSum=c(500,100,25,20,6,5),Scopus=c(1,NA,1,1,1,NA))
    databases <- c("Scopus") 
    resultDF <- data.frame(AnalysisID=c(1,3,4,5),Year=c("2000","2003","2004","2006"),DatabaseName=c("Scopus","Scopus","Scopus","Scopus"),ScolarCitationSum=c(500,25,20,6))
    expect_that(apya$uniqueArticlesPerDatabseWithCitations(mydata,databases),is_identical_to(resultDF))
    
    mydata <- data.frame(AnalysisID=c(1,2,3,4,5,6),Year=c("2000","2002","2003","2004","2006","2007"),ScolarCitationSum=c(500,100,25,20,6,5),
                                  Scopus=c(1,NA,1,1,1,NA),
                                     WOS=c(1,NA,1,NA,NA,1),
                                     ACM=c(NA,1,1,NA,NA,NA))
    databases <- c("Scopus","ACM","WOS") 
    resultDF <- data.frame(AnalysisID=c(4,5,2,6),Year=c("2004","2006","2002","2007"),DatabaseName=c("Scopus","Scopus","ACM","WOS"),ScolarCitationSum=c(20,6,100,5))
    expect_that(apya$uniqueArticlesPerDatabseWithCitations(mydata,databases),is_identical_to(resultDF))
  })
})

describe(" - when - getArticlesSortedAndFilteredByCitation",{
  context(" - when - getArticlesSortedAndFilteredByCitation")
  
  test_that("givendataFrameWithScollarCitationsAndFromToRange_showFrequenciesFromToRange",{
    yearDatabasesDF <- data.frame(AnalysisID=c(1,2,3,4,5,6),Year=c("2000","2002","2003","2004","2006","2007"),ScolarCitationSum=c(20,25,5,100,500,6))
    resultDF <- data.frame(AnalysisID=c(5,4,2,1,6,3,NA),Year=c("2006","2004","2002","2000","2007","2003","Number of articles"),ScolarCitationSum=c(500,100,25,20,6,5,6))
    expect_that(apya$getArticlesSortedAndFilteredByCitation(yearDatabasesDF),is_identical_to(resultDF))
    
    yearDatabasesDF <- data.frame(AnalysisID=c(1,2,3,4,5,6),Year=c("2000","2002","2003","2004","2006","2007"),ScolarCitationSum=c(20,25,5,100,500,6))
    resultDF <- data.frame(AnalysisID=c(5,4,2,1,NA),Year=c("2006","2004","2002","2000","Number of articles"),ScolarCitationSum=c(500,100,25,20,4))
    expect_that(apya$getArticlesSortedAndFilteredByCitation(yearDatabasesDF,10),is_identical_to(resultDF))
    
    yearDatabasesDF <- data.frame(AnalysisID=c(1,2,3,4,5,6),Year=c("2000","2002","2003","2004","2006","2007"),ScolarCitationSum=c(20,25,5,100,500,6))
    resultDF <- data.frame(AnalysisID=c(2,1,NA),Year=c("2002","2000","Number of articles"),ScolarCitationSum=c(25,20,2))
    expect_that(apya$getArticlesSortedAndFilteredByCitation(yearDatabasesDF,10,100),is_identical_to(resultDF))
  })
})
