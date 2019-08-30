#auto_test("analysis/", "analysis/", reporter="summary")

source("mainHeader.R",encoding = "UTF-8")
#Bibliographic analysis
apya$createTableOfAllAnalysisArticles(mydata.filtered)
apya$createTableOfAllAnalysisArticles(mydata.filteredWithManual)
apya$createArticlesPerYearDottedChart(mydata.filtered)
apya$createArticlesPerYearDottedChart(mydata.filteredWithManual)

apya$createArticlesInDatabasePerYearDottedChart(mydata.filtered,"Scopus")
apya$createArticlesInDatabasePerYearDottedChart(mydata.filtered,"ACM")
apya$createArticlesInDatabasePerYearDottedChart(mydata.filtered,"IEEE")
apya$createArticlesInDatabasePerYearDottedChart(mydata.filtered,"WOS")
apya$createArticlesInDatabasePerYearDottedChart(mydata.filtered,"SD")

apya$createArticlesInDatabasesPerYearBarChart(mydata.filtered,databases)
apya$createArticlesInDatabasesPerLastXYearsBarChart(mydata.filtered,databases,"2004","2017")

apya$createTopCitedArticlesPerYearDottedChart(mydata.filtered)
apya$createTopCitedArticlesPerYearDottedChartLogarimic(mydata.filtered)

source("mainHeader.R",encoding = "UTF-8")
#Bibliographic analysis
apya$getFreqTableForArticlesInDatabases(mydata.filtered,databases)
apya$getFreqTableForArticlesInDatabasesWithMissings(mydata.filtered,databases)

apya$getNumberOfArticlesPerDatabase(mydata.filtered,databases)
apya$getNumberOfArticlesPerDatabase(mydata.filtered.topCited,databases)

apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered,c("Scopus","WOS","IEEE"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered,c("Scopus","WOS","ACM"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered,c("IEEE","WOS","ACM"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered,c("Scopus","WOS","ACM","IEEE"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered,c("Scopus","WOS"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered,c("ACM","IEEE"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered,c("WOS","IEEE"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered,c("Scopus","IEEE"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered,c("Scopus","ACM"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered,c("WOS","ACM"))

apya$articlesInDatabaseCombination(mydata.filtered,c("Scopus","WOS","IEEE"))
apya$articlesInDatabaseCombination(mydata.filtered,c("Scopus","WOS","ACM"))
apya$articlesInDatabaseCombination(mydata.filtered,c("Scopus","WOS","ACM","IEEE"))

apya$getNumberOfUniqueArticlesPerDatabase(mydata.filtered,databases)
apya$uniqueArticlesPerDatabseWithCitations(mydata.filtered,databases)

wa$getTopAuthors()
wa$createWordCloude(30)
wa$colorWordCloude(30)
wa$sortedTermMatrix

source("mainHeader.R",encoding = "UTF-8")
#add selection of lessThat or greatherThan citation rate
apya$getArticlesSortedAndFilteredByCitation(mydata.filtered)
apya$getArticlesSortedAndFilteredByCitation(mydata.filtered,50)
apya$getArticlesSortedAndFilteredByCitation(mydata.filtered,20,50)

apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered.topCited,c("Scopus","WOS","IEEE"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered.topCited,c("Scopus","WOS","ACM"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered.topCited,c("IEEE","WOS","ACM"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered.topCited,c("Scopus","WOS","ACM","IEEE"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered.topCited,c("Scopus","WOS"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered.topCited,c("ACM","IEEE"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered.topCited,c("WOS","IEEE"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered.topCited,c("Scopus","IEEE"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered.topCited,c("Scopus","ACM"))
apya$getNumberOfArticlesInDatabaseCombination(mydata.filtered.topCited,c("WOS","ACM"))

noWOS <- mydata.filtered[is.na(mydata.filtered$WOS),]
apya$getNumberOfArticlesInDatabaseCombination(noWOS,c("ACM","IEEE"))
noACM <- mydata.filtered[is.na(mydata.filtered$ACM),]
apya$getNumberOfArticlesInDatabaseCombination(noACM,c("WOS","IEEE"))
noIEEE <- mydata.filtered[is.na(mydata.filtered$IEEE),]
apya$getNumberOfArticlesInDatabaseCombination(noIEEE,c("ACM","WOS"))

source("mainHeader.R",encoding = "UTF-8")
#Article types analysis
ata$createArticleByTypeBarChart(mydata.filtered)
ata$numberOfReviewArticles(mydata.filtered)
ata$numberOfArticlesPerformingSurvey(mydata.filtered)
ata$numberOfArticlesWithVisualization(mydata.filtered)
ata$numberOfArticlesMoreFocusedOnEthics(mydata.filtered)
ata$numberOfArticlesPerResearchMethod(mydata.filtered)

source("mainHeader.R",encoding = "UTF-8")
#tools analysis
pdta$listArticlesThatUseTool(mydata.filtered,"jPlag")
pdta$numberOfArticlesUsingAToolByType(mydata.filtered,"jPlag")
pdta$listArticlesThatUseTools(mydata.filtered,topTools)
pdta$numberOfArticlesUsingToolsPerUsageType(mydata.filtered,topTools)
pdta$numberOfArticlesUsingToolsIncludingUsageTypes(mydata.filtered,topTools,c("c","u","d"))
pdta$numberOfAtriclesDevelopingTool(mydata.filtered)
pdta$numberOfArticlesTahatDevelopedOrUsedTool(mydata.filtered)
pdta$numberOfToolsDevelopedPerYear(mydata.filtered)
pdta$createToolsDevelopedPerYearBarChart(mydata.filtered)

pdta$getTableOfFreqTopToolsUsagePerYear(mydata.filtered,topTools)
pdta$getTableMinAndMaxYearUsingATool(mydata.filtered,topTools)
pdta$numberOfArticlesComparingTools(mydata.filtered)
pdta$numberOfArticlesOnlyComparingTools(mydata.filtered)
pdta$numberOfArticlesComparingToolsAndDeveopingNewTool(mydata.filtered)
pdta$numberOfArticlesComparingToolsAndDeveopingNewToolOrUsingTool(mydata.filtered)
pdta$numberOfArticlesComparingToolsAndNotDeveopingNewToolButUsingTools(mydata.filtered)
pdta$numberOfArticlesBenchmarkToolDeveloping(mydata.filtered)

source("mainHeader.R",encoding = "UTF-8")
#algorithm analysis
aasma$numberOfArticlesUseOrDescribeAlgorithm(mydata.filtered.devTool)
aasma$numberOfArticlesUsingWhatKindOfAlgorithm(mydata.filtered.devTool,standardAlgorithmCategorization)
aasma$getTableMinAndMaxYearUsingAlgorithm(mydata.filtered.devTool,standardAlgorithmCategorization)
aasma$numberOfArticlesUsingWhatKindOfAlgorithm(mydata.filtered,newAlgorithmCategorization)
aasma$getTableMinAndMaxYearUsingAlgorithm(mydata.filtered.devTool,newAlgorithmCategorization)
aasma$numberOfArticlesUsingWhatKindOfAlgorithm(mydata.filtered,commonAlgorithmsUsed)
aasma$getTableMinAndMaxYearUsingAlgorithm(mydata.filtered.devTool,commonAlgorithmsUsed)
aasma$numberOfArticlesUsingWhatKindOfAlgorithmAndOtherTypeOfAlgorithm(mydata.filtered.devTool,standardAlgorithmCategorization)
aasma$numberOfArticlesThatUseTokenizationInAlgorithm(mydata.filtered.devTool)
aasma$numberOfArticlesThatUseOtherAlgorithmsAndTypesOfAlgorithms(mydata.filtered.devTool)
aasma$numberOfArticlesUsingWhatSimilarityMeasure(mydata.filtered.devTool, similarityMeasures)
aasma$getTableMinAndMaxYearUsingAlgorithm(mydata.filtered.devTool,similarityMeasures)
aasma$listArticlesThatUseAlgorithms(mydata.filtered.devTool,allAlgorithms)

source("mainHeader.R",encoding = "UTF-8")
#compariosn measure analyis
cma$numberOfArticlesUsingComparisonMetric(mydata.filtered.compTools,"Fbeta")
cma$numberofArticlesUsingTopcomparisonMetrics(mydata.filtered.devTool,topMetrics)
cma$numberofArticlesUsingTopcomparisonMetrics(mydata.filtered.compTools,topMetrics)
cma$numberOfMetricsUsedPerYear(mydata.filtered.compTools,topMetrics)
cma$getMinAndMaxYearUsedByMetric(mydata.filtered.compTools,topMetrics)
cma$numberOfArticlesUsingPrecisionAndRecallCombinations(mydata.filtered.compTools)
cma$numberOfArticlesUsingSomeOtherMessure(mydata.filtered.compTools)

source("mainHeader.R",encoding = "UTF-8")
#Article dataset analysis
adsa$numberOfArticlesUsingDatasetType(mydata.filtered)
adsa$numberOfArticlesUsingDatasetType(mydata.filtered.devTool)
adsa$numberOfArticlesUsingDatasetType(mydata.filtered.ducTool)
adsa$numberOfArticlesUsingDatasetType(mydata.filtered.compTools)

adsa$numberOfArticlesUsingDatasetTypeGrouped(mydata.filtered,usedDatasetTypes)
adsa$numberOfArticlesUsingDatasetTypeGrouped(mydata.filtered.ducTool,usedDatasetTypes)
adsa$numberOfArticlesUsingDatasetTypeGrouped(mydata.filtered.devTool,usedDatasetTypes)
adsa$numberOfArticlesUsingDatasetTypeGrouped(mydata.filtered.compTools,usedDatasetTypes)
adsa$numberOfArticlesUsingDatasetsTypeByDifferentArticlesIncluded(mydata.filtered,usedDatasetTypes,c("ToolDeveloped","ToolUsedOrDeveloped","ToolCompariosonOrOnlyComparison"))
adsa$createBarChartNumberOfArticlesPerDatasetTypeAndDifferentArticlesIncluded(mydata.filtered,usedDatasetTypes,c("ToolDeveloped","ToolUsedOrDeveloped","ToolCompariosonOrOnlyComparison"))

adsa$numberOfArticlesUsingHowManyCoursesForTest(mydata.filtered)
adsa$numberOfArticlesUsingHowManyCoursesForTest(mydata.filtered.devTool)
adsa$numberOfArticlesUsingHowManyCoursesForTest(mydata.filtered.compToolsAndPersonalDataset)

adsa$numberOfArticlesUsingHowManyAssignementsInTotal(mydata.filtered)
adsa$numberOfArticlesUsingHowManyAssignementsInTotal(mydata.filtered.compToolsAndPersonalDataset)

adsa$numberOfArticlesUsingHowManyTotalSubmissions(mydata.filtered)
adsa$numberOfArticlesUsingHowManyTotalSubmissions(mydata.filtered.compToolsAndPersonalDataset)
adsa$numberOfArticlesUsingHowManyTotalSubmissionsGrouped(mydata.filtered)
adsa$numberOfArticlesUsingHowManyTotalSubmissionsGrouped(mydata.filtered.compToolsAndPersonalDataset)

adsa$numberOfAriclesUsingPlagDetectionOnWhichLevel(mydata.filtered.compToolsAndPersonalDataset,academicLevels)#maybe not the best selection becouse other is not quite clear
adsa$numberOfAriclesUsingPlagDetectionOnWhichLevelPerMaxProgramLength(mydata.filtered,academicLevels)
adsa$numberOfArticlesUsingDatasetsCommingFromWhichInstitution(mydata.filtered)
adsa$numberOfArticlesUsingDatasetsCommingFromWhichInstitution(mydata.filtered.compToolsAndPersonalDataset)

source("mainHeader.R",encoding = "UTF-8")
#programming language analysis
adsa$numberOfAriclesUsingPWhichProgrammingLanguage(mydata.filtered.ducTool,programmingLanguages)
adsa$numberOfArticlesUsingWhichProgrammingLanguagePerYear(mydata.filtered.ducTool,top10ProgrammingLanguages)
adsa$numberOfArticlesUsingWhichProgrammingLanguageMinMaxYear(mydata.filtered.ducTool,top10ProgrammingLanguages)

adsa$numberOfArticlesUsingMAXProgramLength(mydata.filtered.compTools)#up to so many LOC 0-100, 0-200, etc.
adsa$numberOfAriclesUsingPWhichProgrammingLanguagePerMAXProgramLength(mydata.filtered.ducTool,top10ProgrammingLanguages)

source("mainHeader.R",encoding = "UTF-8")
#Obfuscation methods
oappma$numberOfArticlesMentioningObfucationMethods(mydata.filtered)
oappma$numberOfArticlesUsingWhatKindOfObfuscationMethod(mydata.filtered,obfuscationMethods)
oappma$numberOfArticlesUsingWhatKindOfObfuscationMethodPerYear(mydata.filtered,obfuscationMethods)
oappma$createMentioingOfObfuscationMethodPerYearBarChart(mydata.filtered,obfuscationMethods)
oappma$createMentioingOfObfuscationMethodBarChart(mydata.filtered,obfuscationMethods)
oappma$numberOfArticlesUsingWhatKindOfOBMethodtehniqueIncombinationWithOtherOBMehtod(mydata.filtered.ducTool,obfuscationMethods)

#ako radim svoje PP tehn ike info o koje OB metode se spominju zajedno može pomoći da se se stavi što kombinirati koje PP
#preprocessing tehiques analyiss
oappma$numberOfArticlesUsingPPtechniques(mydata.filtered)
oappma$numberOfArticlsUsingWhatKindOfPPtehnique(mydata.filtered,ppTechniques)
oappma$numberOfArticlesUsingWhatKindOfPPtehniqueIncombinationWithOtherTehniques(mydata.filtered,ppTechniques)

source("mainHeader.R",encoding = "UTF-8")
mydata.filtered[mydata.filtered$AnalysisID==7,c("AnalysisID","ArticleName","Year")]
