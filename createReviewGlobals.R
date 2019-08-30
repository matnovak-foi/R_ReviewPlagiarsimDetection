source("reviewIncludes.R")

#mainProgram
#fileName <- "D:/FOI/Doktorski rad/Plagijati pretraživanja exports/Analiza članaka.xlsx"
#fileName <- "/media/matnovak/HomeData/FOI/Doktorski rad/Plagijati pretraživanja exports/Analiza članaka.xlsx"
fileName <- "ExampleReviewExcel.xlsx"
sheetName <- "All articles - filtered"
mydata <- loadSheetFromXMLFile(fileName,sheetName)
mydata <- mydata[order(mydata$Year,mydata$MyIDFiltered),]

mydata.filtered <- mydata[mydata$ForReview==1 & mydata$Year!="N/A" & is.na(mydata$Manual),]
mydata.filtered$AnalysisID <- 1:nrow(mydata.filtered)
mydata.filtered$AnalysisID <- paste0("A",mydata.filtered$AnalysisID)

mydata.filteredWithManual <- mydata[mydata$ForReview==1 & mydata$Year!="N/A",]
mydata.filteredWithManual$AnalysisID <- 1:nrow(mydata.filteredWithManual)
mydata.filteredWithManual$AnalysisID <- paste0("A",mydata.filteredWithManual$AnalysisID)

mydata.filteredOnlyManualForReview <- mydata[mydata$ForReview==1 & mydata$Year!="N/A" & !is.na(mydata$Manual),]
mydata.filteredOnlyManualForReview$AnalysisID <- 1:nrow(mydata.filteredOnlyManualForReview)
mydata.filteredOnlyManualForReview$AnalysisID <- paste0("A",mydata.filteredOnlyManualForReview$AnalysisID)

mydata.filtered.ducTool <- mydata.filtered[!is.na(mydata.filtered$ToolUsedOrDeveloped),]
mydata.filtered.devTool <- mydata.filtered[!is.na(mydata.filtered$ToolDeveloped),]
mydata.filtered.compTools <- mydata.filtered[!is.na(mydata.filtered$ToolComparison) | !is.na(mydata.filtered$OnlyToolComparisons),]
mydata.filtered.compToolsAndPersonalDataset <- mydata.filtered.compTools[!is.na(mydata.filtered.compTools$PersonalStudentDataset),]

mydata.filtered.topCited <- mydata.filtered[mydata.filtered$ScolarCitationSum>50,]

databases <- c("Scopus","ACM","IEEE","WOS","SD")
topTools <- c("MOSS","jPlag","SherlockWarwick","YAP3","SID","Plague","Plaggie","SIM","SIMfromGrune","Marble","Ottenstein",
              "Donnaldson","Accuse","CodeMatch","PMDsCPD","CCFinder","SherlockSydney")
topMetrics <- c("Fbeta","F1","F2","Precision","Recall","SEPwithHFM","HFM","Sensitivity","Selectivity","CorrelactionCoeficient",
                "ExcessDetections","PerformanceIndex","Speed","Qualitatively")
usedDatasetTypes <- c("PersonalGenOrStudDataset","PersonalStudentDataset","PersonalGeneratedDataset","SOCO","ICPC","OtherDataset")
academicLevels <- c("IntroductionaryProgrammingCourse","UndergraduateLevel","UpperLevel","OtherLevel")
programmingLanguages <- c("SupportsAlsoOthers","All","Java","C","Cpp","Csharp","Pascal","Fortran","PHP","JavaScript","CSS","HTML","PERL","Python","COBOL","ANSI_C","Scheme","R","BASIC","VisualBasic","Haskel","SML","Matlab","Prolog","Lisp","DLV","Miranda","VHDL","APL2","Assembly","UnixShell")
top10ProgrammingLanguages <- c("Java","C","Cpp","Pascal","Python","PHP","Csharp","VisualBasic","Haskel","UnixShell")
ppTechniques <- c("ReorderCode","UpperCaseLettersToLowerCase","NoComments","NoWhiteSpacesAndNewLines","NoProfessorsCode","StringReformat","RemovePartsOfCode","OtherPP")
similarityMeasures <- c("BasicStatistic","CorelationCoeficient","LevensteinDistnce","EuclidianDistance","JaccardCoefficient","OtherSimilarityMetric")
standardAlgorithmCategorization <- c("AttributeCounting","Fingerprint","StringMatching","ParametrizedMathicng","TextBased","StructureBased","Stylistic","Semantic")
newAlgorithmCategorization <- c("nGram","Trees","Graphs","Clustering","HistoryBased","XMLbased","CompiledCodeBased","CompressionBased","Wathermarking")
commonAlgorithmsUsed <- c("OnlyGST","OnlyRKR","RKRwithGST","Winnowing","LatentSemantic")
allAlgorithms <- c(standardAlgorithmCategorization, newAlgorithmCategorization, commonAlgorithmsUsed,similarityMeasures)
obfuscationMethods <- c("OM_04_L","OM_05_L","OM_11_AS","OM_07_S","OM_09_S","OM_10_S","OM_08_S","OM_12_LG","OM_02_L","OM_01_L","OM_12_AS","OM_06_L","OM_15_LG","OM_16_LG","OM_14_LG","OM_03_L")

