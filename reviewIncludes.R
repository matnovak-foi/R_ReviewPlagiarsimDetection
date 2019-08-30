library(testthat)
library(UtilityFunctions)
test_package("UtilityFunctions")

listmHelp <- ListManipulator$new()
dfmHelp <- DataFrameMaipulator$new()
ftcHelp <- FrequencyTableCreator$new()
graphBuilderHelp <- GraphBuilderGGplot$new()
lmHelp <- ListManipulator$new()

source("analysis/articlesPerYearAnalysis.R", encoding = "UTF-8")
apya <- ArticlePerYearAnalysis$new()

source("analysis/pdToolsAnalysis.R", encoding = "UTF-8")
pdta <- PlagiarismDetectionToolsAnalysis$new()

source("analysis/comparisonMetricsAnalysis.R", encoding = "UTF-8")
cma <- CompariosnMetricsAnalysis$new()

source("analysis/articleTypesAnalysis.R", encoding = "UTF-8")
ata <- ArticleTypesAnalysis$new()

source("analysis/articleDatasetAnalysis.R", encoding = "UTF-8")
adsa <- ArticleDatasetAnalysis$new()

source("analysis/OBandPPMethodsAnalysis.R", encoding = "UTF-8")
oappma <- OBandPPMethodsAnalysis$new()

source("analysis/algorithmsAndSimilarityMeasuresAnalysis.R", encoding = "UTF-8")
aasma <- AlgorithmsAndSimilarityMeasuresAnalysis$new()

library(tm)
library(wordcloud)
library(RColorBrewer)
source("analysis/wordAnalysis.R", encoding = "UTF-8")
wa <- WordAnalysis$new()

test_results <- test_dir("analysis/", reporter="summary")