WordAnalysis <- setRefClass("WordAnalysis", 
                            
 fields = list(
   TITLES = "character",
   KEYWORDS = "character",
   ABSTRACTS = "character",
   AUTHOR.SURNAMES = "character",
   wordsDF = "ANY",
   sortedTermMatrix = "ANY",
   dfm="ANY"
 ),
 
 methods = list(
   initialize = function() {
     TITLES <<- readChar("analysis/data/final 150 - titles.txt",file.info("analysis/data/final 150 - titles.txt")$size)
     TITLES <<- gsub("\r\n", " ", TITLES)
     KEYWORDS <<- readChar("analysis/data/final 150 - keywords.txt",file.info("analysis/data/final 150 - keywords.txt")$size)
     KEYWORDS <<- gsub("\r\n", " ", KEYWORDS)
     ABSTRACTS <<- readChar("analysis/data/final 150 - abstract.txt",file.info("analysis/data/final 150 - abstract.txt")$size)
     ABSTRACTS <<- gsub("\r\n", " ", ABSTRACTS)
     AUTHOR.SURNAMES <<- readChar("analysis/data/final 150 - author-surnames.txt",file.info("analysis/data/final 150 - author-surnames.txt")$size)
     AUTHOR.SURNAMES <<- gsub("\r\n", " ", AUTHOR.SURNAMES)
     dfm <<- DataFrameMaipulator$new()
   },
   
   getTopAuthors = function(){
     corpus<-VCorpus(VectorSource(AUTHOR.SURNAMES))
     corpus<-tm_map(corpus,content_transformer(tolower))
     corpus<-tm_map(corpus,removeWords,stopwords("english"))
     tdm<-TermDocumentMatrix(corpus)
     v<-sort(rowSums(as.matrix(tdm)),decreasing=FALSE)
     myNames<-names(v)
     return(data.frame(word=myNames,freq=v))
   },
   
   createWordCloude = function(minCount,wordsToRemove=c("")){
     collection<-c(TITLES,KEYWORDS,ABSTRACTS)
     corpus<-VCorpus(VectorSource(collection))
     corpus<-tm_map(corpus,content_transformer(tolower))
     corpus<-tm_map(corpus,removeWords,stopwords("english"))
     corpus<-tm_map(corpus,removeWords,wordsToRemove)
     #corpus<-tm_map(corpus,removePunctuation)
     #corpus<-tm_map(corpus,removeNumbers)
     tdm<-TermDocumentMatrix(corpus)
     sortedRows<-sort(rowSums(as.matrix(tdm)),decreasing=TRUE)
     sortedTermMatrix <<- sortedRows
     myWords<-names(sortedRows)
     wordsDF<<-data.frame(word=myWords,freq=sortedRows)
     wordcloud(wordsDF$word,wordsDF$freq,min.freq=minCount)
   },
   
   colorWordCloude = function(minCount){
     pal <- brewer.pal(6,"Dark2")
     pal <- pal[-(1)]
     wordcloud(wordsDF$word,wordsDF$freq,min.freq=minCount,random.color=TRUE,colors=pal)
   }
 )                             
)