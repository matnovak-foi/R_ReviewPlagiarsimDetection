#uČitavanje iz excela primjer 2
library(readxl)
Analiza_clanaka2 <- read_excel("D:/FOI/Doktorski rad/Plagijati pretraživanja exports/Analiza članaka.xlsx", 
                               sheet = "All articles - filtered", skip = 1, trim_ws=TRUE)
#convert form characters to factors
character_vars <- lapply(Analiza_clanaka2, class) == "character"
Analiza_clanaka2[, character_vars] <- lapply(Analiza_clanaka2[, character_vars], as.factor)
mydata <- Analiza_clanaka2
#mydata = read.csv("D:/FOI/Doktorski rad/Plagijati pretraživanja exports/All articles - filtered.csv", header=TRUE, sep=";");
colnames(mydata)
mydata[mydata$year==2015,"year"]
mydata.year_databases <- data.frame(cbind(mydata["Review"],mydata["year"],mydata["Scopus"],mydata["ACM"],mydata["IEEE"],mydata["WOS"],mydata["SD"],mydata["R"]));
#  yearDatabases <- subset(mydata,select=c("ForReview","Year","Scopus","ACM","IEEE","WOS","SD","Manual"))
#mydata.year_databases$sumifs <- sapply(seq_len(nrow(x)), function(i) with(x,sum(Scopus[year >= 2015])))

mydata.year_databases[!is.na(mydata.year_databases$year),]
mydata.year_databases[mydata.year_databases$Review==1,]
mydata.year_databases[mydata.year_databases$Review==1 & mydata.year_databases$year!="N/A",]
mydata.review <- mydata.year_databases[mydata.year_databases$Review==1 & mydata.year_databases$year!="N/A" & is.na(mydata.year_databases$R),]

library(plyr)

#broj ?lanaka po godini
count(mydata.review$year)

mydata.art_per_year <-  count(mydata.review$year)
mydata.art_per_year <-  mydata.art_per_year[mydata.art_per_year$x!="N/A",]
colnames(mydata.art_per_year)[1] <- "year"
colnames(mydata.art_per_year)[2] <- "freq"

xrange <- range(as.numeric(as.character(mydata.art_per_year$x)))
yrange <- range(mydata.art_per_year$freq)

mydata.art_per_year$year2 <- as.character(mydata.art_per_year$year)
apys <- mydata.art_per_year[order(mydata.art_per_year$year2),]

plot(xrange, yrange, type="n", xlab="Years", ylab="Number of Articles")
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted", lwd = par("lwd"), equilogs = TRUE)
title("Articles per year", "")
lines((as.numeric(as.character(mydata.art_per_year$x))), mydata.art_per_year$freq, type="b")

articleInDatabasePerYear <- articleInDatabasePerYear[,!(names(articleInDatabasePerYear) %in% c("Scopus"))]

library(Rcmdr)

#u boji
#colors <- rainbow(1)
#linetype <- c(1)
#plotchar <- seq(18,18+1,1)
#lines((as.numeric(as.character(mydata.art_per_year$x))), mydata.art_per_year$freq, type="b", lwd=1.5, lty=linetype[1], col=colors[1], pch=plotchar[1])



#grid poku?aj ru?no
#for (i in 1:xrange) {
#  ygrid <- c(0,max(mydata.art_per_year$freq))
#  xgrid <- c((as.numeric(as.character(mydata.art_per_year$x)))[i],(as.numeric(as.character(mydata.art_per_year$x)))[i])
#  lines(xgrid,ygrid)
#}
#for (i in 1:yrange) {
#  xgrid <- c(min(as.numeric(as.character(mydata.art_per_year$x))),max(as.numeric(as.character(mydata.art_per_year$x))))
#  ygrid <- c(mydata.art_per_year$freq[i],mydata.art_per_year$freq[i])
#  lines(xgrid,ygrid)
#}

#Primjer kreiranja grafa
#with(mydata.art_per_year, lineplot(Year, freq))

#Primjer učitavanja iz excela
#TestExcel2 <-  readXL("D:/FOI/Doktorski rad/Plagijati pretraživanja exports/Analiza članaka.xlsx", 
#        rownames=FALSE, header=TRUE, na="", sheet="All articles - filtered", 
#         stringsAsFactors=TRUE)
#View(Analiza_clanaka)
df = data.frame(Name="A",Value="B")
df[,"Name"]
