rm(list = ls())
ls()
metalica<-c("Lars","James","Jason","Kirk")
metalica<-metalica[metalica != "Jason"]
metalica<-c(metalica,"Rob")
metalica<-metalica[metalica != "Jason"]; metalica<-c(metalica,"Rob")
metalica
getwd()
help(count)
metalicaNames<-c("Lars","James","Jason","Kirk")
metalicaAges<-c(47,47,48,46)
metalica<-data.frame(Name = metalicaNames, Age = metalicaAges)
metalica$Name
metalica$Age
metalica$childName<-c(12,12,4,6)
names(metalica)
metalicaList<-list(metalicaAges,metalicaNames)
metalicaList
metalicaBind<-cbind(metalicaNames,metalicaAges)
metalicaBind
metalica$FatherhoodAge<-metalica$Age-metalica$childName
metalica$FatherhoodAge

#-----------Lecturer Data---------------------------------------------------------------------------

name<-c("Ben", "Martin","Andy","Paul", "Graham","Carina","Karina","Doug","Mark", "Zoe")

#Default date format is yyyy-mm-dd
birth_date<-as.Date(c("1977-07-03", "1969-05-24", "1973-06-21", "1970-07-16", "1949-10-10", "1983-11-05", "1987-10-08", "1989-09-16", "1973-05-20", "1984-11-12"))

job<-c(1,1,1,1,1,2,2,2,2,2)
job<-c(rep(1, 5),rep(2, 5))
job<-factor(job, levels = c(1:2), labels = c("Lecturer", "Student"))
job<-gl(2, 5, labels = c("Lecturer", "Student"))

friends<-c(5,2,0,4,1,10,12,15,12, 17)
alcohol<-c(10,15,20,5,30,25,20,16,17,18)
income<-c(20000,40000,35000,22000,50000,5000,100,3000,10000,10)
neurotic<-c(10,17,14,13,21,7,13,9,14,13)

lecturerData<-data.frame(name, birth_date, job, friends, alcohol,income, neurotic)
