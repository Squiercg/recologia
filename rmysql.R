install.packages("RMySQL")

library(RMySQL)

ucscDb <- dbConnect(MySQL(),user="genome",host="genome-mysql.cse.ucsc.edu")
resultado <- dbGetQuery(ucscDb,"show databases;")
head(resultado)
dbDisconnect(ucscDb)


#Connecting to hg19 and listing tables
hg19 <- dbConnect(MySQL(),user="genome", db="hg19",host="genome-mysql.cse.ucsc.edu")
allTables <- dbListTables(hg19)
length(allTables)
allTables[1:5]

#Get dimensions of a specific table
dbListFields(hg19,"affyU133Plus2")
dbGetQuery(hg19, "select count(*) from affyU133Plus2")

#Read from the table
affyData <- dbReadTable(hg19, "affyU133Plus2")
head(affyData)

#Select a specific subset
query <- dbSendQuery(hg19, "select * from affyU133Plus2 where misMatches between 1 and 3")
affyMis <- fetch(query)
affyMis[1:10,1:3]
quantile(affyMis$misMatches)

dbClearResult(query)
dbDisconnect(hg19)
