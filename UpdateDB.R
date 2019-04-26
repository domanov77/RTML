## load functions
source("RFun_DataPrep.R")
source("RFun_Scraping.R")

db <- ReadData("Data/dbtml.csv", davis=FALSE, quali=TRUE, current=TRUE)


db <- UpdateDB(db, write_ended=TRUE, write_current=TRUE)
