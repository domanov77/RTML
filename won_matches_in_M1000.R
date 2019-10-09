################### largest distance between 2 wins
source("RFun_DataPrep.R")
source("RFun_Scraping.R")

## Now read OUR db
system.time(db <- ReadData("Data/dbtml.csv", davis=FALSE, quali=FALSE))


## dbtml now should be consistent
masters <- c("ATP Masters 1000 Canada",
             "ATP Masters 1000 Cincinnati",
             "ATP Masters 1000 Essen",
             "ATP Masters 1000 Hamburg",
             "ATP Masters 1000 Indian Wells",
             "ATP Masters 1000 Madrid",
             "ATP Masters 1000 Miami",
             "ATP Masters 1000 Monte Carlo",
             "ATP Masters 1000 Paris",
             "ATP Masters 1000 Rome",
             "ATP Masters 1000 Shanghai",
             "ATP Masters 1000 Stockholm",
             "ATP Masters 1000 Stuttgart")


dbm <- db[tourney_name %in% masters,]
out <- dbm[,.N,by=winner_name]
setorder(out, -N, na.last=FALSE)
head(out)

aa <- dbm[round=="F", year, by=tourney_name]
setorder(aa, year)

OutputTableToPng(head(out,20), "MatchesWonInMasters.png")
OutputTableToPng(aa, "ConsideredMasters.png")
