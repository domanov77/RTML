################### Wins and Losses in Masters 1000 tourneys
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

## remove the walkovers
dbm <- dbm[score!="W/O",]

## wins
wins   <- dbm[,.N, by=winner_name]
## losses
losses <- dbm[,.N, by= loser_name]

## common name to merge with
names(wins)[1] <- names(losses)[1] <- "name"
names(wins)[2] <-  "wins"
names(losses)[2] <-  "losses"

## merge the tables by "name"
res <- merge(wins, losses, by = c("name"), all=TRUE)

## get rid of NAs, have 0 instead
res[is.na(res)] <- 0

## sum the wins and losses into a new column played
res <- res[, played:=wins+losses]
## order by decreasing total matches
setorder(res, -played)

res

OutputTableToPng(head(res,20), "MatchesPlayedInMasters.png")



### Older code

### here including walkovers
out <- dbm[,.N,by=winner_name]
setorder(out, -N, na.last=FALSE)
head(out)

### here excluding walkovers
out_nowo <- dbm[score!="W/O",.N,by=winner_name]
setorder(out_nowo, -N, na.last=FALSE)
head(out_nowo)

aa <- dbm[round=="F", year, by=tourney_name]
setorder(aa, year)

OutputTableToPng(head(out,20), "MatchesWonInMasters.png")
OutputTableToPng(head(out_nowo,20), "MatchesWonInMasters_noWO.png")
OutputTableToPng(aa[seq(  1,134),], "ConsideredMasters_1.png")
OutputTableToPng(aa[seq(135,268),], "ConsideredMasters_2.png")


out_nwl <- dbm[score!="W/O",.N,by=.(winner_name, loser_name)]

