source("RFun_DataPrep.R")
source("RFun_Scraping.R")

db <- ReadData("Data/dbtml.csv", davis=TRUE)

dbl <- SummaryData(db)

## check how many not unique id
wins <- db[, .N, by=.(winner_name, winner_id)]
loss <- db[, .N, by=.(loser_name, loser_id)]

tt <- rbindlist(list(wins, loss), use.names=FALSE)[,1:2]
colnames(tt) <- c("player", "id")

players <- unique(tt)

ids <- players[ , .N, by=id][order(-N)][N>1]

SearchNameById <- function(id, db) {
    db[ winner_id==id | loser_id==id ]
}

all <- lapply(ids$id, SearchNameById, db=db)
names(all) <- ids$id

PlayerMatches("Wojtek Fibak", db)
a1 <- rbindlist(list(db[ winner_id=="F020" , .(pl=winner_name, id=winner_id)],
                     db[  loser_id=="F020" , .(pl= loser_name, id= loser_id)]))
unique(a1)
### Fix empty tourney_names


db[ (winner_name=="John McEnroe" & winner_id=="F020") | (loser_name=="John McEnroe" & loser_id=="F020")]

## [('Ross Case', 2),
## ('Milan Holecek', 2),
## ('Terry Ryan', 2),
## ('Jaime Fillol', 2),
## ('William Alvarez', 2),
## ('William Brown', 2),
## ('Fred Hemmes', 2),
## ('Greg Perkins', 2),
## ('Tony Bardsley', 2),
## ('Saeed Meer', 2),
## ('Alexander Zverev Sr', 2),
## ('Miloslav Mecir', 2),
## ('Michael Walker', 2),
## ('Bronislaw Lewandowski', 2)]

system.time(dbtop <- ReadData("Data/20190410-DataTop.csv", davis=FALSE))

aa <- dbtop[year<1990,  .SD[1,c("tourney_date", "surface", "draw_size")], by=.(tourney_name, year)]
OutputTableToPng(aa[1:424], "all_tourneys_pre1990_1.png")
OutputTableToPng(aa[425:848], "all_tourneys_pre1990_2.png")
OutputTableToPng(aa[849:1272], "all_tourneys_pre1990_3.png")
OutputTableToPng(aa[1273:1696], "all_tourneys_pre1990_3.png")

fwrite(aa, "all_tourneys_pre1990.csv")


