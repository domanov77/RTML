source("RFun_DataPrep.R")
source("RFun_Scraping.R")

system.time(dbtop <- ReadData("Data/20190410-DataTop.csv", davis=TRUE))
dbtoplist  <- SummaryData(dbtop)

wins <- dbtop[, .N, by=.(winner_name, winner_id)]
loss <- dbtop[, .N, by=.(loser_name, loser_id)]

tt <- rbindlist(list(wins, loss), use.names=FALSE)[,1:2]
a <- tt[, .SD, by=.(winner_id)]
a[N>1,]

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


