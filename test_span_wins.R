################### largest distance between 2 wins 
source("RFun_DataPrep.R")
source("RFun_Scraping.R")
library(parallel)

## Now read OUR db
system.time(db <- ReadData("Data/dbtml.csv", davis=FALSE))
dbl <- SummaryData(db)

allids <- sort(unique(db$winner_id))
winsbyid <- mclapply(allids, PlayerWonMatchesById, db=db, mc.cores=4)

DaysDiff <- function(string1, string2){
   as.integer(as.Date(as.character(string1), format='%Y%m%d')-as.Date(as.character(string2),format='%Y%m%d'))
}
   
FindConsecutiveWins <- function(tab, id) {
    if (nrow(tab) < 2) 
        return(NA)

    setorder(tab, date)
    wins <- tab[winner_id==id]
    if (nrow(wins) < 2) 
        return(NA)
    
    ff <- DaysDiff(wins$date[seq(2, nrow(wins))],  wins$date[seq(1, nrow(wins)-1)] )
    ret <- data.table(cbind(
            wins[seq(2, nrow(wins)), .(winner_name, winner_id, tourney_name, date, year, loser_name, loser_id, surface)], 
            wins[seq(1, nrow(wins)-1), .(tourney_name, date, year, loser_name, loser_id, surface)],
            span=ff))
    return(ret)
}
 
fff <- mclapply(seq_along(winsbyid), function(x) FindConsecutiveWins(winsbyid[[x]], allids[x]), mc.cores=4)

rmind <- which(is.na(fff))
fff <- fff[-rmind]
ffl <- rbindlist(fff)
setorder(ffl, -span)

out <- ffl[1:20,] 
colnames(out) <- c("Player", "Id", "Tourney 1", "Date 1", "Year 1", "Opponent 1", "Id 1", "Surface 1", "Tourney 2", "Date 2", "Year 2", "Opponent 2", "Id 2", "Surface 2", "Span [d]")
OutputTableToPng(out, "tentative_largest_span3.png")


############ database fixes (here it is incomprehensible and old

## ## Alexander Zverev
## dbtop[winner_id==100644,]
## dbtop[winner_name=="Alexander Zverev",]
## 
## dbtop[winner_name=="Alexander Zverev" & is.na(winner_id), "winner_id"] <- 100644
## 
## 
## dbtop[year<1986 & (winner_name=="Alexander Volkov" | loser_name == "Alexander Volkov"), ]
## dbtop[winner_name=="Alexander Zverev Sr", "winner_id"] <- 1006441
## dbtop[loser_name=="Alexander Zverev Sr", "loser_id"] <- 1006441
## 
## dbtop[(winner_name=="Alexander Volkov Sr"), "winner_id"]
## dbtop[(loser_name=="Alexander Volkov Sr"), "loser_id"]
## 
## 
## dbtop <- ReadData("Data/20190410-DataTop.csv", davis=F)
## 
## dbtop[winner_name=="Gustavo Guerrero" & year < 1979, "winner_id"] <- 1006301
## dbtop[loser_name=="Gustavo Guerrero" & year < 1979, "loser_id"] <- 1006301
## 
## dbtop[winner_id== 1006301, "winner_name"] <- "Gustavo Guerrero 2"
## dbtop[loser_id== 1006301, "loser_name"] <- "Gustavo Guerrero 2"
## gg <- PlayerMatches("Gustavo Guerrero", dbtop)
## 
## fwrite(dbtop, "Data/20190410-DataTop.csv")
## 
