source("RFun_DataPrep.R")
source("RFun_Scraping.R")


## Read data from csv files of Sackmann (tennis_atp git must be cloned locally
system.time(dbsack <- ReadDataSackmann(dir="~/ATP/SackmanGit/tennis_atp", pattern="atp_matches_[[:digit:]]{4}.csv"))
## Perform some basic manips of the database: all present players, all tournaments, 
## and a big table with one row per player per tournament per year
dbsacklist <- SummaryData(dbsack)

## Now read OUR db
system.time(dbtop <- ReadData("Data/20190410-DataTop.csv", davis=TRUE))
dbtoplist  <- SummaryData(dbtop)

## In Sackmann data many tourneys have names written in different spellings
roger_for_sack <- SearchByPlayer("Roger Federer", data=dbsacklist)

## In "OUR" db many of these inconsistencies have been fixed
roger_for_top  <- SearchByPlayer("Roger Federer", data=dbtoplist)

## dbtop[tourney_level=="G" & winner_name=="Roger Federer", .N, .(tourney_name, year)]
## dbtop[tourney_level=="G" & loser_name=="Roger Federer", .(tourney_name, year)]
## dbtop[tourney_level=="G" & round=="F", .(tourney_name, year, winner_name)]
## 
## aa <- as.data.frame(dbtop[tourney_name %in% c("London", "Tour Finals"), .(tourney_name, year, tourney_level)])
## 
## dbtop[tourney_name=="Roland Garros" & (winner_name=="Rafael Nadal" | loser_name == "Rafael Nadal"), .(year, loser_name, score)]
## 
## scores <- dbtop[tourney_name=="Roland Garros" & (winner_name=="Rafael Nadal" | loser_name == "Rafael Nadal"), .(year, loser_name, score)]
## scores[sapply(strsplit(scores$score, " "), length)==5,]
## 
## dbtop[tourney_name=="London", .(tourney_name, year, tourney_level)]

## how many bagels did John Isner suffer?
scores_win <- dbtop[winner_name=="John Isner", .(year, tourney_name, winner_name, loser_name, score)]
w <- grep("0-6", scores_win$score, fixed=TRUE)
scores_los <- dbtop[loser_name=="John Isner", .(year, tourney_name, winner_name, loser_name, score)]
l <- grep("6-0", scores_los$score, fixed=TRUE)
tbagel <- rbind(scores_win[w,], scores_los[l,])
nrow(tbagel)

## how many bagels did John Isner inflict?
scores_win <- dbtop[winner_name=="John Isner", .(year, tourney_name, winner_name, loser_name, score)]
w <- grep("6-0", scores_win$score, fixed=TRUE)
scores_los <- dbtop[loser_name=="John Isner", .(year, tourney_name, winner_name, loser_name, score)]
l <- grep("0-6", scores_los$score, fixed=TRUE)
tbagel <- rbind(scores_win[w,], scores_los[l,])
nrow(tbagel)

## Finali in straight sets vinte nei 1000
fm <- dbtop[tourney_level=="M" & round=="F", .(year, tourney_name, winner_name, loser_name, score, best_of)]
fm <- fm[lengths(regmatches(fm$score, gregexpr("-", fm$score)))==(fm$best_of+1)/2,]
table <- fm[year>1999, .(year, tourney_name, winner_name, loser_name, score, best_of)]
library(gridExtra)
cairo_pdf("table_straight_sets.pdf", height=40, width=10)
grid.table(as.data.frame(table))
dev.off()


### Most recurrent score for Federer's wwins
wins <- dbtop[winner_name=="Roger Federer", .N, by=.(score, surface)] 
setorder(wins, -N)
wins[N>5,]

## how many bagels did Mackenzie McDonald suffer?
scores_win <- dbtop[winner_name=="Mackenzie Mcdonald", .(year, tourney_name, surface, round, winner_name, loser_name, score)]
w <- grep("0-6", scores_win$score, fixed=TRUE)
scores_los <- dbtop[loser_name=="Mackenzie Mcdonald", .(year, tourney_name, surface, round, winner_name, loser_name, score)]
l <- grep("6-0", scores_los$score, fixed=TRUE)
tbagel <- rbind(scores_win[w,], scores_los[l,])
nrow(tbagel)
OutputTableToPng(tbagel, "MacBagelLos.png")
## how many bagels did Mac inflict?
scores_win <- dbtop[winner_name=="Mackenzie Mcdonald", .(year, tourney_name, surface, round, winner_name, loser_name, score)]
w <- grep("6-0", scores_win$score, fixed=TRUE)
scores_los <- dbtop[loser_name=="Mackenzie Mcdonald", .(year, tourney_name, surface, round, winner_name, loser_name, score)]
l <- grep("0-6", scores_los$score, fixed=TRUE)
tbagel <- rbind(scores_win[w,], scores_los[l,])
nrow(tbagel)
OutputTableToPng(tbagel, "MacBagelWn.png")
 
 
#### h2h of two players: FEDAL
fedal <- h2h("Roger Federer", "Rafael Nadal", dbtop)
fedal[, .N, by=.(winner_name, surface)]
fedal[, .N, by=.(winner_name)]

#### inquiry all matches of a given player
roge <- PlayerMatches("Roger Federer", dbtop)
rafa <- PlayerMatches("Rafael Nadal", dbtop)
nole <- PlayerMatches("Novak Djokovic", dbtop)

### Match ended with WO or retirement, for roge, rafa and nole
roge[grep("W/O|RET", roge$score),.N, by=.(winner_name)]
rafa[grep("W/O|RET", rafa$score),.N, by=.(winner_name)]
nole[grep("W/O|RET", nole$score),.N, by=.(winner_name)]

 
 
## search ALL tournaments played by ALL players in the database

## parallel version, Linux; 7 cores, ~1.1 sec
system.time(tot <- parallel::mclapply(dbsacklist$players, SearchByPlayer, data=dbsacklist, mc.cores=7))
## serial version, ~11 sec
system.time(tot2 <- lapply(dbsack$players, SearchByPlayer, data=dbsack))
names(tot) <- names(tot2) <- dbsack$players
all.equal(tot, tot2) ## TRUE

## most total entries 
te <- sapply(tot, function(x) sum(x$N))
te <- sort(te, decreasing=TRUE)

head(te)
##  Fabrice Santoro   Feliciano Lopez   Mikhail Youzhny     Jimmy Connors      David Ferrer Fernando Verdasco 
##              443               424               420               390               388               384 

system.time(roger_top <- SearchByPlayer("Roger Federer", data=dbtop))
## serial version, ~11 sec
system.time(tot_top <- lapply(dbtop$players, SearchByPlayer, data=dbtop))
names(tot_top) <- dbtop$players

SearchByPlayer("Roger Federer", data=dbtoplist, tournament="Tour Finals")
tep <- sapply(tot_top, function(x) sum(x$N))
tep <- sort(tep, decreasing=TRUE)

head(tep)

 
## punti ATP medi negli slam:Fe
losses <- dbtop[tourney_level=="G" & (loser_name=="Roger Federer" ), ]
wins <- dbtop[tourney_level=="G" & (winner_name=="Roger Federer" )&round=="F",  ]

points_ <- c(R128=10, R64=45, R32=90, R16=180, QF=360, SF=720, F=1200, W=2000)
##points_ <- c(R128=0, R64=1, R32=2, R16=3, QF=4, SF=5, F=6, W=7)

losses[, points:=points_[match(losses$round , names(points_))] ]
wins[,points:=points_[length(points_)] ]
tots <- rbind(losses, wins)
 tots[ , .(points=mean(points))] 
res <- tots[ , .(points=mean(points), .N ), by=.(tourney_name)]
setorder(res, -points)
res
OutputTableToPng(res, "average_points_slam_fed.png")

res2 <- tots[ , .(points=mean(points), .N ), by=.(surface)]
setorder(res2, -points)
OutputTableToPng(res2, "average_points_slam_surf_fed.png")



## punti ATP medi negli slam:Nadal
losses <- dbtop[tourney_level=="G" & (loser_name=="Rafael Nadal" ), ]
wins <- dbtop[tourney_level=="G" & (winner_name=="Rafael Nadal" )&round=="F",  ]

## points <- c(R128=10, R64=45, R32=90, R16=180, QF=360, SF=720, F=1200, W=2000)

 losses[, points:=points_[match(losses$round , names(points_))] ]
wins[,points:=points_[length(points_)] ]
tots <- rbind(losses, wins)
 tots[ , .(points=mean(points))] 

res <- tots[ , .(points=mean(points), .N ), by=.(tourney_name)]
setorder(res, -points)
OutputTableToPng(res, "average_points_slam_rafa.png")

res2 <- tots[ , .(points=mean(points), .N ), by=.(surface)]
setorder(res2, -points)
OutputTableToPng(res2, "average_points_slam_surf_rafa.png")




## punti ATP medi negli slam: Nole
losses <- dbtop[tourney_level=="G" & (loser_name=="Novak Djokovic" ), ]
wins <- dbtop[tourney_level=="G" & (winner_name=="Novak Djokovic" )&round=="F",  ]

points <- c(R128=10, R64=45, R32=90, R16=180, QF=360, SF=720, F=1200, W=2000)

points[match(losses$round , names(points))]
losses[, points:=points[match(losses$round , names(points))] ]
wins[,points:=2000]
tots <- rbind(losses, wins)
 tots[ , .(points=mean(points))] 

res <- tots[ , .(points=mean(points), .N ), by=.(tourney_name)]
setorder(res, -points)
OutputTableToPng(res, "average_points_slam_nole.png")

res2 <- tots[ , .(points=mean(points), .N ), by=.(surface)]
setorder(res2, -points)
OutputTableToPng(res2, "average_points_slam_surf_nole.png")


### clay tournaments for Almagro
alma <- dbtop[surface=="Clay" & (winner_name=="Nicolas Almagro" | loser_name=="Nicolas Almagro"),  .(matches_played=.N) , by=.(tourney_name, year)]
OutputTableToPng(alma, "ClayTournamentsAlmagro.png")

### find all clay tournaments of a player
AllTourney <- function(name, tab){
     tab[surface=="Clay" & (winner_name==name | loser_name==name),  .(matches_played=.N) , by=.(tourney_name, year)]
}

### find who played most clay tourneys 
tut <- lapply(dbtoplist$players, AllTourney, dbtop)
res <- sapply(tut, nrow)
tab <- cbind(Player=dbtoplist$players[order(res, decreasing=TRUE)], N=res[order(res, decreasing=TRUE)] )
OutputTableToPng(tab[1:20,], "MostClay.png")

### Clay matches of Almagro
sres <- sapply(tut, function(x) sum(x$matches_played))
grep("Almagro", dbtoplist$players) ## 2879
sres[2879]


### Clay matches of Vilas
grep("Vilas", dbtoplist$players) ## 1467
tut[[1467]]
OutputTableToPng(tut[[1467]], "VilasClay.png")

## Most tourney wins
wins <- dbtop[round=="F", .N, by=.(winner_name)]
setorder(wins, -N)
wins[1:20,]
OutputTableToPng(wins[1:20,], "MostWins.png")


## largest distance between 2 wins 
source("RFun_DataPrep.R")
source("RFun_Scraping.R")
library(parallel)

## Now read OUR db
system.time(dbtop <- ReadData("Data/20190410-DataTop.csv", davis=FALSE))
dbtoplist  <- SummaryData(dbtop)

allids <- sort(unique(dbtop$winner_id))

winsbyid <- mclapply(allids, PlayerWonMatchesById, db=dbtop, mc.cores=4)

DaysDiff <- function(string1, string2){
   as.integer(as.Date(as.character(string1), format='%Y%m%d')-as.Date(as.character(string2),format='%Y%m%d'))
}
   
FindConsecutiveWins <- function(tab, id) {
    if (nrow(tab) < 2) 
        return(NA)

    setorder(tab, tourney_date)
    wins <- tab[winner_id==id]
    if (nrow(wins) < 2) 
        return(NA)
    
    ff <- DaysDiff(wins$tourney_date[seq(2, nrow(wins))],  wins$tourney_date[seq(1, nrow(wins)-1)] )
    ret <- data.table(cbind(
            wins[seq(2, nrow(wins)), .(winner_name, winner_id, tourney_name, tourney_date, year, loser_name, loser_id, surface)], 
            wins[seq(1, nrow(wins)-1), .(tourney_name, tourney_date, year, loser_name, loser_id, surface)],
            span=ff))
    return(ret)
}
 
fff <- mclapply(seq_along(winsbyid), function(x) FindConsecutiveWins(winsbyid[[x]], allids[x]), mc.cores=4)

rmind <- which(is.na(fff))
fff <- fff[-rmind]
ffl <- rbindlist(fff)
setorder(ffl, -span)
## pl <- dbtoplist$players[-rmind]

out <- ffl[1:20,] ##c("winner_name", "loser_name", "winner_id", "tourney_name", "year", "since_last_w")
colnames(out) <- c("Player", "Id", "Tourney 1", "Date 1", "Year 1", "Opponent 1", "Id 1", "Surface 1", "Tourney 2", "Date 2", "Year 2", "Opponent 2", "Id 2", "Surface 2", "Span [d]")
OutputTableToPng(out, "tentative_largest_span3.png")

which(allids==104548)

aa <- lapply(fff, function(x) ifelse(is.na(x), c(NA, NA), c(which.max(x$since_last_w ), max(x$since_last_w))))


###### database fixes

## Alexander Zverev
dbtop[winner_id==100644,]
dbtop[winner_name=="Alexander Zverev",]

dbtop[winner_name=="Alexander Zverev" & is.na(winner_id), "winner_id"] <- 100644


dbtop[year<1986 & (winner_name=="Alexander Volkov" | loser_name == "Alexander Volkov"), ]
dbtop[winner_name=="Alexander Zverev Sr", "winner_id"] <- 1006441
dbtop[loser_name=="Alexander Zverev Sr", "loser_id"] <- 1006441

dbtop[(winner_name=="Alexander Volkov Sr"), "winner_id"]
dbtop[(loser_name=="Alexander Volkov Sr"), "loser_id"]


dbtop <- ReadData("Data/20190410-DataTop.csv", davis=F)

dbtop[winner_name=="Gustavo Guerrero" & year < 1979, "winner_id"] <- 1006301
dbtop[loser_name=="Gustavo Guerrero" & year < 1979, "loser_id"] <- 1006301

dbtop[winner_id== 1006301, "winner_name"] <- "Gustavo Guerrero 2"
dbtop[loser_id== 1006301, "loser_name"] <- "Gustavo Guerrero 2"
gg <- PlayerMatches("Gustavo Guerrero", dbtop)

fwrite(dbtop, "Data/20190410-DataTop.csv")

