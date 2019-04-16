source("RFun_DataPrep.R")
source("RFun_Scraping.R")

## Now read OUR db
system.time(db <- ReadData("Data/dbtml.csv", davis=TRUE))
dbl <- SummaryData(db)

## In "OUR" db many of these inconsistencies have been fixed
SearchByPlayer("Roger Federer", data=dbl)


## Most tourney wins
wins <- db[round=="F", .N, by=.(winner_name)]
setorder(wins, -N)
wins[1:20,]
OutputTableToPng(wins[1:20,], "MostWins.png")



### clay tournaments for Almagro
alma <- db[surface=="Clay" & (winner_name=="Nicolas Almagro" | loser_name=="Nicolas Almagro"),  .(matches_played=.N) , by=.(tourney_name, year)]
OutputTableToPng(alma, "ClayTournamentsAlmagro.png")

### find all clay tournaments of a player
AllTourney <- function(name, tab){
     tab[surface=="Clay" & (winner_name==name | loser_name==name),  .(matches_played=.N) , by=.(tourney_name, year)]
}

### find who played most clay tourneys 
tut <- lapply(dbl$players, AllTourney, db)
res <- sapply(tut, nrow)
tab <- cbind(Player=dbl$players[order(res, decreasing=TRUE)], N=res[order(res, decreasing=TRUE)] )
OutputTableToPng(tab[1:20,], "MostClay.png")

### Clay matches of Almagro
sres <- sapply(tut, function(x) sum(x$matches_played))
grep("Almagro", dbl$players) ## 2879
sres[2879]


### Clay matches of Vilas
grep("Vilas", dbl$players) ## 1467
tut[[1467]]
OutputTableToPng(tut[[1467]], "VilasClay.png")

wins <- db[round=="F", .N, by=.(winner_id)]
db[,  .SD[.N], by=.(tourney_id)]
db[,  .N, by=.(winner_id)]
db[,  .N, by=.(winner_name)]



## how many bagels did John Isner suffer?
scores_win <- db[winner_name=="John Isner", .(year, tourney_name, winner_name, loser_name, score)]
w <- grep("0-6", scores_win$score, fixed=TRUE)
scores_los <- db[loser_name=="John Isner", .(year, tourney_name, winner_name, loser_name, score)]
l <- grep("6-0", scores_los$score, fixed=TRUE)
tbagel <- rbind(scores_win[w,], scores_los[l,])
nrow(tbagel)

## how many bagels did John Isner inflict?
scores_win <- db[winner_name=="John Isner", .(year, tourney_name, winner_name, loser_name, score)]
w <- grep("6-0", scores_win$score, fixed=TRUE)
scores_los <- db[loser_name=="John Isner", .(year, tourney_name, winner_name, loser_name, score)]
l <- grep("0-6", scores_los$score, fixed=TRUE)
tbagel <- rbind(scores_win[w,], scores_los[l,])
nrow(tbagel)

## Finali in straight sets vinte nei 1000
fm <- db[tourney_level=="M" & round=="F", .(year, tourney_name, winner_name, loser_name, score, best_of)]
fm <- fm[lengths(regmatches(fm$score, gregexpr("-", fm$score)))==(fm$best_of+1)/2,]
table <- fm[year>1999, .(year, tourney_name, winner_name, loser_name, score, best_of)]
library(gridExtra)
cairo_pdf("table_straight_sets.pdf", height=40, width=10)
grid.table(as.data.frame(table))
dev.off()


### Most recurrent score for Federer's wwins
wins <- db[winner_name=="Roger Federer", .N, by=.(score, surface)] 
setorder(wins, -N)
wins[N>5,]

## how many bagels did Mackenzie McDonald suffer?
scores_win <- db[winner_name=="Mackenzie Mcdonald", .(year, tourney_name, surface, round, winner_name, loser_name, score)]
w <- grep("0-6", scores_win$score, fixed=TRUE)
scores_los <- db[loser_name=="Mackenzie Mcdonald", .(year, tourney_name, surface, round, winner_name, loser_name, score)]
l <- grep("6-0", scores_los$score, fixed=TRUE)
tbagel <- rbind(scores_win[w,], scores_los[l,])
nrow(tbagel)
OutputTableToPng(tbagel, "MacBagelLos.png")
## how many bagels did Mac inflict?
scores_win <- db[winner_name=="Mackenzie Mcdonald", .(year, tourney_name, surface, round, winner_name, loser_name, score)]
w <- grep("6-0", scores_win$score, fixed=TRUE)
scores_los <- db[loser_name=="Mackenzie Mcdonald", .(year, tourney_name, surface, round, winner_name, loser_name, score)]
l <- grep("0-6", scores_los$score, fixed=TRUE)
tbagel <- rbind(scores_win[w,], scores_los[l,])
nrow(tbagel)
OutputTableToPng(tbagel, "MacBagelWn.png")
 
 
#### h2h of two players: FEDAL
fedal <- h2h("Roger Federer", "Rafael Nadal", db)
dcast(fedal[, .N, by=.(winner_name, surface)], winner_name ~ surface, value.var="N")
fedal[, .N, by=.(winner_name)]

#### inquiry all matches of a given player
roge <- PlayerMatches("Roger Federer", db)
rafa <- PlayerMatches("Rafael Nadal", db)
nole <- PlayerMatches("Novak Djokovic", db)

### Match ended with WO or retirement, for roge, rafa and nole
roge[grep("W/O|RET", roge$score),.N, by=.(winner_name)]
rafa[grep("W/O|RET", rafa$score),.N, by=.(winner_name)]
nole[grep("W/O|RET", nole$score),.N, by=.(winner_name)]

## Nole Rafa
nadalovic <- h2h("Novak Djokovic", "Rafael Nadal", db)
dcast(nadalovic[, .N, by=.(winner_name, surface)], winner_name ~ surface, value.var="N")
 
 
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

system.time(roger_top <- SearchByPlayer("Roger Federer", data=db))
## serial version, ~11 sec
system.time(tot_top <- lapply(db$players, SearchByPlayer, data=db))
names(tot_top) <- db$players

SearchByPlayer("Roger Federer", data=dbl, tournament="Tour Finals")
tep <- sapply(tot_top, function(x) sum(x$N))
tep <- sort(tep, decreasing=TRUE)

head(tep)

 
## punti ATP medi negli slam:Fe
losses <- db[tourney_level=="G" & (loser_name=="Roger Federer" ), ]
wins <- db[tourney_level=="G" & (winner_name=="Roger Federer" )&round=="F",  ]

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
losses <- db[tourney_level=="G" & (loser_name=="Rafael Nadal" ), ]
wins <- db[tourney_level=="G" & (winner_name=="Rafael Nadal" )&round=="F",  ]

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
losses <- db[tourney_level=="G" & (loser_name=="Novak Djokovic" ), ]
wins <- db[tourney_level=="G" & (winner_name=="Novak Djokovic" )&round=="F",  ]

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


