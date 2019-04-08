source("RFun_DataPrep.R")
source("RFun_ScrapingATP.R")

system.time(dbsack <- PrepareDataSackmann(dir="~/ATP/SackmanGit/tennis_atp", pattern="atp_matches_[[:digit:]]{4}.csv"))
system.time(dbtop <- PrepareDataTop())
roger_for_sack <- SearchByPlayer("Roger Federer", data=dbsack)
roger_for_top  <- SearchByPlayer("Roger Federer", data=dbtop)

tt <- dbtop$all_data
ss <- dbsack$all_data
tt[tourney_level=="A", .(tourney_name, year)]
tt[tourney_level=="F", .(tourney_name, year)]


tt[tourney_level=="G" & winner_name=="Roger Federer", .N, .(tourney_name, year)]
tt[tourney_level=="G" & loser_name=="Roger Federer", .(tourney_name, year)]
tt[tourney_level=="G" & round=="F", .(tourney_name, year, winner_name)]

aa <- as.data.frame(tt[tourney_name %in% c("London", "Tour Finals"), .(tourney_name, year, tourney_level)])

tt[tourney_name=="Roland Garros" & (winner_name=="Rafael Nadal" | loser_name == "Rafael Nadal"), .(year, loser_name, score)]

scores <- tt[tourney_name=="Roland Garros" & (winner_name=="Rafael Nadal" | loser_name == "Rafael Nadal"), .(year, loser_name, score)]
scores[sapply(strsplit(scores$score, " "), length)==5,]

tt[tourney_name=="London", .(tourney_name, year, tourney_level)]

## how many bagels did John Isner suffer?
scores_win <- tt[winner_name=="John Isner", .(year, tourney_name, winner_name, loser_name, score)]
w <- grep("0-6", scores_win$score, fixed=TRUE)
scores_los <- tt[loser_name=="John Isner", .(year, tourney_name, winner_name, loser_name, score)]
l <- grep("6-0", scores_los$score, fixed=TRUE)
tbagel <- rbind(scores_win[w,], scores_los[l,])
nrow(tbagel)

## how many bagels did John Isner inflict?
scores_win <- tt[winner_name=="John Isner", .(year, tourney_name, winner_name, loser_name, score)]
w <- grep("6-0", scores_win$score, fixed=TRUE)
scores_los <- tt[loser_name=="John Isner", .(year, tourney_name, winner_name, loser_name, score)]
l <- grep("0-6", scores_los$score, fixed=TRUE)
tbagel <- rbind(scores_win[w,], scores_los[l,])
nrow(tbagel)

## Finali in straight sets vinte nei 1000
fm <- tt[tourney_level=="M" & round=="F", .(year, tourney_name, winner_name, loser_name, score, best_of)]
fm <- fm[lengths(regmatches(fm$score, gregexpr("-", fm$score)))==(fm$best_of+1)/2,]
table <- fm[year>1999, .(year, tourney_name, winner_name, loser_name, score, best_of)]
library(gridExtra)
cairo_pdf("table_straight_sets.pdf", height=40, width=10)
grid.table(as.data.frame(table))
dev.off()


### Most recurrent score for Federer
wins <- tt[winner_name=="Roger Federer", .N, by=.(score, surface)] 
setorder(wins, -N)
 
wins[N>5,]

### Bagels Mc

## how many bagels did Mackenzie McDonald suffer?
scores_win <- tt[winner_name=="Mackenzie Mcdonald", .(year, tourney_name, surface, round, winner_name, loser_name, score)]
w <- grep("0-6", scores_win$score, fixed=TRUE)
scores_los <- tt[loser_name=="Mackenzie Mcdonald", .(year, tourney_name, surface, round, winner_name, loser_name, score)]
l <- grep("6-0", scores_los$score, fixed=TRUE)
tbagel <- rbind(scores_win[w,], scores_los[l,])
nrow(tbagel)
OutputTableToPng(tbagel, "MacBagelLos.png")
## how many bagels did Mac inflict?
scores_win <- tt[winner_name=="Mackenzie Mcdonald", .(year, tourney_name, surface, round, winner_name, loser_name, score)]
w <- grep("6-0", scores_win$score, fixed=TRUE)
scores_los <- tt[loser_name=="Mackenzie Mcdonald", .(year, tourney_name, surface, round, winner_name, loser_name, score)]
l <- grep("0-6", scores_los$score, fixed=TRUE)
tbagel <- rbind(scores_win[w,], scores_los[l,])
nrow(tbagel)
OutputTableToPng(tbagel, "MacBagelWn.png")
 
 

fedal <- h2h("Roger Federer", "Rafael Nadal", tt)
fedal[, .N, by=.(winner_name, surface)]
fedal[, .N, by=.(winner_name)]

roge <- PlayerMatches("Roger Federer", tt)
rafa <- PlayerMatches("Rafael Nadal", tt)
nole <- PlayerMatches("Novak Djokovic", tt)


roge[grep("W/O|RET", roge$score),.N, by=.(winner_name)]
rafa[grep("W/O|RET", rafa$score),.N, by=.(winner_name)]
nole[grep("W/O|RET", nole$score),.N, by=.(winner_name)]

 
 
 
grep("Tour Finals", tt$tourney_name)
tt[grep("^Tour Finals", tt$tourney_name),]
## search ALL tournaments played by ALL players in the database

## parallel version, Linux; 7 cores, ~1.1 sec
system.time(tot <- parallel::mclapply(dbsack$players, SearchByPlayer, data=dbsack, mc.cores=7))
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

SearchByPlayer("Roger Federer", data=dbtop, tour="Tour Finals")
tep <- sapply(tot_top, function(x) sum(x$N))
tep <- sort(tep, decreasing=TRUE)

head(tep)

 
## punti ATP medi negli slam:Fe
losses <- tt[tourney_level=="G" & (loser_name=="Roger Federer" ), ]
wins <- tt[tourney_level=="G" & (winner_name=="Roger Federer" )&round=="F",  ]

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
losses <- tt[tourney_level=="G" & (loser_name=="Rafael Nadal" ), ]
wins <- tt[tourney_level=="G" & (winner_name=="Rafael Nadal" )&round=="F",  ]

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
losses <- tt[tourney_level=="G" & (loser_name=="Novak Djokovic" ), ]
wins <- tt[tourney_level=="G" & (winner_name=="Novak Djokovic" )&round=="F",  ]

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
