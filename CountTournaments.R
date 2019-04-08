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
fm <- tt[tourney_level=="M" & round=="F", .(year, tourney_name, winner_name, loser_name, score)]
tfm <- fm[sapply(strsplit(fm$score, " "), length)==2,]
tfm[year>1999,]


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


### scraping tournament results from ATP website
## Paris 2017: https://www.atptour.com/en/scores/archive/paris/352/2017/results
res_paris_2017 <- ScrapeTourneyFromATP("https://www.atptour.com/en/scores/archive/paris/352/2017/results", id="Paris Masters")

atp2015 <- ScrapeYearATP(year=2015)
atp2016 <- ScrapeYearATP(year=2016)
AllRes2016 <- lapply(atp2016$url, ScrapeTourneyFromATP)

tots <- vector(mode="list", length=length(atp2016$url))
for (i in seq_along(atp2016$url)) {
    tots[[i]] <- ScrapeTourneyFromATP(atp2016$url[i])
    cat(paste(":: ", i, ")", atp2016$tourney_name[i], "[done] \n"))
}
atp_matches_2016 <- data.table(rbindlist(tots))

atp_matches_2016_2 <- data.table(rbindlist(AllRes2016))
all.equal(atp_matches_2016, atp_matches_2016_2)


