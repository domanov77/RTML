## load functions
source("RFun_DataPrep.R")
source("RFun_Scraping.R")

db <- ReadData("Data/dbtml.csv")
db <- db[year>1972 & !round%in%c("Q1","Q2","Q3","Q4")]
dbl <- SummaryData(db)

################ Player info
    
players <- db[, .N, by=winner_name][order(-N), winner_name]

fed <- ScrapePlayer(name=players[1], db=db)

## name <- "Calvin Hemery"
## ScrapePlayer(name="Calvin Hemery", db=db)
## ScrapePlayer(id="KI74", db=db)


pl <- grep("unknown", players, ignore.case=TRUE)
pl

players <- players[-pl]

res_pl <- vector(mode="list", length=length(players))
for (i in seq_along(players)) {
    res_pl[[i]] <- ScrapePlayer(name=players[i], db=db)
    saveRDS(res_pl[[i]], file=sprintf("ranks/chunk_%04d.RData", i))
    cat(":Dumped", players[i], " or ", i,"/", length(players), ", ", res_pl[[i]]$name,"\n")
}

names(res_pl) <- players

save(list=c("res_pl"), file="Scrape_PlayerRankings.Rdata")

ids <- sapply(res_pl, function(x) x[nrow(x),3])

## alternatively, in parallel:
## res_pl <- mclapply(players, ScrapePlayer, db=dd, mc.cores=4)
## names(res_pl) <- players


Age <- function(player, date, scrape){
    fmt_db <- '%Y%m%d'
    fmt_bday <- '%Y.%m.%d'
    res <- scrape[[player]]
    days <- as.Date(as.character(date), format=fmt_db)-as.Date(res[nrow(res),2],format=fmt_bday)
    age <- round(as.numeric(days)/365.25,6)
    return(age)
}


AddAge <- function(name, db) {
    ##Age("John McEnroe", date="19850205", res_pl)
    datesw <- db[winner_name==name, date]
    agew <- Age(name, datesw, res_pl)
    datesl <- db[loser_name==name, date]
    agel <- Age(name, datesl, res_pl)
    new <- db[loser_name==name, loser_age:=agel]
    new <- new[winner_name==name, winner_age:=agew]
    return(new)
}

Rank <- function(name, tourney_date) {
    fmt_db <- '%Y%m%d'
    fmt_bday <- '%Y.%m.%d'
    res <- res_pl[[name]]
    ## eliminate last row
    res[nrow(res),] <- c(1970101,"5000U","5000U")
    ## revert the whole ranking matrix
    res <- res[seq(nrow(res),1),]
    
    rankdate <- as.Date(res$Date,format=fmt_bday)
    tdate <- as.Date(as.character(tourney_date), format=fmt_db)
    ##cat ("Rank: len(tdate):", length(tdate),"\n")
    a <<- rep(nrow(res), length(tdate))
    for (i in seq_along(tdate))
        a[i] <<- which.max(rankdate > tdate[i])-1
    if (0 %in% a)
        cat("0 %in% a!!\n")
    if (any(is.na(a)))
        cat("NA %in% a!!\n")
    rank <- res[a, 2]
    return(rank)
}


AddRank <- function(name, db2) {
    datesw <- db2[winner_name==name, date]
    rank_w <- Rank(name, datesw)
    datesl <- db2[loser_name==name, date]
    rank_l <- Rank(name, datesl)
    new <- db2[winner_name==name, winner_rank:=rank_w]
    new <- new[loser_name==name, loser_rank:=rank_l]
    return(new)
}

AddHand <- function(name, db2) {
    res <- res_pl[[name]]
    hand <- res[nrow(res), 1]
    new <- db2[winner_name==name, winner_hand:=hand]
    new <- new[loser_name==name, loser_hand:=hand]
    return(new)
}

cdb <- copy(db)
for (i in seq_along(players)) {
    cdb <- AddAge(name=players[i], db=cdb)
    cdb <- AddRank(name=players[i], db2=cdb)
    cdb <- AddHand(name=players[i], db2=cdb)
    cat(":: Player:", players[i], "done\n")
}


setcolorder(cdb, c("year","date","tourney_name","tourney_id","surface",     
                   "indoor","commitment","draw_size","round","winner_id",   
                   "winner_seed","winner_rank","winner_age","winner_name","winner_hand",
                   "score","loser_id","loser_seed", "loser_rank", "loser_age", 
                   "loser_name","loser_hand","w_ace","w_df","w_svpt","w_1stIn",
                   "w_1stWon","w_2ndWon","w_SvGms","w_bpSaved","w_bpFaced",   
                   "l_ace", "l_df","l_svpt","l_1stIn","l_1stWon",    
                   "l_2ndWon","l_SvGms","l_bpSaved","l_bpFaced","minutes"))

fwrite(cdb, "Data/dbtml.csv", quote=FALSE)
