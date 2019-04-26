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

ids <- sapply(res_pl, function(x) x$id)

## alternatively, in parallel:
## res_pl <- mclapply(players, ScrapePlayer, db=dd, mc.cores=4)
## names(res_pl) <- players


Age <- function(player, date, scrape){
    fmt_db <- '%Y%m%d'
    fmt_bday <- '%Y.%m.%d'
    res <- scrape[[player]]
    days <- as.Date(as.character(date), format=fmt_db)-as.Date(res$bday,format=fmt_bday)
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
    
    ranktab <- res$table
    if (is.na(ranktab$Date)) {
        cat(":: No ranking data for player ", name, "\n")
        return(rep(NA_integer_, length(tourney_date)))
    }
    ## revert the whole ranking tab
    ranktab <- ranktab[seq(nrow(ranktab),1),]
    
    rankdate <- as.Date(ranktab$Date,format=fmt_bday)
    tdate <- as.Date(as.character(tourney_date), format=fmt_db)
    ##cat ("Rank: len(tdate):", length(tdate),"\n")
    a <- rep(NA_integer_, length(tdate))
    ## a will store the index raw of the ranking tab immediately before the tourney_date
    ## we need to track 0s and NAs!
    for (i in seq_along(tdate))
        a[i] <- which.max(rankdate > tdate[i])-1

    rtab <- c(ranktab$Singles,NA_integer_)
    if (0 %in% a) {
        cat("0 %in% a!!\n")
        a[a==0] <- length(rtab)
    }

    if (any(is.na(a))) {
        cat("NA %in% a!!\n")
        a[is.na(a)] <- length(rtab)
    }
    rank <- rtab[a]
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
    hand <- res$plays
    new <- db2[winner_name==name, winner_hand:=hand]
    new <- new[loser_name==name, loser_hand:=hand]
    return(new)
}

AddNationCheckId <- function(name, db) {
    res <- res_pl[[name]]
    nation <- res$nation
    new <- db[winner_name==name, winner_ioc:=res$nation]
    new <- new[loser_name==name, loser_ioc:=res$nation]
    if (!is.na(res$other_id)) {
        new <- new[winner_name==name, winner_id:=res$id]
        new <- new[loser_name==name, loser_id:=res$id]
    }
    return(new)
}



## load functions
source("RFun_DataPrep.R")
source("RFun_Scraping.R")

db <- ReadData("Data/dbtml.csv")
## db <- db[year>1972 & !round%in%c("Q1","Q2","Q3","Q4")]
dbl <- SummaryData(db)

cdb <- copy(db)
for (i in seq_along(players)) {
    cdb <- AddAge(name=players[i], db=cdb)
    cdb <- AddRank(name=players[i], db2=cdb)
    cdb <- AddHand(name=players[i], db2=cdb)
    cdb <- AddNationCheckId(name=players[i], db=cdb)
    cat(":: Player:", players[i], "done\n")
}
name <- "Anders Jarryd"
tourney_date <- res_pl[[name]]$date

cdb <- AddRank(name=players[i], db2=cdb)

setcolorder(cdb, c("year","date","tourney_name","tourney_id","surface",     
                   "indoor","commitment","draw_size","round","winner_id",   
                   "winner_seed","winner_rank","winner_age","winner_ioc", "winner_name","winner_hand",
                   "score","loser_id","loser_seed", "loser_rank", "loser_age", 
                   "loser_ioc", "loser_name","loser_hand","w_ace","w_df","w_svpt","w_1stIn",
                   "w_1stWon","w_2ndWon","w_SvGms","w_bpSaved","w_bpFaced",   
                   "l_ace", "l_df","l_svpt","l_1stIn","l_1stWon",    
                   "l_2ndWon","l_SvGms","l_bpSaved","l_bpFaced","minutes"))

fwrite(cdb, "Data/dbtml.csv", quote=FALSE)



### fix ScrapeYear for 1975
y75 <- ScrapeYear(1975)

aa <- fread("all_tourneys_in_atp_db_until_today.csv")
aa[year==1975]
row_idnx <- aa[,.I[year== 1975L]] ## Retrieve row number
set(aa, row_idnx, names(aa), as.list(y75))
fwrite(aa, "all_tourneys_in_atp_db_until_today.csv", quote=FALSE)


## Update tourney_list
aa <- fread("all_tourneys_in_atp_db_until_today.csv")
row_idnx <- aa[,.I[year== 2019]] ## Retrieve row number

current_year <- ScrapeYear(2019)
new <- rbindlist(list(aa[-row_idnx], current_year), fill=TRUE)

fwrite(new, "all_tourneys_in_atp_db_until_today.csv", quote=FALSE)
