################### largest distance between 2 wins 
source("RFun_DataPrep.R")
source("RFun_Scraping.R")

## read OUR db
## dbtml <- UpdateDB(write_ended=TRUE, write_current=TRUE, save_html=TRUE)

system.time(dbtml <- ReadData(quali=FALSE))

YearsOfPresenceByPlayerName <- function(name, alltou) sort(unique(as.numeric(alltou[winner_name==name | loser_name==name, .(year)]$year)))

LongestConsecutiveSeq <- function(vec) { 
    temp <- cumsum(c(1, diff(vec) - 1))
    temp2 <- rle(temp)
    a <- vec[which(temp == with(temp2, values[which.max(lengths)]))]
    return(list(n=length(a), years=a))
}

ConsecutivePresences <- function(tourney, min_round, no2019=TRUE, db=dbtml) {
    ## consider all the years for the given tournament
    all_tourn <- db[(tourney_name %in% tourney)]

    ## optionally, don#t consider 2019
    if (no2019)
        all_tourn <- all_tourn[year < 2019]
        
    ## optionally, restrict to a given round
    if (!missing(min_round)) {
        ## consider only from the given round on
        all_tourn <- all_tourn[round==min_round]
    }
    ## find all players who appear (either winner or loser)
    allplayers <- sort(unique(c(all_tourn$winner_name,all_tourn$loser_name)))

    ## find all the years where each players appears
    presence <- lapply(allplayers, YearsOfPresenceByPlayerName, all_tourn)
    names(presence) <- allplayers
    ## check the consecutive streak
    streak <- lapply(presence, LongestConsecutiveSeq)

    ## length of the streak
    nstreak <- sapply(streak, function(x) x$n)
    ## start and end
    years   <- as.data.frame(t(sapply(streak, function(x) range(x$years))))
    colnames(years) <- c("First", "Last")
    
    ## all together
    tab <- data.table(Name=names(nstreak), ConsPres=nstreak, First=years$First, Last=years$Last, row.names = NULL)
    setorder(tab, -ConsPres, -Last)
    return(tab)
}



## Consecutive presences in the slams MAIN DRAWS
AO <- ConsecutivePresences(c("Australian Open","Australasian Championships"), no2019=FALSE, db=dbtml)
RG <- ConsecutivePresences(c("Roland Garros","French Championships"), no2019=FALSE, db=dbtml)
WI <- ConsecutivePresences("Wimbledon", no2019=FALSE, db=dbtml) 
UO <- ConsecutivePresences(c("US Open","US Championships"), db=dbtml)

## pictures
OutputTableToPng(AO[1:30,], "Consecutive_AO.png")
OutputTableToPng(RG[1:30,], "Consecutive_RG.png")
OutputTableToPng(WI[1:30,], "Consecutive_WI.png")
OutputTableToPng(UO[1:30,], "Consecutive_UO.png")
## csvs
fwrite(AO, "Consecutive_AO.csv")
fwrite(RG, "Consecutive_RG.csv")
fwrite(WI, "Consecutive_WI.csv")
fwrite(UO, "Consecutive_UO.csv")

## Restrict to reaching the R64 (second) round 
WI_R64 <- ConsecutivePresences("Wimbledon", min_round="R64", no2019=TRUE, db=dbtml) ## no2019=TRUE is needed here to make sure partial updates don't mess with the records
OutputTableToPng(WI_R64[1:30,], "Consecutive_WI_R64.png")
fwrite(WI_R64, "Consecutive_WI_R64.csv")

WI_R32 <- ConsecutivePresences("Wimbledon", min_round="R32", no2019=TRUE, db=dbtml)
WI_R16 <- ConsecutivePresences("Wimbledon", min_round="R16", no2019=TRUE, db=dbtml)
WI_QF <- ConsecutivePresences("Wimbledon", min_round="QF", no2019=TRUE, db=dbtml)
WI_SF <- ConsecutivePresences("Wimbledon", min_round="SF", no2019=TRUE, db=dbtml)
WI_F <- ConsecutivePresences("Wimbledon", min_round="F", no2019=TRUE, db=dbtml)
OutputTableToPng(WI_R16[1:30,], "Consecutive_WI_R16.png")
OutputTableToPng(WI_QF[1:30,], "Consecutive_WI_QF.png")
OutputTableToPng(WI_SF[1:30,], "Consecutive_WI_SF.png")
OutputTableToPng(WI_F[ConsPres>1], "Consecutive_WI_F.png")
fwrite(WI_R16, "Consecutive_WI_R16.csv")
fwrite(WI_QF, "Consecutive_WI_QF.csv")
fwrite(WI_SF, "Consecutive_WI_SF.csv")
fwrite(WI_F, "Consecutive_WI_F.csv")

## How many wimbledons are in the db
unique(dbtml[tourney_name=="Wimbledon",year])

## Try other tournaments
halle <- ConsecutivePresences("Halle", no2019=FALSE, db=dbtml)
halle[1:30]



###### old code
## nstreak <- sapply(streak, function(x) x$n)
## years   <- as.data.frame(t(sapply(streak, function(x) range(x$years))))
## colnames(years) <- c("Begin", "End")
## tab <- data.table(Name=names(nstreak), ConsPres=nstreak, Begin=years$Begin, End=years$End, row.names = NULL)
## setorder(tab, -ConsPres)
## 
##     
## wimb <- db[tourney_name=="Wimbledon"]
## 
## YearsPresenceByName <- function(name) sort(unique(as.numeric(wimb[winner_name==name | loser_name==name, .(year)]$year)))
## 
## allplayers <- sort(unique(wimb$winner_name))
## presence <- lapply(allplayers, YearsPresenceByName)
## names(presence) <- allplayers
## 
## 
## vec <- presence[[2123]]
## 
## LongestConsecutive <- function(vec) { 
##     temp <- cumsum(c(1, diff(vec) - 1))
##     temp2 <- rle(temp)
##     a <- vec[which(temp == with(temp2, values[which.max(lengths)]))]
##     return(list(n=length(a), years=a))
## }
## 
## streak <- mclapply(presence, LongestConsecutive, mc.cores=4)
## 
## nstreak <- sapply(streak, function(x) x$n)
## years   <- as.data.frame(t(sapply(streak, function(x) range(x$years))))
## colnames(years) <- c("Begin", "End")
## tab <- data.table(Name=names(nstreak), ConsPres=nstreak, Begin=years$Begin, End=years$End, row.names = NULL)
## setorder(tab, -ConsPres)
## 
## OutputTableToPng(tab[1:20,], "ConsecutiveWimbledonApparitions.png")

db <- dbtml


DefeatedFinalists <- function(player, finalists, tdb) {
    allmatches <- tdb[winner_name==player, .(loser_name)]
    ## res <-  sum(allmatches$loser_name %in% finalists)
    inds <-  which(allmatches$loser_name %in% finalists)
    defeated <- allmatches$loser_name[inds]
    ## cat(paste(player, "defeated" , res, "finalists in ", tourney, unique(tdb$year) ))
    return(list(player=player, defs=defeated))
}

PreviousWinners <- function(tourney, y, win=FALSE, n, db=dbtml) {
    ## list of all participants in that tourney in that year; we start from 3R because we seek for at least 3 wins against former finalists or winners
    a <- db[tourney_name%in%tourney & year==y & round=="R32",.(winner_name, loser_name)]
    allplayers <- unique(c(a$winner_name, a$loser_name))
    
    ## Find out all the finalists/winners in that tourney until the previous year!
    fin  <- db[tourney_name%in%tourney & round=="F" & year < y, .(winner_name, loser_name, year)]
    if (win) { ## we look for former winners
        allfin <- sort(unique(c(fin$winner_name)))
    } else {
        allfin <- sort(unique(c(fin$winner_name, fin$loser_name)))
    }
    
    ## take just all matches of this year's tourney
    tdb <- db[tourney_name%in%tourney & year==y]
    
    ## look which of those players defeated former finalists
    res <- lapply(allplayers, DefeatedFinalists, finalists=allfin, tdb=tdb)
    
    ## strip the results from the unsignificant ones
    inds <- which(sapply(res, function(x) length(x$defs))>n)
    ret <- res[inds]

    return(ret)
}
 
## Function to print nicely the results
PrintThree <- function(res) {
    if (length(res)==0) {
        cat("No results\n")
    } else {
        y <- names(res)
        tourney <- attr(res, "tourney")
        cat(paste(tourney, collapse=" or "),":\n")
        for (i in seq_along(y)) {
            cat(paste(y[i], ": ", res[[i]][[1]]$player, " defs. ", paste(res[[i]][[1]]$defs, collapse=", "),"\n"))
        }
    }
}


## Checks who won against n or more former winners or finalists of a tourney in a particular editions
ThreeFinalists <- function(tourney, winners=FALSE, n=2, print=TRUE, db=dbtml) { 
    ## all available years of tourney in the db
    allyears <- unique(db[tourney_name%in%tourney, .(year)]$year)
    
    ## Checks for a given year the wins of each participants against former winners or finalists
    tots <- lapply(allyears, function(x) PreviousWinners(tourney=tourney, y=x, win=winners, n=n))
    names(tots) <- allyears
    
    inds <- which(sapply(tots, length) > 0)
    ret <- tots[inds]
    attr(ret, "tourney") <- tourney
    if (print)
        PrintThree(ret)
    return(ret)
}
    
rAOw <- ThreeFinalists(c("Australasian Championships","Australian Open"), n=2, winners=TRUE)
rAOf <- ThreeFinalists(c("Australasian Championships","Australian Open"), n=2, winners=FALSE)

rRGw <- ThreeFinalists(c("French Championships", "Roland Garros"), n=2, winners=TRUE)
rRGf <- ThreeFinalists(c("French Championships", "Roland Garros"), n=2, winners=FALSE)

rWIw <- ThreeFinalists("Wimbledon", winners=TRUE)
rWIf <- ThreeFinalists("Wimbledon", winners=FALSE)

rUOw <- ThreeFinalists(c("US Championships", "US Open"), winners=TRUE)
rUOf <- ThreeFinalists(c("US Championships", "US Open"), winners=FALSE)
