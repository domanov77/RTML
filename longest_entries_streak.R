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

fin  <- db[tourney_name=="Wimbledon" & round=="F", .(winner_name, loser_name, year)]
allfin <- sort(unique(c(fin$winner_name, fin$loser_name)))

w <- db[tourney_name=="Wimbledon" ,.N ,by=.(year)]

MatchDefeatedFinalists <- function(player, finalists, tdb) {
    allmatches <- tdb[winner_name==player, .(loser_name)]
    res <-  sum(allmatches$loser_name %in% finalists)
    ## cat(paste(player, "defeated" , res, "finalists in ", tourney, unique(tdb$year) ))
    return(res)
}

AllPart <- function(tourney, y, db=dbtml) {
    a <- db[tourney_name%in%tourney & year==y & round=="R32",.(winner_name, loser_name)]
    tot <- unique(c(a$winner_name, a$loser_name))
    
    fin  <- db[tourney_name%in%tourney & round=="F" & year < y, .(winner_name, loser_name, year)]
    allfin <- sort(unique(c(fin$winner_name))) ## , fin$loser_name

    tdb <- db[tourney_name%in%tourney & year==y]
    res <- sapply(tot, MatchDefeatedFinalists, finalists=allfin, tdb=tdb)
    return(res)
}
 

ThreeFinalists <- function(tourney) { 
   allyears <- unique(dbtml[tourney_name%in%tourney, .(year)]$year)
   tots <- lapply(allyears, function(x) AllPart(tourney=tourney, y=x))
   names(tots) <- allyears
   Find <- function(vec) names(vec)[vec>2]
   yy <- sapply(tots, Find)
   inds <- which(sapply(yy, length)>0)
   return(yy[inds])
}
    
    
ThreeFinalists(c("Australasian Championships","Australian Open"))
ThreeFinalists(c("French Championships", "Roland Garros"))
ThreeFinalists("Wimbledon")
ThreeFinalists(c("US Championships", "US Open"))
