################### largest distance between 2 wins 
source("RFun_DataPrep.R")
source("RFun_Scraping.R")
library(parallel)

## read OUR db
system.time(db <- ReadData())

YearsPresenceByName <- function(name, alltou) sort(unique(as.numeric(alltou[winner_name==name | loser_name==name, .(year)]$year)))

LongestConsecutive <- function(vec) { 
    temp <- cumsum(c(1, diff(vec) - 1))
    temp2 <- rle(temp)
    a <- vec[which(temp == with(temp2, values[which.max(lengths)]))]
    return(list(n=length(a), years=a))
}

ConsecutivePresences <- function(tourney, db) {
    ## consider all the years for the given tournament
    all_tourn <- db[tourney_name==tourney]
    ## find all players who won at least one match
    allplayers <- sort(unique(all_tourn$winner_name))
    ## find all the years where each players appears
    presence <- lapply(allplayers, YearsPresenceByName, all_tourn)
    names(presence) <- allplayers
    ## check the consecutive streak
    streak <- lapply(presence, LongestConsecutive)

    ## length of the streak
    nstreak <- sapply(streak, function(x) x$n)
    ## start and end
    years   <- as.data.frame(t(sapply(streak, function(x) range(x$years))))
    colnames(years) <- c("First", "Last")
    
    ## all together
    tab <- data.table(Name=names(nstreak), ConsPres=nstreak, First=years$First, Last=years$Last, row.names = NULL)
    setorder(tab, -ConsPres)
    return(tab)
}

wimb <- ConsecutivePresences("Wimbledon", db)
halle <- ConsecutivePresences("Halle", db)

wimb[1:20]
OutputTableToPng(wimb[1:20,], "ConsecutiveWimbledonApparitions.png")

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

