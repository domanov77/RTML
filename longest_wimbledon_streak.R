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

OutputTableToPng(wimb[1:30,], "ConsecutiveWimbledonApparitions.png")

halle <- ConsecutivePresences("Halle", db)
halle[1:30]


ConsecutivePresences2 <- function(tourney, db) {
    ## consider all the years for the given tournament
    all_tourn <- db[tourney_name %in% tourney]
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
UO2 <- ConsecutivePresences2(c("US Open","US Championships"), db)
OutputTableToPng(UO2[1:30,], "ConsecutiveUSOpen.png")
OutputTableToPng(UO[1:30,], "ConsecutiveUSOpen_openera.png")



AO2 <- ConsecutivePresences2(c("Australian Open","Australasian Championships"), db)
OutputTableToPng(AO2[1:30,], "ConsecutiveAO.png")

RG <-  ConsecutivePresences2(c("Roland Garros","French Championships"), db)

OutputTableToPng(RG[1:30,], "ConsecutiveRG.png")


fwrite(AO2, "LongestConsecutivePresenceAO.csv")
fwrite(RG, "LongestConsecutivePresenceRG.csv")
fwrite(wimb, "LongestConsecutivePresenceW.csv")
fwrite(UO2, "LongestConsecutivePresenceUO.csv")


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

