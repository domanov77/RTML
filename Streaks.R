library(data.table)

## Function to load Top's db
LoadTMLdb <- function(basedir=".", current=TRUE, davis=FALSE) {
    lf <- list.files(path=basedir, pattern=".csv", full.names=TRUE)
    if ((!current) & (paste0(basedir,"/ongoing_tourneys.csv") %in% lf)) {
        ind <- match(paste0(basedir,"/ongoing_tourneys.csv"), lf)
        lf <- lf[-ind]
    }
    tots <- lapply(lf, data.table::fread, fill=TRUE)
    db <- rbindlist(tots)

    if (!davis) {
        db  <- db[tourney_level!="D", ]
    }
    return(db)
}

## Select all matches of one given player (by name)
PlayerMatches <- function(name, db) {
    db[winner_name==name | loser_name==name, ]
}

## Function to find the longest streak in a sequence
LongestSequence <- function(vec, target=TRUE) {
    if (!target %in% vec)
        return(list(n=0, pos=0))
    tmp <- rle(vec)
    inds_of_target <- which(tmp$values==target)
    nmax <- max(tmp$lengths[inds_of_target])
    ind_max_target <- which(tmp$lengths[inds_of_target] == nmax)
    ## nstreak <- length(ind_max_target)
    ## if (nstreak > 1) cat("Warning: longest streak of ",nmax," repeats ", nstreak, "times. Taking the first one!\n")

    imax <- which(tmp$lengths==nmax & tmp$values==target)[1]
    ##    cat("imax =", imax, "; nstreak=", nstreak, "; nmax=", nmax, "\n")
    posinvec <- sum(tmp$lengths[seq(1,imax-1)])+1
    return(list(n=nmax, pos=posinvec))
}

## workhorse function to be iterated per player name
ExtractStreak <- function(name, tab){
    matches <- PlayerMatches(name, tab)
    ## encode the wins as TRUE
    enc <- matches$winner_name == name
    ## find the FIRST occurrence of the single longest win streak
    streak <- LongestSequence(enc, target=TRUE)
    return(list(length=streak$n, matches=matches[c(streak$pos,streak$pos+streak$n), ]))
}



StreakBySurface <- function(db, surf) {
    ## first we restrict the db to the given surface
    sdb <- db[surface==surf, ]
    
    ## find all names in this sdb
    all_names <- sort(unique(c(sdb$winner_name, sdb$loser_name)))

    ## iterate on each name to find the single longest streak
    all_streak <- lapply(all_names, ExtractStreak, tab=sdb)
    names(all_streak) <- all_names
    return(all_streak)
}




## Load db (change basedir if needed, i.e. ".")
db <- LoadTMLdb(basedir="TML-Database", davis=TRUE, current=FALSE)


## on grass
grass_str <- StreakBySurface(db, surf="Grass")
tot_grass <- sapply(grass_str, "[[", "length")
tot_grass[order(tot_grass, decreasing=TRUE)[1:10] ]

## on clay
clay_str <- StreakBySurface(db, surf="Clay")
tot_clay <- sapply(clay_str, "[[", "length")
tot_clay[order(tot_clay, decreasing=TRUE)[1:10] ]


## on hard
hard_str <- StreakBySurface(db, surf="Hard")
tot_hard <- sapply(hard_str, "[[", "length")
tot_hard[order(tot_hard, decreasing=TRUE)[1:10] ]







############# For Debugging
## surf <- "Grass"
## sdb <- db[surface==surf, ]
    
## ## find all names in this sdb
## all_names <- sort(unique(c(sdb$winner_name, sdb$loser_name)))

## rr <- vector(mode="list", length=length(all_names))
## for (i in seq_along(all_names)) {
##     cat(":: Doing ", all_names[i], "... ")
##     rr[[i]] <- ExtractStreak(all_names[i], tab=sdb)
##     cat("[OK]\n")
## }

## name <- "Aaron Krickstein"
## tab <- sdb
## matches <- PlayerMatches(name, tab)
## ## encode the wins as TRUE
## enc <- matches$winner_name == name
## ## find the FIRST occurrence of the single longest win streak
## streak <- LongestSequence(enc, target=TRUE)
