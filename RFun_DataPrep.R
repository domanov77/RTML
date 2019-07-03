### Functions to interrogate the databases of matches

### Time-stamp: "Last modified 2019-07-03 14:08:00 delucia"
library(data.table)


## Read OUR data and return a data.table
ReadData <- function(file="Data/dbtml.csv", davis=FALSE, quali=TRUE, current=TRUE) {
    require(data.table)
    ## read the data
    read_timing <- system.time(data <- data.table::fread(file)) ## removed fill=TRUE since I corrected the database!
    cat(paste(":: Read", nrow(data),"matches done in", round(read_timing[3], 5), "s\n")) 
  
    if (current) {
        if (file.exists("Data/ongoing_tourneys.csv")) {
            read_timing_curr <- system.time(current <- data.table::fread("Data/ongoing_tourneys.csv")) 
            cat(paste(":: Read Data/ongoing_tourneys.csv with ", nrow(current),"matches done in", round(read_timing_curr[3], 5), "s\n"))
            data <- AppendMatches(current, data)
        }
    }

    if (!quali) {
        before <- nrow(data)
        data <- data[!round %in% c("Q1", "Q2", "Q3", "Q4")]
        after <- nrow(data)
        cat(":: Removed", before-after, "qualification matches\n")

    }
  
    ## exclude Davis cup matches and select only relevant columns
    if (!davis) {
        ind_davis <- grep("^Davis", data$tourney_name)
        if (length(ind_davis)>0)
            data <- data[-ind_davis, ]
        cat(":: Removed", length(ind_davis), "Davis matches\n")
    }
    
    ## Define a new "year" column
    if (! "year" %in% colnames(data))
        data$year <- substr(data$tourney_date, 1, 4)

    return(data)
}

### Read the data from Sackmann's csv files
ReadDataSackmann <- function(dir, pattern="atp_matches_[[:digit:]]{4}.csv", davis=FALSE) {
    if (missing(dir))
        stop(":: Argument 'dir' is mandatory, path to directory containing Sackmann's csv files")

    ## We match only the csv files that we want, e.g. no challenger here
    lf <- list.files(path=dir, pattern=pattern, full.names=TRUE)
    lf_nonfull <- list.files(path=dir, pattern=pattern, full.names=FALSE)
    lf_nonfull <- sub("atp_matches_","",lf_nonfull)
    lf_nonfull <- sub(".csv","",lf_nonfull)
    
    cat(paste(":: Found ", length(lf), "files, from", lf_nonfull[1], "to", lf_nonfull[length(lf_nonfull)], "\n")) 
    
    ## read the data
    read_timing <- system.time(res <- lapply(lf, data.table::fread))
    names(res) <- lf_nonfull
    
    ## flatten the list into a big data.table adding the column with the year
    data <- data.table::rbindlist(res, use.names=TRUE, fill=FALSE, idcol=TRUE)
        
    cat(paste(":: Read", nrow(data),"matches done in", round(read_timing[3], 5), "s\n")) 
    
    ## exclude Davis cup matches and select only relevant columns
    if (!davis) {
        ind_davis <- grep("^Davis", data$tourney_name)
        data <- data[-ind_davis, ]
        cat(":: Removed", length(ind_davis), "Davis matches\n")
    }
    
    ## Define a new "year" column
    data$year <- substr(data$tourney_date, 1, 4)

    return(data)
    
}


### Function which finds out all players and tourneys contained in the database,
### and returns a list with those plus a data.table with each row containing 
### participant to a tournament
SummaryData <- function(db) {
    require(data.table)

    ## find out all tournaments
    all_tourn <- sort(unique(db$tourney_name))
    ## find out all players
    all_names <- sort(unique(c(db$winner_name, db$loser_name)))

    ## group all the finals per tournament, per winner and per year
    nw <- db[,.N, by=.(tourney_name, winner_name, year)][, 1:3]
    ## this was faster but didn't work for round robin tournaments such as Tour Finals
    ## nw <- data[round=="F",.N, by=.(tourney_name, winner_name, year)]

    ## find all losers per tournament and per year
    nl <- db[,.N, by=.(tourney_name, loser_name, year)][, 1:3]

    ## perform the union of the two sets! Here, each row represents 
    ## a player who played in that tournament in that year
    ## use.names=FALSE cause we have winner_ and loser_player
    dt <- data.table::rbindlist(list(nw, nl), use.names=FALSE) 
    colnames(dt) <- c("tourney_name", "player", "year")

    ## remove any ordering or "key" in data.table jargon
    if (data.table::haskey(dt)) 
        data.table::setkey(dt,NULL)
        
    ## we need now to remove duplicates
    dt <- unique(dt)
    setorder(dt, player, tourney_name, year)

    cat(":: the database contains", length(all_names), " players and", length(all_tourn),"tournaments\n") 

    return(list(tourney=all_tourn, players=all_names, dt=dt))
}


### Searches all tournaments played by a player, optionally in a given tournament 
### (over multiple years, that is). 
### "datalist" is the result of SummaryData(), has components "players", 
### "tourney" and "dt"
SearchByPlayer <- function(name, datalist, tournament) {

    if (!name %in% datalist$players)
        stop(":: Missing player, check the spelling\n")

    ## from the datalist
    tab <- datalist$dt
    ## first we look at the player
    dtplayer <- tab[player==name, .N, by=.(tourney_name)]
    if (!missing(tournament))
        dtplayer <- dtplayer[tourney_name==tournament]
    
    setorder(dtplayer, -N)
    colnames(dtplayer) <- c("tourney_name", "appearances")
    return(dtplayer)
}

### Extract all matches played by a given player from a db
PlayerMatches  <- function(name, db) {
    db[winner_name==name | loser_name==name, ]
}
### Extract all matches played by a given player from a db
PlayerMatchesById  <- function(id, db) {
    db[winner_id==id | loser_id==id, ]
}
### Extract all won matches played by a given player from a db
PlayerWonMatchesById  <- function(id, db) {
    db[winner_id==id, ]
}

### Extract all h2h matches between two players
h2h  <- function(name1, name2, db) {
    t1 <- PlayerMatches(name1, db)
    PlayerMatches(name2, t1)
}


### Quick/dirty output as table using gridExtra, outputting to pdf 
### and using pdftoppm and imagemagick to resize and trim to png
OutputTableToPng <- function(table, file) {
    require(gridExtra)
    a <- tempfile()
    cairo_pdf(a, height=50, width=30)
    grid.table(as.data.frame(table))
    dev.off()
    system(paste0("pdftoppm -png -r 150 ",a," > ", a,".png"))
    system(paste0("convert ",  a,".png -trim ", a,"_trim.png"))
    file.copy(paste0(a,"_trim.png"), file, overwrite=TRUE)
    file.remove(c(a, paste0(a,".png"),paste0(a,"_trim.png")))
    invisible()
}
