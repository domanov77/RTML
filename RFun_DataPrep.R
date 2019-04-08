### Functions to interrogate the databases of matches

### Time-stamp: "Last modified 2019-04-05 19:58:54 delucia"
library(data.table)

PrepareDataTop <- function(file="Data/20190405-DataTop.csv") {
    
    ## read the data
    read_timing <- system.time(data <- data.table::fread(file)) ## removed fill=TRUE since I corrected the database!
    cat(paste(":: Reading done in", round(read_timing[3], 5), "s\n")) 
    
    ## exclude Davis cup matches and select only relevant columns
    ind_davis <- grep("^Davis", data$tourney_name)
    data <- data[-ind_davis, ] ## c("tourney_date", "tourney_name","tourney_level", "draw_size", "round", "winner_name","loser_name")]

    ## Define a new "year" column
    data$year <- substr(data$tourney_date, 1, 4)

    ## find out all tournaments
    all_tourn <- sort(unique(data$tourney_name))
    ## find out all players
    all_names <- sort(unique(c(data$winner_name, data$loser_name)))

    ## Define a new "year" column
    data$year <- substr(data$tourney_date, 1, 4)

    ## here .id column is the year
    ## group all the finals per tournament, per winner and per year
    nw <- data[,.N, by=.(tourney_name, winner_name, year)]
    ## this was faster but didn't work for round robin tournaments such as Tour Finals
    ## nw <- data[round=="F",.N, by=.(tourney_name, winner_name, year)]

    ## find all losers per tournament and per year
    nl <- data[,.N, by=.(tourney_name, loser_name, year)]

    ## perform the union of the two sets! Here, each row represents a player who played in that tournament in that year
    dt <- rbindlist(list(nw, nl), use.names=FALSE)[, 1:3]
    if (haskey(dt)) 
        setkey(dt,NULL)
    ## we need now to remove duplicates
    dt <- unique(dt)
    colnames(dt) <- c("tourn", "player", "year")
    setorder(dt, tourn, player, year)

    cat(":: prepared data.table with", nrow(dt), "rows and 3 columns. Bye!\n") 

    return(list(all_data=data, tourn=all_tourn, players=all_names, dt=dt))
}

PrepareDataSackmann <- function(dir="SackmanGit/tennis_atp", pattern="atp_matches_[[:digit:]]{4}.csv") {

    ## We match only the csv files that we want, e.g. no challenger here
    lf <- list.files(path=dir, pattern=pattern, full.names=TRUE)
    lf_nonfull <- list.files(path=dir, pattern=pattern, full.names=FALSE)
    lf_nonfull <- sub("atp_matches_","",lf_nonfull)
    lf_nonfull <- sub(".csv","",lf_nonfull)
    
    cat(paste(":: Found ", length(lf), "files, from", lf_nonfull[1], "to", lf_nonfull[length(lf_nonfull)], "\n")) 
    
    ## read the data
    read_timing <- system.time(res <- lapply(lf, fread))
    names(res) <- lf_nonfull
    cat(paste(":: Reading done in", round(read_timing[3], 5), "s\n")) 
    
    ## flatten the list into a big data.table adding the column with the year
    data <- rbindlist(res, use.names=TRUE, fill=FALSE, idcol=TRUE)
    
    ## exclude Davis cup matches and select only relevant columns
    ind_davis <- grep("^Davis", data$tourney_name)
    data <- data[-ind_davis, ] ###c(".id","tourney_date", "tourney_name","tourney_level", "draw_size", "round", "winner_name","loser_name")]

    ## find out all tournaments
    all_tourn <- sort(unique(data$tourney_name))
    ## find out all players
    all_names <- sort(unique(c(data$winner_name, data$loser_name)))

    ## here .id column is the year
    ## group all the finals per tournament, per winner and per year
    nw <- data[,.N, by=.(tourney_name, winner_name, .id)]
    ## this was faster but didn't work for round robin tournaments such as Tour Finals
    ## nw <- data[round=="F",.N, by=.(tourney_name, winner_name, .id)]

    ## find all losers per tournament and per year
    nl <- data[,.N, by=.(tourney_name, loser_name, .id)]

    ## perform the union of the two sets! Here, each row represents a player who played in that tournament in that year
    dt <- rbindlist(list(nw, nl), use.names=FALSE)[, 1:3]
    if (haskey(dt)) 
        setkey(dt,NULL)
    ## we need now to remove duplicates
    dt <- unique(dt)
    colnames(dt) <- c("tourn", "player", "year")
    setorder(dt, tourn, player, year)

    cat(":: prepared data.table with", nrow(dt), "rows and 3 columns. Bye!\n") 

    return(list(all_data=data, tourn=all_tourn, players=all_names, dt=dt))
}

SearchByPlayer <- function(name, data, tour) {

    if (!name %in% data$players)
        stop(":: Missing player, check the spelling\n")

    ## from the data
    tab <- data$dt
    ## first we look at the player
    dtplayer <- tab[player==name, .N, by=.(tourn)][order(N, decreasing = TRUE)]
    if (!missing(tour))
        dtplayer <- dtplayer[tourn==tour]

    return(dtplayer)
}

PlayerMatches  <- function(name, table) {
    table[winner_name==name | loser_name==name, ]
}


h2h  <- function(name1, name2, table) {
    t1 <- PlayerMatches(name1, table)
    PlayerMatches(name2, t1)
}



OutputTableToPng <- function(table, file) {
    require(gridExtra)
    a <- tempfile()
    cairo_pdf(a, height=50, width=20)
    grid.table(as.data.frame(table))
    dev.off()
    system(paste0("pdftoppm -png -r 150 ",a," > ", a,".png"))
    system(paste0("convert ",  a,".png -trim ", a,"_trim.png"))
    file.copy(paste0(a,"_trim.png"), file, overwrite=TRUE)
    file.remove(c(a, paste0(a,".png"),paste0(a,"_trim.png")))
    invisible()
}
