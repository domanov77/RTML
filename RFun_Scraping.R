### Functions to scrape ATP database of matches

### Time-stamp: "Last modified 2019-04-27 00:17:19 delucia"

### Function to scrape all tourneys for a given year in the db
ScrapeYear <- function(year, verbose=TRUE) {
    require(rvest)
    require(httr)
    
    uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
    page_url <- paste0("https://www.atptour.com/en/scores/results-archive?year=", year)
    
    response <- GET(page_url, user_agent(uastring))
    html <- read_html(response)

    allnodes <- html %>% html_nodes("*") %>% html_attr("class") %>% unique()

    ## first we look for the tournament names
    title <- html_nodes(html, ".tourney-title")%>% html_text()
    title <- gsub("[[:space:]]{2,}","", title)

    ## extract the big table with the entries
    table <- html_nodes(html, "table.results-archive-table")%>% html_table(header=TRUE)
    table <- table[[1]]

    ## date is contained in table[,3]
    date <- gsub(".*([[:digit:]]{4})\\.([[:digit:]]{2})\\.([[:digit:]]{2})","\\1\\2\\3", table[,3])

    ## draw size from table[,4]
    tmpd <- gsub("[[:space:]]{2,}"," ", table[,4])
    dsize <- gsub("SGL[[:space:]](.*)[[:space:]]DBL.*$","\\1", tmpd)

    ## extract surfaces and indoor/outdoor
    tmpn <- gsub("[[:space:]]{2,}"," ", table$Surface)
    ## some tourneys are still malformed and do not have Indoor|Outdoor[:space:]Surface!!
    indoor <- surface <- rep(NA_character_, length(tmpn))
    indoor[grep("Indoor", tmpn)] <- "Indoor"
    indoor[grep("Outdoor", tmpn)] <- "Outdoor"

    surface[grep("Hard", tmpn)] <- "Hard"
    surface[grep("Grass", tmpn)] <- "Grass"
    surface[grep("Clay", tmpn)] <- "Clay"
    surface[grep("Carpet", tmpn)] <- "Carpet"

    ## extract the urls to each tournament - in the current year this list can be less long!
    have_results <- table$Results=="Results"
    ind_res <- which(have_results)
    
    urls  <- html_nodes(html, "a") %>% html_attr("href") %>% grep(pattern="results$|live-scores$", value=TRUE)
    tourney_urls <- rep(NA_character_, nrow(table))
    tourney_urls[ind_res] <- urls

    if ((FALSE %in% have_results) & verbose)
        cat(paste(":ScrapeYear: No results (and no url) for ", paste(title[!have_results], collapse = " ; "), year,"\n"))
    
    ## discriminate between current/monte-carlo/410/live-scores and archive/australasian-championships/580/1915/results
    has_current <- grep("current", tourney_urls)
    has_archive <- grep("archive", tourney_urls)

    ## "current"
    tmpid <- gsub(".*current/(.*)/live-scores","\\1", tourney_urls[has_current])
    tourney_ids_current <- paste0(year, "_", gsub("^.*?/(.*)$","\\1", tmpid))
    
    ## retrieve the tourney_id from the urls themselves if there is no "current"
    tmpid <- gsub(".*archive/(.*)/results","\\1", tourney_urls[has_archive])
    tourney_ids_archive <- gsub("^.*/(.*?)/(.*)$","\\2_\\1", tmpid) ## remove the name
            
    ## bring the ids together
    tourney_ids <- rep(NA_character_, nrow(table))
    tourney_ids[ has_current ] <- tourney_ids_current
    tourney_ids[ has_archive ] <- tourney_ids_archive
    
    
    ## commitment should live in table[,6], clean up the "," as
    ## thousands separator
    commitment <- gsub(",","", table[,6], fixed=TRUE)
    
    
    tab <- data.table(date=date, year=year, tourney_name=title,
                      tourney_id=tourney_ids, surface=surface,
                      indoor=indoor, commitment=commitment,
                      draw_size=dsize,
                      url=ifelse(is.na(tourney_urls), NA_character_, paste0("https://www.atptour.com", tourney_urls)))
    
    if (FALSE %in% have_results)
        tab <- tab[ind_res]
    
    return(tab)
}

## Funtion which scrapes all matches in a given tourney
ScrapeTourney <- function(url, id, details=FALSE) {
    
    if (is.na(url) | url=="") return(NA_character_)
    

    ## discriminate between current/monte-carlo/410/live-scores and archive/australasian-championships/580/1915/results
    has_current <- grep("current", url)
    if (length(has_current)==0) {
        ## url is from archive
        tourid <- gsub(".*archive/(.*)/results","\\1", url)
        tourney_id <- gsub("^.*/(.*?)/(.*)$","\\2_\\1", tourid) ## remove the name
        name_url   <- gsub("(^.*?)/.*$","\\1", tourid) 
    } else {
        ## change it if there is "current"
        tourid <- gsub(".*current/(.*)/live-scores","\\1", url)
        tourney_id <- paste0("2019_", gsub("^.*?/(.*)$","\\1", tourid))
        name_url   <- gsub("(^.*?)/.*$","\\1", tourid) 
    }

    require(rvest)
    require(httr)

    uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
    response <- GET(as.character(url), user_agent(uastring))
    html <- read_html(response)

    allnodes <- html %>% html_nodes("*") %>% html_attr("class") %>% unique()

    table <- html_nodes(html, "table.day-table")%>% html_table(header=FALSE)
    
    if (length(table)==0) {
        cat(":ScrapeTourney: No results for ", name_url, " present\n")
        return(NA_character_)
    }
    
    table <- table[[1]]

    ind_round <- which(table[,2]!="")
    tmprounds <- table[ind_round,2]
    rounds <- rep(NA_character_,nrow(table))

    if ("Final" %in% tmprounds | "Quarterfinal" %in% tmprounds) {
        hash_round <- c("Final"="F",
                        "Semifinals"="SF",
                        "Quarterfinals"="QF",
                        "Round of 16"="R16",
                        "Round of 32"="R32",
                        "Round of 64"="R64",
                        "Round of 128"="R128",
                        "Round Robin"="RR",
                        "3rd Round Qualifying"="Q3",
                        "2nd Round Qualifying"="Q2",
                        "1st Round Qualifying"="Q1")

        
    } else {
        hash_round <- c("Finals"="F",
                        "Semi-Finals"="SF",
                        "Quarter-Finals"="QF",
                        "Round of 16"="R16",
                        "Round of 32"="R32",
                        "Round of 64"="R64",
                        "Round of 128"="R128",
                        "Round Robin"="RR",
                        "3rd Round Qualifying"="Q3",
                        "2nd Round Qualifying"="Q2",
                        "1st Round Qualifying"="Q1")
    }


    if (length(tmprounds)>1) {
        for (i in seq_along(tmprounds)[-1]) {
            rounds[seq(ind_round[i-1]+1, ind_round[i]-1)] <- hash_round[match(tmprounds[i-1], names(hash_round))]
        }
        rounds[seq(ind_round[length(ind_round)]+1, length(rounds))] <- hash_round[match(tmprounds[i], names(hash_round))]
    } else {
        rounds <- hash_round[match(tmprounds[1], names(hash_round))]
    }
           
    ## now get the players ids
    ids_players <- html_nodes(html, "a") %>% html_attr("href") %>% grep(pattern="players.*overview", value=TRUE) %>% 
        gsub(pattern="/en/players/",replacement="",fixed=TRUE) %>% gsub(pattern="/overview",replacement="",fixed=TRUE) %>%
        gsub(pattern="^.*/",replacement="")%>% toupper()
    
    player_id <- matrix( ids_players, ncol=2, byrow=TRUE)
 
### for now no matches_id
    ## ids_m    <- html_nodes(html, "a") %>% html_attr("href") %>% grep(pattern="match-stats", value=TRUE) %>% 
    ##     gsub(pattern="/en/scores/",replacement="",fixed=TRUE) %>% gsub(pattern="/match-stats.*$",replacement="") %>% 
    ##     gsub(pattern="^.*/",replacement="")
    
    ## if (length(ids_m)==0) {
    ##     cat(paste(":: Tourney", tid, "no match ids\n"))
    ##     ids_m <- rep(NA_character_, nrow(ids_p))
    ## }
 
    tab <- data.table(round=rounds, table)[-ind_round, c(1,2,4,6,8,9)]

    tab <- cbind(tab, player_id)
    colnames(tab) <- c("round", "winner_seed", "winner_name", "loser_seed", "loser_name","score","winner_id", "loser_id")
    rownames(tab) <- NULL
    
    ## some cleanup
    ## remove parenthesis from "seeds" fields
    tab$winner_seed <- gsub("\\(|\\)","", tab$winner_seed)
    tab$loser_seed <- gsub("\\(|\\)","", tab$loser_seed)
    
    tab$score <- gsub("\\((RET|W/O)\\)","\\1", tab$score)
    tab$score <- gsub("(76|67)([[:digit:]]{1,3})([[:space:]]|$)","\\1(\\2)\\3", tab$score)
    
    tab$score <- gsub("([[:digit:]])([[:digit:]])","\\1-\\2",tab$score)
    tab$score <- gsub("\\(([[:digit:]])-([[:digit:]])\\)","(\\1\\2)",tab$score)
    ## fix long sets, which at this point should be in the form "n-nn-n"
    tab$score <- gsub("([[:digit:]])-([[:digit:]])([[:digit:]])-([[:digit:]])","\\1\\2-\\3\\4",tab$score)
    ## NB: this doesn't work if the long sets start by 67 or 76!!
    
    ## extract the urls to single match stats, maybe we can scrape them in future
    url_matches <- rep(NA_character_, nrow(tab))
    urls  <- html_nodes(html, "a") %>% html_attr("href") %>% grep(pattern="match-stats", fixed=TRUE, value=TRUE)
    
    if (length(urls)!=0) { ## there are match stats, apparently
        ## if there are walkovers we have less urls than matches!
        wos <- which(tab$score!="W/O")
        url_matches[wos] <- urls
    }

    tab$tourney_id_from_url <- rep(tourney_id, nrow(tab))
    tab$url_matches <- url_matches

     
    ## Find other details: date, surface, draw size... all these info
    ## are actually already scrpaed in "ScrapeYear"; we explicitely
    ## want them retrieved here (if details==TRUE)
    if (details) {
        options(try.outFile = stdout()) 
        det <- try(html_nodes(html, "table.tourney-results-wrapper")%>% html_table(fill=TRUE), silent=TRUE)

        if (class(det)!="try-error") {
            ## which surface is played on?
            dd <- det[[1]]
            surf <- dd[2,2]
            tab$surface <- surf
            
            ## extract the draw_size
            tmpd <- gsub("\\r|\\n|\\t","", dd[2,1])
            tmpd <- gsub("[[:space:]]+"," ", tmpd)
            ## draw_size <- unlist(strsplit(tmpd, " "))[2] 
            
            ## extract tourney_name (if no id is provided)
            tmpn <- unlist(strsplit( dd[1,2], "\r\n"))
            tmpn <- gsub("[[:space:]]{2,}","", tmpn)
            if (missing(id))
                tab$tourney_name <- tmpn[1]
            else
                tab$tourney_name <- id
            
### tourney_date and year are known already    
            ## date <- tmpn[length(tmpn)]
            ## date <- unlist(strsplit( date, " - "))[2]
            ## year <- unlist(strsplit( date, "\\."))[1]
            ## date <- gsub(".", "", date, fixed=TRUE)
            ## tab$tourney_date <- date
            ## tab$year <- year
        } else {
            tab$surface <- NA_character_
            tab$tourney_name <- gsub("^.*archive/([[:alnum:]]+)/.*$","\\1", url)
            ## tab$year <- gsub("^.*/([[:digit:]]{4})/results$","\\1", url)
            ## tab$tourney_date <- NA_character_
            cat(":: ScrapeTourney: found no details\n")
        }

    }

    
    ## We return the tab in inverse order (final last)
    return(tab[nrow(tab):1])
}


### Superseded by the "table" method
##     scores  <- html_nodes(html, ".day-table-score") %>% html_text()
##     players <- html_nodes(html, ".day-table-name") %>% html_text()
##     seeds   <- html_nodes(html, ".day-table-seed") %>% html_text()
##     urls    <- html_nodes(html, "a") %>% html_attr("href") %>% grep(pattern="match-stats", fixed=TRUE, value=TRUE)
##     rounds  <- html_nodes(html, ".th") %>% html_text()
## 
##     scores <- gsub("\\r|\\n|\\t","", scores)
##     scores <- gsub("[[:space:]]+$","", scores)
## 
##     seeds <- gsub("\\r|\\n|\\t","", seeds)
## 
##     players <- gsub("\\r|\\n|\\t","", players)
##     players <- gsub("[[:space:]]{2,}","", players)
## 
##     n <- length(players)
##     nmatches <- n/2


### Function returns the match stats for the match pointed by the url
ScrapeMatch <- function(url, winner) {

    ## prepare the return container if we can't scrape
    ret_na <- c("w_ace"     = NA_integer_,
                "w_df"      = NA_integer_,
                "w_svpt"    = NA_integer_,
                "w_1stIn"   = NA_integer_,
                "w_1stWon"  = NA_integer_,
                "w_2ndWon"  = NA_integer_,
                "w_SvGms"   = NA_integer_,
                "w_bpSaved" = NA_integer_,
                "w_bpFaced" = NA_integer_,
                "l_ace"     = NA_integer_,
                "l_df"      = NA_integer_,
                "l_svpt"    = NA_integer_,
                "l_1stIn"   = NA_integer_,
                "l_1stWon"  = NA_integer_,
                "l_2ndWon"  = NA_integer_,
                "l_SvGms"   = NA_integer_,
                "l_bpSaved" = NA_integer_,
                "l_bpFaced" = NA_integer_,
                "minutes"   = NA_integer_)
    ## proceed only if url is not NA
    if (is.na(url)) {
        return(ret_na)
    }
    
    require(rvest)
    require(httr)

    uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
    page_url <- paste0("https://www.atptour.com", url)
    
    response <- GET(page_url, user_agent(uastring))
    html <- read_html(response)

    allnodes <- html %>% html_nodes("*") %>% html_attr("class") %>% unique()

    ## some pages just don't cut it for rvest.
    if (!"time" %in% allnodes)
        return(ret_na)

    ## now we should be safe
    time <- html_nodes(html, ".time")%>% html_text()
    time <- gsub("[[:space:]]","", time)
    ## convert HH:MM:SS into total minutes
    minutes <- sum(as.integer(unlist(strsplit(time,":")))*c(60,1,0))



    ## Who are the players
    player1 <- html_nodes(html, ".player-left-name")%>% html_text(trim=TRUE) 
    player1 <- gsub("[[:space:]]+"," ", player1) 
    
    player2 <- html_nodes(html, ".player-right-name")%>% html_text(trim=TRUE) 
    player2 <- gsub("[[:space:]]+"," ", player2) 
    
    ## Find out who actually won if we don't know already
    if (missing(winner)) {
        win <- html_nodes(html, ".won-game")%>% html_text(trim=TRUE)
        win <- gsub("^.*\\.[[:space:]]","",win)

        ## fuzzy matching
        if (agrepl(win, player1, ignore.case = TRUE))
            winner <- player1
        else if (agrepl(win, player2, ignore.case = TRUE))
            winner <- player2
        else 
            return(ret_na)

        ## not all pages contain this!!!
        ## win <- html_nodes(html, ".match-info-row")%>% html_text(trim=TRUE) 
        ## win <- gsub("[[:space:]]+"," ", win) 
        ## win <- gsub("^.*Match[[:space:]](.*?)\\..*$","\\1",win)
    }

    
    ## extract the big table with the entries we need
    if (!"match-stats-table" %in% allnodes)
        return(ret_na)
    table <- html_nodes(html, "table.match-stats-table")%>% html_table() ##header=TRUE
    table <- table[[1]]
    fields <- table[,3]
    
    ## swap left and right based on the winner
    ind_left    <- ifelse(player1==winner, 1, 5)
    ind_right  <- ifelse(player2==winner, 1, 5)
    if (ind_left==ind_right) {
        cat(":: Can't find winner : < ", winner, " > or player1= < ", player1, " > or player2= < ", player2, " >\n")
        return(ret_na)
    }
    service1 <- table[,ind_left]
    service2 <- table[,ind_right]
    service1 <- gsub("[[:space:]]{2,}"," ", service1)
    service2 <- gsub("[[:space:]]{2,}"," ", service2)
    stats <- cbind(fields, service1, service2)
    row <- c("w_ace"     = service1[2], 
             "w_df"      = service1[3], 
             "w_svpt"    = gsub("^.*/([[:digit:]]+).*$","\\1", service1[4]),
             "w_1stIn"   = gsub("^.*\\(([[:digit:]]+)/.*$","\\1", service1[4]),
             "w_1stWon"  = gsub("^.*\\(([[:digit:]]+)/.*$","\\1", service1[5]),
             "w_2ndWon"  = gsub("^.*\\(([[:digit:]]+)/.*$","\\1", service1[6]),
             "w_SvGms"   = service1[8],
             "w_bpSaved" = gsub("^.*\\(([[:digit:]]+)/.*$","\\1", service1[7]),
             "w_bpFaced" = gsub("^.*/([[:digit:]]+).*$","\\1", service1[7]),
             "l_ace"     = service2[2], 
             "l_df"      = service2[3], 
             "l_svpt"    = gsub("^.*/([[:digit:]]+).*$","\\1", service2[4]),
             "l_1stIn"   = gsub("^.*\\(([[:digit:]]+)/.*$","\\1", service2[4]),
             "l_1stWon"  = gsub("^.*\\(([[:digit:]]+)/.*$","\\1", service2[5]),
             "l_2ndWon"  = gsub("^.*\\(([[:digit:]]+)/.*$","\\1", service2[6]),
             "l_SvGms"   = service2[8],
             "l_bpSaved" = gsub("^.*\\(([[:digit:]]+)/.*$","\\1", service2[7]),
             "l_bpFaced" = gsub("^.*/([[:digit:]]+).*$","\\1", service2[7]),
             "minutes"   = minutes
             )
    return(row)
}


## Function which scrapes the match stats and adds them to a "tourney tab", 
## that is, the data returned by ScrapeTourney()
## parallelization through foreach and doParallel (should be multiplatform!)
ScrapeMatchStats <- function(tab, cores=8) {
    if (!"url_matches" %in% colnames(tab))
        stop(":: ScrapeMatchStats needs the data.table returned by ScrapeTourney()\n:: with well formed url_matches!")
    
    require(doParallel)
    require(foreach)
    cl <- makeCluster(cores)
    registerDoParallel(cl, cores=cores)
    
    match_stats <- foreach(i=seq_along(tab$url_matches),
                           .combine='rbind',
                           .export="ScrapeMatch") %dopar% {                               
                               ScrapeMatch(tab$url_matches[i], winner=tab$winner_name[i])
        }
    
    ## close parallelization cluster
    stopImplicitCluster()
    stopCluster(cl)

    ## append match_stats to the tab
    ret <- data.table(cbind(tab, match_stats))
    ## remove the url_matches
    ret[, url_matches:=NULL]
    return(ret)
    
}

### Serial veersion of the above for debug
ScrapeMatchStatsSerial <- function(tab) {
    
    if (!"url_matches" %in% colnames(tab))
        stop(":: ScrapeMatchStatsSerial needs the data.table returned by ScrapeTourney()\n:: with well formed url_matches!")
    

    
    match_stats <<- vector(mode="list", length=nrow(tab))
    for (i in seq_along(tab$url_matches)) {
        cat(paste(":: ", i, ")", tab$tourney_name[i], " - ", tab$winner_name[i]," vs ", tab$loser_name[i]))
        match_stats[[i]] <<- ScrapeMatch(tab$url_matches[i], winner=tab$winner_name[i])
        cat("  [done] \n")
    }

    stats <- data.table(do.call("rbind", match_stats))
    ## append match_stats to the tab
    ret <- data.table(cbind(tab, stats))
    ## remove the url_matches
    ret[, url_matches:=NULL]
    return(ret)
    
}


### Function which appends a data.table of new matches 
### (with or without stats) to the all_data db
AppendMatches <- function(matches, db) {
    ret <- rbindlist(list(db, matches), use.names=TRUE, fill=TRUE)
    return(ret)
}


ScrapeIdsFromTourney <- function(url) {
    ##url <- "https://www.atptour.com/en/scores/archive/brisbane/339/2019/results"
    if (is.na(url)|url=="") return(data.table(winner_id=NA_character_, loser_id=NA_character_, match_id=NA_character_, tourney_id=NA_character_))
    require(rvest)
    require(httr)

    tourid <- gsub(".*archive/(.*)/results","\\1", url)
    tid <- paste0(rev(unlist(strsplit(tourid, "/")))[1:2], collapse="_")
    
    uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
    response <- GET(as.character(url), user_agent(uastring))
    html <- read_html(response)

    allnodes <- html %>% html_nodes("*") %>% html_attr("class") %>% unique() %>% sort()

    table <- html_nodes(html, "table.day-table")%>% html_table(header=FALSE)
    
    if (length(table)==0) {
        cat(":: No results present\n")
        return(NA_character_)
    }
        
    ## now get the players ids
    ids_players    <- html_nodes(html, "a") %>% html_attr("href") %>% grep(pattern="players.*overview", value=TRUE) %>% ##unique() %>% sort() %>%
    gsub(pattern="/en/players/",replacement="",fixed=TRUE) %>% gsub(pattern="/overview",replacement="",fixed=TRUE) %>% gsub(pattern="^.*/",replacement="")%>% toupper()
    
    ids_p <- matrix( ids_players, ncol=2, byrow=TRUE)
 
    ## now get the matches_id
    ids_m    <- html_nodes(html, "a") %>% html_attr("href") %>% grep(pattern="match-stats", value=TRUE) %>% ##unique() %>% sort() %>%
    gsub(pattern="/en/scores/",replacement="",fixed=TRUE) %>% gsub(pattern="/match-stats.*$",replacement="") %>% 
    gsub(pattern="^.*/",replacement="")
    
    if (length(ids_m)==0) {
        cat(paste(":: Tourney", tid, "no match ids\n"))
        ids_m <- rep(NA_character_, nrow(ids_p))
        }
 
    tabid <- cbind( ids_p, ids_m, rep(tid, nrow(ids_p)))
    colnames(tabid) <- c("winner_id", "loser_id", "match_id", "tourney_id")

    ## We return the tab in inverse order (final last)
    return( data.table(tabid[ nrow(tabid):1, ] ))
}

### Function which scrapes the data for a single player, including
### ranking history, playing hand and nationality. It accepts player
### name and/or id as input
ScrapePlayer <- function(name, id, db) {

    if (missing(id) & missing(name))
        stop(":ScrapePlayer: you must specify at least name or player_id!\n")

    has_other_id <- FALSE
    if (missing(id)) { 
        ## check if more than one ID is used
        idw <- db[winner_name==name, unique(winner_id)]
        idl <- db[loser_name==name, unique(loser_id)]
        tot_id <- c(idw, idl)
        ids <- unique(tot_id)
        ## if several IDs are present, we choose the most common one
        if (length(ids)>1) {
            has_other_id <- TRUE
            id <- names(which.max(table(tot_id)))
            cat(":ScrapePlayer: several ids for", paste(name, sep=" "), ", going with", id,
                "; other ids are: ", paste(ids[ids!=id], sep="; "), "\n")
        } else id <- ids
    }
    

    has_other_name <- FALSE
    if (missing(name)) {
        ## same logic for the names
        naw <- db[winner_id==id, unique(winner_name)]
        nal <- db[loser_id==id, unique(loser_name)]
        tot_nam <- c(naw, nal)
        nam <- unique(tot_nam)
        if (length(nam)>1) {
            has_other_name <- TRUE
            name <- names(which.max(table(tot_nam)))
            cat(":ScrapePlayer: several names for id ", id, ", going with", name,
                "; other names are: ", paste(nam[nam!=name], sep="; "), "\n")
        } else name <- nam
    }

    url <- paste0("https://www.atptour.com/en/players/", tolower(gsub(" ", "-", fixed=TRUE, name)), "/", tolower(id),"/rankings-history")
    require(rvest)
    require(httr)
    
    uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
    response <- GET(as.character(url), user_agent(uastring))
    html <- read_html(response)
    
    allnodes <- html %>% html_nodes("*") %>% html_attr("class")
    
    if ("mega-table" %in% allnodes)
        table <- html_node(html, "table.mega-table") %>% html_table(header=TRUE)
    else
        table <- data.frame(Date=NA_character_, Singles=NA_character_, Doubles=NA_character_) 
    
    ## Get rid of T for tied
    table$Singles <- gsub(pattern="T", replacement="", table$Singles, fixed=TRUE)
    table$Doubles <- gsub(pattern="T", replacement="", table$Doubles, fixed=TRUE)
    
    
    ## birthday
    bday <- NA_character_
    if ("table-birthday" %in% allnodes)
        bday <- html_node(html, ".table-birthday") %>% html_text(trim=TRUE) %>% gsub(pattern="\\(|\\)", replacement="")
    

    ## player hand
    plays <- NA_character_
    if ("table-value" %in% allnodes) {
        plays_tmp <- html_nodes(html, ".table-value") %>% html_text(trim=TRUE)
    
        ip <- grep("handed", plays_tmp, ignore.case=TRUE)
 
        if (length(ip)>0)
            plays <- plays_tmp[ip] %>% substr(start=1, stop=1)
    }

    ## nationality
    nationality <- NA_character_
    if ("player-flag-code" %in% allnodes) {
        nationality <- html_nodes(html, ".player-flag-code") %>% html_text(trim=TRUE)
    }

    
    ## pack all informations in a list
    ret <- list(table=table, plays=plays, bday=bday, nation=nationality, id=id, name=name,
                other_ids=ifelse(has_other_id, ids[ids!=id], NA_character_),
                other_names=ifelse(has_other_name, nam[nam!=name], NA_character_),
                url=url) 
    return(ret)
}



### Function which scrapes the data for a single player, WITH NO
### Ranking History, i.e. only playing hand, bday and nationality. It
### accepts ONLY player name as input
ScrapePlayerNoRank <- function(name, id) {

    if (missing(id) | missing(name))
        stop(":ScrapePlayerNoRank: you must specify name and id!\n")

    url <- paste0("https://www.atptour.com/en/players/", tolower(gsub(" ", "-", fixed=TRUE, name)), "/", tolower(id),"/overview")
    require(rvest)
    require(httr)
    
    uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
    response <- GET(as.character(url), user_agent(uastring))
    html <- read_html(response)

    allnodes <- html %>% html_nodes("*") %>% html_attr("class")

    ## birthday
    bday <- NA_character_
    if ("table-birthday" %in% allnodes)
        bday <- html_node(html, ".table-birthday") %>% html_text(trim=TRUE) %>% gsub(pattern="\\(|\\)", replacement="")
    

    ## player hand
    plays <- NA_character_
    if ("table-value" %in% allnodes) {
        plays_tmp <- html_nodes(html, ".table-value") %>% html_text(trim=TRUE)
    
        ip <- grep("handed", plays_tmp, ignore.case=TRUE)
 
        if (length(ip)>0)
            plays <- plays_tmp[ip] %>% substr(start=1, stop=1)
    }

    ## nationality
    nationality <- NA_character_
    if ("player-flag-code" %in% allnodes) {
        nationality <- html_nodes(html, ".player-flag-code") %>% html_text(trim=TRUE)
    }

    
    ## pack all informations in a list
    ret <- list(plays=plays, bday=bday, nation=nationality, id=id, name=name, url=url) 
    return(ret)
}



### Function to scrape all atp rankings (1-5000) for a given week
ScrapeRankings <- function(date) {

    ret_na <- data.frame("Ranking"=NA_integer_, "Player"=NA_character_, "Age"=NA_integer_, "Points"=NA_integer_, "Tourn_Played"=NA_integer_,   
                         "Points_Dropping"=NA_integer_, "Next_Best"=NA_integer_) 

    url <- paste0("https://www.atptour.com/en/rankings/singles?rankDate=", date, "&rankRange=1-5000")
    require(rvest)
    require(httr)
    
    uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
    response <- GET(as.character(url), user_agent(uastring))
    html <- read_html(response)
    
    allnodes <- html %>% html_nodes("*") %>% html_attr("class") %>% unique() %>% sort() 

    ## check if the date is present
    if ("dropdown" %in% allnodes) {
        all_dates <- html_nodes(html, ".dropdown") %>% html_text(trim=TRUE) 
        all_dates <- strsplit(gsub(pattern="[[:space:]]{2,}", " ", all_dates[1]), " ")[[1]]
        if (! gsub("-",".", date, fixed=TRUE) %in% all_dates) {
            cat(":ScrapeRankings: date is not correct\n")
            return(ret_na)
        }
    }
    
    if ("mega-table" %in% allnodes) {
        table <- html_node(html, "table.mega-table") %>% html_table(header=TRUE)
        table[, c("Move", "Country")] <- NULL
        colnames(table) <- gsub(" ", "_", colnames(table), fixed=TRUE)
        table$Ranking <- as.integer(gsub("T","",table$Ranking, fixed=TRUE))
        table$Points  <- as.integer(gsub(",","",table$Points, fixed=TRUE))
        table$Points_Dropping  <- as.integer(gsub(",","",table$Points_Dropping, fixed=TRUE))
    } else {
        cat(":ScrapeRankings: no data found, check url and date\n")
        table <- ret_na
    }
    return(table)
}

### Function which retrieves all ranking dates and returns them as strings
AvailableRankings <- function() {

    require(rvest)
    require(httr)
    
    uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
    url <- "https://www.atptour.com/en/rankings/singles"
    response <- GET(as.character(url), user_agent(uastring))
    html <- read_html(response)
    all_dates <- html_nodes(html, ".dropdown") %>% html_text(trim=TRUE) 
    all_dates <- strsplit(gsub(pattern="[[:space:]]{2,}", " ", all_dates[1]), " ")[[1]]
    all_dates <- gsub(pattern=".", "", all_dates, fixed=TRUE)
    return(rev(all_dates))
    
}
    

## Function which finds the right ranking date from a data.table
## containing matches (typically as result of ScrapeMatchStats),
## controls the available rankings, downloads the ranking into
## Rankings/Rankings/Ranks_DATE.csv and fills the DT with the ranks of
## the players
ScrapeRankingForMatches <- function(matches) {
    fmt_td   <- '%Y%m%d'
    fmt_bday <- '%Y.%m.%d'
    ## scrape available rankings from ATP site
    avranks <- AvailableRankings()
    
    tdate <- as.Date(as.character(unique(matches$date)), format=fmt_td)
    ranks <- as.Date(as.character(avranks), format=fmt_td)

    ind <- rep(NA_integer_, length(tdate))
    for (i in seq_along(tdate))
        ind[i] <- which.max(ranks >= tdate[i])

    un_inds <- unique(ind)

    to_scrape <- ranks[un_inds]
    new <- copy(matches)
    for (i in seq_along(to_scrape)) {
        ## start the iteration per tourney
        nfile <- paste0("Rankings/Ranks_", to_scrape[i],".csv")
        cat(":ScrapeRankingForMatches: looking for rankings on ", as.character(to_scrape[i]))
        if (!file.exists(nfile)) {
            rtab <- ScrapeRankings(to_scrape[i])
            fwrite(rtab, file=nfile)
            cat("\n:ScrapeRankingForMatches: rankings retrieved and saved in file ", nfile,"\n")
        } else {
            rtab <- fread(nfile)
            cat(": file ", nfile,"already present, no need to scrape it\n")
        }
        ind_matches <- which(matches$date==gsub("-","", tdate[i],fixed=TRUE))
        w <- new$winner_name[ind_matches]
        l <- new$loser_name[ind_matches]
        indw <- match(w, rtab$Player)
        indl <- match(l, rtab$Player)
        set(new, ind_matches, "winner_rank", rtab$Ranking[indw])
        set(new, ind_matches, "loser_rank",  rtab$Ranking[indl])
    }

    ## strip the column "tpirney_id_from_url" which was only used as control
    if ("tourney_id_from_url" %in% colnames(new))
        set(new, ,"tourney_id_from_url", NULL )
    return(new)
}




### Function which automatically updates the db
UpdateDB <- function(db, write_ended=FALSE, write_current=FALSE) {
    if (missing(db))
        db <- ReadData("Data/dbtml.csv")

    newdb <- copy(db)

    this_year <- as.integer(format(Sys.Date(),"%Y"))
    y <- ScrapeYear(this_year, verbose=FALSE)

    current <- grep("current", y$url)

    playing <- y[ current]
    archive <- y[-current]

    ## completely missing tourneys which are already finished and
    ## archived (typically, a few days later they are online)
    inds <- which(! archive$tourney_id %in% newdb$tourney_id) 

    if (length(inds) > 0) { ## we have whole new tourneys to scrape
        to_scrape <- archive[inds]
        cat(":UpdateDB: going to add", paste(to_scrape$tourney_name, collapse=", "), "to the db\n")
        tourneys <-  lapply(to_scrape$url, ScrapeTourney)
        
        ind_nodata <- which(is.na(tourneys))
        if (length(ind_nodata)> 0) {
            tourneys[ind_nodata] <- NULL
            to_scrape <- to_scrape[-ind_nodata]
        }

        newmatches <- rbindlist(lapply(seq_along(tourneys), function(i) add_info_from_tour(tourney=to_scrape[i], matches=tourneys[[i]])))

        res <- ScrapeMatchStats(newmatches, cores=4) 
        cat(":UpdateDB: retrieved match statistics\n")
        res <- ScrapeRankingForMatches(res) 
        cat(":UpdateDB: retrieved rankings, now adding player info\n")
        final <- AddPlayerInfo(res, save=TRUE)
        
        newdb <- AppendMatches(final, newdb)
        if (write_ended) {
            cat(":UpdateDB: writing the new db onto Data/dbtml.csv\n") 
            fwrite(newdb, paste0("Data/dbtml.csv"), quote=FALSE)
        }
    } else
        cat(":UpdateDB: No new archived tourneys/matches to write in the db\n") 


    ## Now we scrape the results of the "current", which we save separately
    ## today <- format(Sys.Date(),"%Y-%m-%d")
    ## now <- format(Sys.time(),"%Y-%m-%d_%H_%M")
    cat(":UpdateDB: scraping ongoing tourneys", paste0(playing$tourney_name, collapse=", "),"\n")
        
    ongoing_tourneys <- lapply(playing$url, ScrapeTourney)
    ind_nodata <- which(is.na(ongoing_tourneys))
    if (length(ind_nodata)> 0) {
        ongoing_tourneys[ind_nodata] <- NULL
        playing <- playing[-ind_nodata]
    }

    on_matches <- rbindlist(lapply(seq_along(ongoing_tourneys), function(i) add_info_from_tour(tourney=playing[i], matches=ongoing_tourneys[[i]])))

    on_res <- ScrapeMatchStats(on_matches, cores=4) 
    cat(":UpdateDB: retrieved match statistics for ongoing tournaments\n")
    on_res <- ScrapeRankingForMatches(on_res) 
    cat(":UpdateDB: retrieved rankings for ongoing tournaments, now adding player info\n")

    on_final <- AddPlayerInfo(on_res, save=TRUE)

    if (write_current) {
        cat(":UpdateDB: writing the matches of ongoing tournaments onto Data/ongoing_tourneys.csv\n")
        fwrite(on_final, "Data/ongoing_tourneys.csv", quote=FALSE)
    }
    newdb <- AppendMatches(on_final, newdb)
    return(newdb)
}


## workhorse function to add to a DT with matches (or match_stats) the
## tournament informations from a "tourney" DT
add_info_from_tour <- function(tourney, matches) {
    matches$year <- tourney$year
    matches$date <- tourney$date
    matches$indoor <- tourney$indoor
    matches$surface <- tourney$surface
    matches$commitment <- tourney$commitment
    matches$draw_size <-  tourney$draw_size
    matches$tourney_id <- tourney$tourney_id 
    matches$tourney_name <- tourney$tourney_name
    return(matches)
}



### Function which fills missing informations for a Player from
### records in the PlayerInfo.RData database; if the player is not
### already in the db, scrapes those informations from the ATP site
AddPlayerInfo <- function(dt, save=TRUE) {
    hdt <- HashNameId(dt)
    pinfo <- readRDS("Data/PlayersInfo.RData")

    has_info <- sapply(hdt$player, function(x) x %in% names(pinfo))
    
    ind_to_scrape <- which(!has_info)
    if (length(ind_to_scrape)> 0) {
        scraped <- vector(mode="list", length=length(ind_to_scrape))
        for (i in seq_along(ind_to_scrape)) {
            scraped[[i]] <- ScrapePlayerNoRank(hdt[ ind_to_scrape[i],1], hdt[ ind_to_scrape[i],2])
        }
        names(scraped) <- hdt[ind_to_scrape, player]
        pinfo <- c(pinfo, scraped)
        if (save) {
            saveRDS(pinfo, "Data/PlayersInfo.RData")
            cat(":AddPlayerInfo: saved player info for ", paste(names(scraped), collapse=", "), "\n")
        }
    }

    for (i in seq(1, nrow(hdt))) {
        set(hdt, i, "plays",  pinfo[[ hdt$player[i] ]]$plays)
        set(hdt, i, "bday",   pinfo[[ hdt$player[i] ]]$bday)
        set(hdt, i, "nation", pinfo[[ hdt$player[i] ]]$nation)
    }
    
    w <- dt$winner_name
    l <- dt$loser_name
    indw <- match(w, hdt$player)
    indl <- match(l, hdt$player)
    set(dt, , "winner_hand", hdt$plays[indw])
    set(dt, , "loser_hand",  hdt$plays[indl])
    
    set(dt, , "winner_ioc", hdt$nation[indw])
    set(dt, , "loser_ioc",  hdt$nation[indl])

    ## Compute Age
    fmt_db <- '%Y%m%d'
    fmt_bday <- '%Y.%m.%d'
    agew <- round(as.numeric(as.Date(as.character(dt$date), format=fmt_db)-as.Date(hdt$bday[indw],format=fmt_bday))/365.25,6)
    agel <- round(as.numeric(as.Date(as.character(dt$date), format=fmt_db)-as.Date(hdt$bday[indl],format=fmt_bday))/365.25,6)
    
    set(dt, , "winner_age", agew)
    set(dt, , "loser_age",  agel)

    return(dt)
}


### Workhorse Function to give a "hash table" of player names and
### player ids
HashNameId <- function(dt) {
    tot <- data.table(cbind(player=c(dt$winner_name, dt$loser_name),
                            id=c(dt$winner_id, dt$loser_id)))
    tmp <- unique(tot)
    return(tmp[order(id)])
}
