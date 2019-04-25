### Functions to scrape ATP database of matches

### Time-stamp: "Last modified 2019-04-25 18:28:40 delucia"

### Function to scrape all tourneys for a given year in the db
ScrapeYear <- function(year) {
    require(rvest)
    require(httr)

    uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
    page_url <- paste0("https://www.atptour.com/en/scores/results-archive?year=", year)
    
    response <- GET(page_url, user_agent(uastring))
    html <- read_html(response)
    
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
    inout_surf <- t(sapply(strsplit(tmpn, " "), rbind))

    ## extract the urls to each tournament - in the current year this list can be less long!
    have_results <- table$Results=="Results"
    ind_res <- which(have_results)
    
    urls  <- html_nodes(html, "a") %>% html_attr("href") %>% grep(pattern="results$|live-scores$", value=TRUE)
    tourney_urls <- rep(NA_character_, nrow(table))
    tourney_urls[ind_res] <- urls

    if (FALSE %in% have_results)
        cat(paste(":ScrapeYear: No results (and no url) for ", paste(title[!have_results], collapse = " ; "), year,"\n"))

    ## discriminate between current/monte-carlo/410/live-scores and archive/australasian-championships/580/1915/results
    has_current <- grep("current", tourney_urls)
    ## retrieve the tourney_id from the urls themselves if there is no "current"
    tourid <- gsub(".*archive/(.*)/results","\\1", tourney_urls[-has_current])
    tourney_ids_archive <- gsub("^.*/(.*?)/(.*)$","\\2_\\1", tourid) ## remove the name

    ## change it if there is "current"
    tourid <- gsub(".*current/(.*)/live-scores","\\1", tourney_urls[has_current])
    tourney_ids_current <- paste0(year, "_", gsub("^.*?/(.*)$","\\1", tourid))

    ## bring the ids together
    tourney_ids <- rep(NA_character_, nrow(table))
    tourney_ids[ has_current] <- tourney_ids_current
    tourney_ids[-has_current] <- tourney_ids_archive

    
    ## commitment should live in table[,6], clean up the "," as
    ## thousands separator
    commitment <- gsub(",","", table[,6], fixed=TRUE)

    
    tab <- data.table(date=date, year=year, tourney_name=title,
                      tourney_id=tourney_ids, surface=inout_surf[,2],
                      indoor=inout_surf[,1], commitment=commitment,
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
             "w_df"        = service1[3], 
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

    
ScrapePlayer <- function(name, id, db) {

    if (missing(id) & missing(name))
        stop(":ScrapePlayer: you must specify at least name or player_id!\n")

    if (missing(id)) {
        idw <- db[winner_name==name, unique(winner_id)]
        idl <- db[loser_name==name, unique(loser_id)]
        ids <- unique(tot_ids <- c(idw, idl))
        if (length(ids)>1) {
            id <- names(which.max(table(tot_id)))
            cat(":ScrapePlayer: several ids for", paste(name, sep=" "), ", going with", id,
                "; other ids are: ", paste(ids[ids!=id], sep="; "), "\n")
        } else id <- ids
    }
    
    if (missing(name)) {
        naw <- db[winner_id==id, unique(winner_name)]
        nal <- db[loser_id==id, unique(loser_name)]
        nam <- unique(c(naw, nal))
        if (length(nam)>1) {

            
            cat(":ScrapePlayer: several names for id ", id, ", going with", na[1], "\n")
            name <- name[1]
        }
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

    bday <- NA_character_
    if ("table-birthday" %in% allnodes)
        bday <- html_node(html, ".table-birthday") %>% html_text(trim=TRUE) %>% gsub(pattern="\\(|\\)", replacement="")
    
    plays <- NA_character_
    if ("table-value" %in% allnodes) {
        plays_tmp <- html_nodes(html, ".table-value") %>% html_text(trim=TRUE)
    
        ip <- grep("handed", plays_tmp, ignore.case=TRUE)
 
        if (length(ip)>0)
            plays <- plays_tmp[ip] %>% substr(start=1, stop=1)
    }

    nationality <- NA_character_
    if ("player-flag-code" %in% allnodes) {
        nationality <- html_nodes(html, ".player-flag-code") %>% html_text(trim=TRUE)
    }
    ## append bday and plays at end
    table <- rbind(table, c(plays, bday, nationality))
    return(table)
}
