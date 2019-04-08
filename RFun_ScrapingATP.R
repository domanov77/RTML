### Functions to interrogate the databases of matches

### Time-stamp: "Last modified 2019-04-05 19:58:54 delucia"

ScrapeTourneyFromATP <- function(url, id) {
    require(rvest)
    require(httr)

    uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
    response <- GET(as.character(url), user_agent(uastring))
    html <- read_html(response)

    table <- html_nodes(html, "table.day-table")%>% html_table(header=FALSE)
    table <- table[[1]]

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

    ind_round <- which(table[,2]!="")
    tmprounds <- table[ind_round,2]

    rounds <- rep("",nrow(table))
    for (i in seq_along(tmprounds)[-1]) {
        rounds[seq(ind_round[i-1]+1, ind_round[i]-1)] <- hash_round[match(tmprounds[i-1], names(hash_round))]
    }
    rounds[seq(ind_round[length(ind_round)]+1, length(rounds))] <- hash_round[match(tmprounds[i], names(hash_round))]
    
    
    tab <- data.frame(round=rounds, table, stringsAsFactors=FALSE)[-ind_round, c(1,2,4,6,8,9)]
    colnames(tab) <- c("round", "winner_seed", "winner_name", "loser_seed", "loser_name","score")
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
    urls  <- html_nodes(html, "a") %>% html_attr("href") %>% grep(pattern="match-stats", fixed=TRUE, value=TRUE)

    ## if there are walkovers we have less urls than matches!
    wos <- which(tab$score!="W/O")
    url_matches <- rep("NA", nrow(tab))
    url_matches[wos] <- urls
    
    tab$url_matches <- url_matches
    
    ## Find other details: date, surface, draw size...
    details <- html_nodes(html, "table.tourney-results-wrapper")%>% html_table(fill=TRUE)

    ## which surface is played on?
    dd <- details[[1]]
    surf <- dd[2,2]
    tab$surface <- surf
    
    ## extract the draw_size
    tmpd <- gsub("\\r|\\n|\\t","", dd[2,1])
    tmpd <- gsub("[[:space:]]+"," ", tmpd)
    draw_size <- unlist(strsplit(tmpd, " "))[2] 

    ## extract tourney_name (if no id is provided)
    tmpn <- unlist(strsplit( dd[1,2], "\r\n"))
    tmpn <- gsub("[[:space:]]{2,}","", tmpn)
    if (missing(id))
        tab$tourney_name <- tmpn[1]
    else
        tab$tourney_name <- id
        
    date <- tmpn[length(tmpn)]
    date <- unlist(strsplit( date, " - "))[2]
    year <- unlist(strsplit( date, "\\."))[1]
    date <- gsub(".", "", date, fixed=TRUE)
    tab$tourney_date <- date
    tab$year <- year
    
    return(tab)
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



### Function returns the tournaments urls for a given year
ScrapeYearATP <- function(year) {
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

    ## extract the urls to each tournament
    tourney_urls  <- html_nodes(html, "a") %>% html_attr("href") %>% grep(pattern="results$", value=TRUE)

    tab <- data.table(date=date, year=year, tourney_name=title, surface=inout_surf[,2], indoor=inout_surf[,1], commitment=table[,6], 
                      draw_size=dsize, url=paste0("https://www.atptour.com", tourney_urls))
    return(tab)
}




