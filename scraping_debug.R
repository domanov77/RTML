## load functions
source("RFun_DataPrep.R")
source("RFun_Scraping.R")


### scraping tournament results from ATP website
## e.g.,Paris 2017: https://www.atptour.com/en/scores/archive/paris/352/2017/results
res_paris_2017 <- ScrapeTourney("https://www.atptour.com/en/scores/archive/paris/352/2017/results", id="Paris Masters")
url <- res_paris_2017[1, "url_matches"]



url <- "https://www.atptour.com/en/scores/2017/352/QS023/match-stats?isLive=False"
   uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
    page_url <- url
    
    response <- GET(page_url, user_agent(uastring))
    html <- read_html(response)

    allnodes <- html %>% html_nodes("*") %>% html_attr("class") %>% unique()

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
        win <- html_nodes(html, ".match-info-row")%>% html_text(trim=TRUE) 
        win <- gsub("[[:space:]]+"," ", win) 
        win <- gsub("^.*Match[[:space:]](.*?)\\..*$","\\1",win)
    }
        
    win <- html_nodes(html, ".won-game")%>% html_text(trim=TRUE)
    win <- gsub("^.*\\.[[:space:]]","",win)
    
    agrepl(win, player1, ignore.case = TRUE)
    agrepl(win, player2, ignore.case = TRUE)
    pmatch(win, player1)
agrep(win, player2, ignore.case = FALSE, value = TRUE)







res_paris_2017 <- ScrapeMatchStats(res_paris_2017, cores=4)

## currently playing tournament, Houston (as of 20190409)
houston <- ScrapeTourney("https://www.atptour.com/en/scores/current/houston/717/results")
ScrapeYear(2019)

year <- 2019
    require(rvest)
    require(httr)

    uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
    page_url <- paste0("https://www.atptour.com/en/scores/results-archive?year=", year)
    
    response <- GET(page_url, user_agent(uastring))
    html <- read_html(response)
        
    allnodes <- html %>% html_nodes("*") %>% html_attr("class") %>% sort() %>% unique()

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
    tourney_urls  <- html_nodes(html, "a") %>% html_attr("href") %>% grep(pattern="results$|live-scores$", value=TRUE)


url <- "https://www.atptour.com/en/scores/current/houston/717/results"

    uastring <- "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2228.0 Safari/537.36"
    response <- GET(as.character(url), user_agent(uastring))
    html <- read_html(response)

    table <- html_nodes(html, "table.day-table")%>% html_table(header=FALSE)
    
    if (length(table)==0) {
        cat(":: No results present\n")
        return(NA_character_)
    }
    
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
    rounds <- rep(NA_character_,nrow(table))






## this link works as well
houston2 <- ScrapeTourney("https://www.atptour.com/en/scores/current/houston/717/live-scores")


## scrape the match stats for a given tournament 
## (actually for any table containing url_matches field)
## NB: PARALLELIZED (WIN/Linux/Mac) with cores defaulting to 8 
## to make many concurrent requests. It is safe to use much larger values
## (e.g. 32)
houston <- ScrapeMatchStats(houston, cores=8)
houston <- ScrapeMatchStatsSerial(houston)


### Append the matches freshly scraped to the big db
# dtnew <- AppendMatches(houston, dbtop)


### Scrape all tournaments played in a given year
atp2016 <- ScrapeYear(year=2016)

### Now scrape all matches from these tournaments
## This can beparallelized!!
## AllRes2016 <- mclapply(atp2016$url, ScrapeTourney, mc.cores=24)
AllRes2016 <- lapply(atp2016$url, ScrapeTourney)

## flatten this list into a big data.table
atp_matches_2016_lapply <- data.table(rbindlist(AllRes2016))

### The same but with a for cicle, so to know what's going on
list_tourney_matches <- vector(mode="list", length=length(atp2016$url))
for (i in seq_along(atp2016$url)) {
    list_tourney_matches[[i]] <- ScrapeTourney(atp2016$url[i])
    cat(paste(":: ", i, ")", atp2016$tourney_name[i], "[done] \n"))
}

## Flatten the list
atp_matches_2016_for <- data.table(rbindlist(list_tourney_matches))

## Check if the two methods give identical results
all.equal(atp_matches_2016_for, atp_matches_2016_lapply) ## TRUE



## Fixed data retrival error!!! Before we did not check if the winner was always 
## the left player in the stats page, now this is fixed

## Check with Brisbane 2019
brisb <- ScrapeTourney(url="https://www.atptour.com/en/scores/archive/brisbane/339/2019/results")
brisb <- ScrapeMatchStats(brisb)

## Check with the live-score of current tourney: Marrakech
marrakech <- ScrapeMatchStats(ScrapeTourney(url="https://www.atptour.com/en/scores/current/marrakech/360/live-scores"))
## try the pipe syntax
marrakech2 <- ScrapeTourney(url="https://www.atptour.com/en/scores/current/marrakech/360/live-scores") %>% ScrapeMatchStats() 

all.equal(marrakech, marrakech2) ## true
OutputTableToPng(marrakech, "marrakech.png")


######## From here on: make sure you know what you are doing!!!!


### Retrieve ALL YEARS from 1915 to 1918
years <- seq(1915, 2018)
dbyears <- vector(mode="list", length=length(years))
for (i in seq_along(years)) {
    dbyears[[i]] <- ScrapeYear(years[i])
    cat(paste(":: ", i, ")", years[i], "[done] \n"))
}

alltourn <- rbindlist(dbyears)


## The data.table alltourn (4101 rows) as of today 2019 04 09
## contains all urls for tourney results. Now we can scrape for the tournaments
## and afterwards for each single match!


### scrape the tournaments
tourneys <- vector(mode="list", length=length(alltourn$url))
for (j in seq(1, length(alltourn$url))) {
    tourneys[[j]] <- ScrapeTourney(alltourn$url[j])
    cat(paste(":: ", j, ")", alltourn$year[j], alltourn$tourney_name[j], "[done] \n"))
}
fwrite(alltourn, file = "all_tourneys_in_atp_db_until_2018.csv", eol="\n")

### This is parallelized with mclapply; windows users should use foreach/dopar/doparallel 
tourneys_lapply <-  mclapply(alltourn$url, ScrapeTourney, mc.cores=24)

## sometimes scraping doesn't find anything and it just returns "NA"; find those and remove them
ind_nodata <- which(is.na(tourneys_lapply))
tourneys_lapply[ind_nodata] <- NULL

## flatten the list with correct names!
all_matches_in_atp_db <- rbindlist(tourneys_lapply, use.names=TRUE)

## write everything in a csv
fwrite(all_matches_in_atp_db, file = "all_matches_in_atp_db_until_2018_nostat.csv", eol="\n")

