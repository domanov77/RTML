## load functions
source("RFun_DataPrep.R")
source("RFun_Scraping.R")


### scraping tournament results from ATP website
## e.g.,Paris 2017: https://www.atptour.com/en/scores/archive/paris/352/2017/results
res_paris_2017 <- ScrapeTourney("https://www.atptour.com/en/scores/archive/paris/352/2017/results", id="Paris Masters")
res_paris_2017 <- ScrapeMatchStats(res_paris_2017, cores=4)

## currently playing tournament, Houston (as of 20190409)
houston <- ScrapeTourney("https://www.atptour.com/en/scores/current/houston/717/results",
                         id="Houston")

## scrape the match stats for a given tournament 
## (actually for any table containing url_matches field)
## NB: PARALLELIZED (WIN/Linux/Mac) with cores defaulting to 8 
## to make many concurrent requests. It is safe to use much larger values
## (e.g. 32)
houston <- ScrapeMatchStats(houston, cores=8)


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

