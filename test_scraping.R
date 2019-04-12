## load functions
source("RFun_DataPrep.R")
source("RFun_Scraping.R")


### scraping tournament results from ATP website
## e.g.,Paris 2017: https://www.atptour.com/en/scores/archive/paris/352/2017/results
res_paris_2017 <- ScrapeTourney("https://www.atptour.com/en/scores/archive/paris/352/2017/results", id="Paris Masters")

## add the stats
res_paris_2017 <- ScrapeMatchStats(res_paris_2017, cores=4)

## serial version, for debugging
## res_paris_2017 <- ScrapeMatchStatsSerial(res_paris_2017)

## currently playing tournament, Houston (as of 20190409)
houston <- ScrapeTourney("https://www.atptour.com/en/scores/current/houston/717/results")

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

## Check with the live-score of current tourney: Marrakech
marrakech <- ScrapeMatchStats(ScrapeTourney(url="https://www.atptour.com/en/scores/current/marrakech/360/live-scores"))
## try the pipe syntax
marrakech2 <- ScrapeTourney(url="https://www.atptour.com/en/scores/current/marrakech/360/live-scores") %>% ScrapeMatchStats() 

all.equal(marrakech, marrakech2) ## true
OutputTableToPng(marrakech, "marrakech.png")
