## load functions
source("RFun_DataPrep.R")
source("RFun_ScrapingATP.R")


### scraping tournament results from ATP website
## e.g.,Paris 2017: https://www.atptour.com/en/scores/archive/paris/352/2017/results

res_paris_2017 <- ScrapeTourneyFromATP("https://www.atptour.com/en/scores/archive/paris/352/2017/results", id="Paris Masters")



### Retrieve all tournaments played in a given year
atp2016 <- ScrapeYearATP(year=2016)

### This can be parallelized
AllRes2016 <- lapply(atp2016$url, ScrapeTourneyFromATP)
## flatten this list into a big data.table
atp_matches_2016_lapply <- data.table(rbindlist(AllRes2016))


### The same but with a for cicle, so to know what's going on
tots <- vector(mode="list", length=length(atp2016$url))
for (i in seq_along(atp2016$url)) {
    tots[[i]] <- ScrapeTourneyFromATP(atp2016$url[i])
    cat(paste(":: ", i, ")", atp2016$tourney_name[i], "[done] \n"))
}
atp_matches_2016_for <- data.table(rbindlist(tots))

all.equal(atp_matches_2016_for, atp_matches_2016_lapply) ## TRUE


