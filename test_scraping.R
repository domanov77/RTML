## load functions
source("RFun_DataPrep.R")
source("RFun_Scraping.R")


### scraping tournament results from ATP website
## e.g.,Paris 2017: https://www.atptour.com/en/scores/archive/paris/352/2017/results
res_paris_2017 <- ScrapeTourney("https://www.atptour.com/en/scores/archive/paris/352/2017/results", id="Paris Masters")
res_paris_2017 <- AddMatchStats(res_paris_2017, cores=4)


### Retrieve all tournaments played in a given year
atp2016 <- ScrapeYear(year=2016)

## currently playing tournament, Houston (as of 20190409)
houston <- ScrapeTourney("https://www.atptour.com/en/scores/current/houston/717/results")
houston <- AddMatchStats(houston, cores=8)

dt <- AppendMatches(houston, tt)


### This can be parallelized
AllRes2016 <- lapply(atp2016$url, ScrapeTourney)
## flatten this list into a big data.table
atp_matches_2016_lapply <- data.table(rbindlist(AllRes2016))


### The same but with a for cicle, so to know what's going on
tots <- vector(mode="list", length=length(atp2016$url))
for (i in seq_along(atp2016$url)) {
    tots[[i]] <- ScrapeTourney(atp2016$url[i])
    cat(paste(":: ", i, ")", atp2016$tourney_name[i], "[done] \n"))
}
atp_matches_2016_for <- data.table(rbindlist(tots))

all.equal(atp_matches_2016_for, atp_matches_2016_lapply) ## TRUE

tots <- vector(mode="list", length=nrow(atp_matches_2016_for))
for (i in seq_along(atp_matches_2016_for$url_matches)) {
    tots[[i]] <- ScrapeMatch(atp_matches_2016_for$url_matches[i])
    cat(paste(":: ", i, ")", atp_matches_2016_for$tourney_name[i], " - ", atp_matches_2016_for$winner_name[i]," vs ", atp_matches_2016_for$loser_name[i], "[done] \n"))
}
match_stats  <- data.table(rbindlist(tots))
sapply(tots, rbind)
match_stats <- parallel::mclapply(atp_matches_2016_for$url_matches, ScrapeMatch, mc.cores=32)

ms <- data.table(do.call("rbind",match_stats))
atp_matches_2016_for <- cbind(atp_matches_2016_for, ms) 
atp_matches_2016_for[, url_matches:=NULL]



### ALL YEARS
years <- 
tots <- vector(mode="list", length=length(atp2016$url))
for (i in seq_along(atp2016$url)) {
    tots[[i]] <- ScrapeTourneyFromATP(atp2016$url[i])
    cat(paste(":: ", i, ")", atp2016$tourney_name[i], "[done] \n"))
}


