## load functions
source("RFun_DataPrep.R")
source("RFun_Scraping.R")
### Retrieve ALL YEARS from 1915 to 1918
years <- seq(1915, 2019)
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

