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
dbyears[[105]] <- ScrapeYear(2019)
alltourn <- rbindlist(dbyears)

fwrite(alltourn, file = "all_tourneys_in_atp_db_until_2019.csv", eol="\n")

## The data.table alltourn (4101 rows) as of today 2019 04 09
## contains all urls for tourney results. Now we can scrape for the tournaments
## and afterwards for each single match!

alltourn <- fread(file = "all_tourneys_in_atp_db_until_2019.csv",na.strings=NA_character_)


### scrape the tournaments
tourneys <- vector(mode="list", length=length(alltourn$url))
for (j in seq(1, length(alltourn$url))) {
    tourneys[[j]] <- ScrapeTourney(alltourn$url[j])
    cat(paste(":: ", j, ")", alltourn$year[j], alltourn$tourney_name[j], "[done] \n"))
}

s70 <- ScrapeYear(1970)

### This is parallelized with mclapply; windows users should use foreach/dopar/doparallel 
tourneys_lapply <-  parallel::mclapply(alltourn$url, ScrapeTourney, mc.cores=8)

## sometimes scraping doesn't find anything and it just returns "NA"; find those and remove them
ind_nodata <- which(is.na(tourneys_lapply))
tourneys_lapply[ind_nodata] <- NULL

## short names
tt <- tourneys_lapply
at <- alltourn[-ind_nodata,]

tour <- alltourn[3]
matches <- tt[[3]]
is.data.table(tour)
is.data.table(matches)

add_info <- function(tour, matches) {
    matches$year <- tour$year
    matches$date <- tour$date
    matches$indoor <- tour$indoor
    matches$commitment <- tour$commitment
    matches$draw_size <-  tour$draw_size
    return(matches)
}

allmat <- lapply(seq_along(tt), function(i) add_info(tour=at[i], matches=tt[[i]]))

all_matches_in_atp_db <- rbindlist(allmat, use.names=TRUE)

setcolorder(all_matches_in_atp_db, c("year","date","tourney_name","surface","indoor","commitment", 
                                     "draw_size", "round","winner_seed","winner_name","score", 
                                     "loser_seed","loser_name", "url_matches"  ))
                                     
                                     
save(list=c("tourneys_lapply", "ind_nodata", "alltourn","dbyears","all_matches_in_atp_db"), file="20190412_Dump.Rdata")

## to eventually load back the results in anorher R session:
# load("20190412_Dump.Rdata")

## write everything in a csv
fwrite(all_matches_in_atp_db, file = "all_matches_in_atp_db_until_2019_nostat.csv", eol="\n")

all_matches_in_atp_db_stats <- ScrapeMatchStats(all_matches_in_atp_db, cores=24)

db <- fread(file = "all_matches_in_atp_db_until_2019_nostat.csv")
to <- fread(file = "all_tourneys_in_atp_db_until_2019.csv")

a <- all_matches_in_atp_db[, .N, by=year]


db <- fread(file = "all_matches_in_atp_db_until_2019_nostat.csv")
fedal <- h2h("Roger Federer", "Rafael Nadal", db)
dcast(fedal[, .N, by=.(winner_name, surface, indoor)], winner_name ~ surface + indoor, value.var="N")
fedal <- ScrapeMatchStats(fedal)

dcast(fedal[, .N, by=.(winner_name, surface)], winner_name ~ surface, value.var="N")
fedal[, .N, by=.(winner_name)]


## preparing the distributed scraping of the stats
## load functions
source("RFun_DataPrep.R")
source("RFun_Scraping.R")

db <- fread(file = "all_matches_in_atp_db_until_2019_nostat.csv")

to_scrape <- db[url_matches!="",]
cuts <- c(seq(1, nrow(to_scrape), by=100), nrow(to_scrape))
ss <- lapply(seq_along(cuts)[-1], function(i) seq(cuts[i-1], cuts[i]-1))

res <- vector(mode="list", length=length(ss))
for (i in seq_along(ss)) {
    res[[i]] <- to_scrape[ ss[[i]] ]
    res[[i]] <- ScrapeMatchStats(res[[i]])
    cat(paste0(":: scraped chunk ", i, "/",length(ss), "\n"))
    fwrite(res[[i]], file = paste0("Data/Chunk", i,".csv"), eol="\n")
}
