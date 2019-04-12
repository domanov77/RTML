## preparing the distributed scraping of the stats
## load functions
source("RFun_DataPrep.R")
source("RFun_Scraping.R")

db <- fread(file = "all_matches_in_atp_db_until_2019_nostat.csv")

to_scrape <- db[url_matches!=""]


cuts <- c(seq(1, nrow(to_scrape), by=40), nrow(to_scrape))
ss <- lapply(seq_along(cuts)[-1], function(i) seq(cuts[i-1], cuts[i]-1))

myseq <-  seq(2514, 1)

res <- vector(mode="list", length=length(ss))
for (i in myseq) {
    res[[i]] <- to_scrape[ ss[[i]] ]
    res[[i]] <- ScrapeMatchStats(res[[i]], cores=4)
    cat(paste0(":: scraped chunk ", i, "/",length(ss), "\n"))
    fwrite(res[[i]], file = sprintf("Data/chunk_%04d.csv", i), eol="\n")
}

