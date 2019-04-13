## preparing the distributed scraping of the stats
## load functions
source("RFun_DataPrep.R")
source("RFun_Scraping.R")

db <- fread(file = "all_matches_in_atp_db_until_2019_nostat.csv")

to_scrape <- db[url_matches!=""]


cuts <- c(seq(1, nrow(to_scrape), by=40), nrow(to_scrape))
ss <- lapply(seq_along(cuts)[-1], function(i) seq(cuts[i-1], cuts[i]-1))

myseq <-  seq(2514, 1)

## res <- vector(mode="list", length=length(ss))
for (i in myseq) {
    res <- to_scrape[ ss[[i]] ]
    res <- ScrapeMatchStats(res, cores=4)
    cat(paste0(":: scraped chunk ", i, "/",length(ss), "\n"))
    fwrite(res, file = sprintf("Data/chunk_%04d.csv", i), eol="\n")
}




##### From here bits and pieces to resume/fill in the missing
lf <- list.files(path="chunks", pattern="chunk_[[:digit:]]{4}\\.csv", full.names=TRUE)
lf_nonfull <- list.files(path="chunks", pattern="chunk_[[:digit:]]{4}\\.csv", full.names=FALSE)
lf_nonfull <- sub("chunk_","",lf_nonfull, fixed=TRUE)
lf_nonfull <- sub(".csv","",lf_nonfull, fixed=TRUE)
ll <- as.integer(lf_nonfull)
missing <- which(is.na(match(seq(1,2558), ll)))

for (i in missing) {
    res <- to_scrape[ ss[[i]] ]
    res <- ScrapeMatchStats(res, cores=4)
    cat(paste0(":: scraped chunk ", i, "/",length(ss), "\n"))
    fwrite(res, file = sprintf("Data/chunk_%04d.csv", i), eol="\n")
}

## read the data
read_timing <- system.time(res <- lapply(lf, data.table::fread))
    
## these are the matches we scraped for    
inds <- which(! db$url_matches=="")
length(inds)

## make a copy of the db
db2 <- db
## I actually forgot the very last match 
last <- ScrapeMatch(to_scrape[102303,"url_matches"])

## flatten, add the last matches
data <- data.table::rbindlist(res, use.names=TRUE, fill=T, idcol=F)[1:102264]

# data <- rbind(data, c(to_scrape[102303,],last)[-14])

## add new columns to thre original db
new <- which(! colnames(data) %in% colnames(db))
for (i in new) db2[, colnames(data)[i]:=NA_character_]

## place the scraped data at the right place
set(db2, i=inds, j=colnames(data)[new], data[, ..new])
## remove the url_matches column
set(db2, , "url_matches", NULL)

## save it
fwrite(db2, file = "Data/dbtml.csv", eol="\n")
# fwrite(db, file = "Data/dbtml2.csv", eol="\n")

