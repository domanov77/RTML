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

fwrite(alltourn, file = "20190620_all_tourneys_in_atp_db_until_today.csv", eol="\n", quote=FALSE)

## The data.table alltourn (4101 rows) as of today 2019 04 09
## contains all urls for tourney results. Now we can scrape for the tournaments
## and afterwards for each single match!

prev <- fread(file = "all_tourneys_in_atp_db_until_today.csv",na.strings=NA_character_)

### scrape the tournaments
## tourneys <- vector(mode="list", length=length(alltourn$url))
## 
## for (j in seq(356, length(alltourn$url))) {
##     tourneys[[j]] <- ScrapeTourney(alltourn$url[j])
##     cat(paste(":: ", j, ")", alltourn$year[j], alltourn$tourney_name[j], "[done] \n"))
## }

### This is parallelized with mclapply; windows users should use foreach/dopar/doparallel 

##fix all 1975
alltourn <- fread(file = "20190620_all_tourneys_in_atp_db_until_today.csv",na.strings=NA_character_)
alltourn <- alltourn[year==1975]
tourneys_lapply <-  parallel::mclapply(alltourn$url, ScrapeTourney, save_html=TRUE, mc.cores=8)

### this isn't needed anymore
## tourneys_lapply_id <-  parallel::mclapply(alltourn$url, ScrapeIdsFromTourney, mc.cores=4)
## ts <- vector(mode="list", length=length(alltourn$url))
## for (j in rev(seq(1, length(alltourn$url)))) {
##     ts[[j]] <- ScrapeIdsFromTourney(alltourn$url[j])
##     cat(paste(":: ", j, ")", alltourn$year[j], alltourn$tourney_name[j], "[done] \n"))
## }
##  
## lapply(tourneys_lapply_id, function(x) set(x, , "match_id", NULL))

save(list=c("tourneys_lapply"), file="dump.Rdata")

## sometimes scraping doesn't find anything and it just returns "NA"; find those and remove them
ind_nodata <- which(is.na(tourneys_lapply ))
tourneys_lapply[ind_nodata] <- NULL

## ind_no <- which(is.na(tourneys_lapply_id))
## tourneys_lapply_id[ind_nodata] <- NULL

## fix 2019 in all_matches_in_atp_db_until_today_nostat.csv
## tt <- parallel::mclapply(ScrapeYear(2019)$url, ScrapeTourney, mc.cores=8)
## ind_nodata <- which(is.na(tt))
## at <- rbindlist(tt[-ind_nodata])
## tod <- fread("all_matches_in_atp_db_until_today_nostat.csv")
## tod[year==2019, "round":=at$round]
## tod <- fwrite(tod, "all_matches_in_atp_db_until_today_nostat.csv", quote=FALSE)


add_info_from_tour <- function(tourney, matches) {
    matches$year <- tourney$year
    matches$date <- tourney$date
    matches$indoor <- tourney$indoor
    matches$surface <- tourney$surface
    matches$commitment <- tourney$commitment
    matches$draw_size <-  tourney$draw_size
    matches$tourney_id <- tourney$tourney_id 
    matches$tourney_name <- tourney$tourney_name
   return(matches)
}

tt <- rbindlist(tourneys_lapply)
at <- alltourn[-ind_nodata]

allmat <- lapply(seq_along(tourneys_lapply), function(i) add_info_from_tour(tour=at[i], matches=tourneys_lapply[[i]]))
all_1975 <- rbindlist(allmat, use.names=TRUE, fill=TRUE)

db <- ReadData()
row_ind <- db[,.I[year== 1975L]] ## Retrieve row number
db[row_ind]
## check the rows of the two!
set(db, row_ind, names(all_1975), as.list(all_1975))

db[db$tourney_id==""]
set(db, url_matches:=NULL)
set( db, j=c("url_matches","tourney_id_from_url"), value=NULL )
fwrite(db, "Data/dbtml.csv", quote=FALSE)


all_matches_in_atp_db <- rbindlist(allmat, use.names=TRUE, fill=TRUE)

db2 <- all_matches_in_atp_db 

db19 <- rbindlist(tt, use.names=TRUE, fill=TRUE)

setcolorder(all_matches_in_atp_db, c("year","date","tourney_name","tourney_id", "tourney_id_from_url","surface","indoor","commitment", 
                                     "draw_size", "round","winner_id","winner_seed", "winner_name","score", 
                                     "loser_id","loser_seed","loser_name", "url_matches"  ))
                                     
             
save(list=c("tourneys_lapply", "ind_nodata", "alltourn","all_matches_in_atp_db"), file="20190415_Dump.Rdata")

## to eventually load back the results in another R session:
# load("20190412_Dump.Rdata")

all_matches_in_atp_db[tourney_id!=tourney_id_from_url] ## ok, only marrakech and houston fail


## write everything in a csv
fwrite(all_matches_in_atp_db, file = "all_matches_in_atp_db_until_20190620_nostat.csv", eol="\n")


atp <- all_matches_in_atp_db
db <- ReadData("Data/dbtml.csv")

dbid <- unique(db$tourney_id)
idsc <- unique(atp$tourney_id)[1:4051]

db[db$tourney_id==""]









###################### from here on the old version with separated id scraping
## short names
tt <- tourneys_lapply
at <- alltourn[-ind_nodata,]
tti <- tourneys_lapply_id

tour <- alltourn[3]
matches <- tt[[3]]
is.data.table(tour)
is.data.table(matches)

add_info <- function(tour, matches, it) {
    matches$year <- tour$year
    matches$date <- tour$date
    matches$indoor <- tour$indoor
    matches$commitment <- tour$commitment
    matches$draw_size <-  tour$draw_size
    if (nrow(matches)>nrow(it)) {
        cat(":: tid ",it$tourney_id[1], "\n")
              it <- rbind(matrix(NA_character_, ncol=ncol(it), nrow=nrow(matches)-nrow(it)), it, fill=T)
    }  
     matches$winner_id <-  it$winner_id 
    matches$loser_id <-  it$loser_id 
    matches$tourney_id <-  it$tourney_id 
   return(matches)
}

rr <- vector(mode="list", length=length(tt))

for (i in seq_along(tt)){
   cat(paste(":: n ", i, "\n"))
   rr[[i]] <- add_info(tour=at[i], matches=tt[[i]], it=tti[[i]])
}


allmat <- lapply(seq_along(tt), function(i) add_info(tour=at[i], matches=tt[[i]], it=tti[[i]]))

all_matches_in_atp_db <- rbindlist(allmat, use.names=TRUE, fill=TRUE)
all_matches_in_atp_db$commitment <- gsub(",", "", all_matches_in_atp_db$commitment )

setcolorder(all_matches_in_atp_db, c("year","date","tourney_name","tourney_id", "surface","indoor","commitment", 
                                     "draw_size", "round","winner_id","winner_seed", "winner_name","score", 
                                     "loser_id","loser_seed","loser_name", "url_matches"  ))
                                     
             
save(list=c("tourneys_lapply", "ind_nodata", "alltourn","dbyears","all_matches_in_atp_db"), file="20190412_Dump.Rdata")

## to eventually load back the results in anorher R session:
# load("20190412_Dump.Rdata")
