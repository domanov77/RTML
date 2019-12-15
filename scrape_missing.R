## load functions
source("RFun_DataPrep.R")
source("RFun_Scraping.R")

mt <- read.table("Missing.txt", sep=",")
years <- sort(unique(mt[,2]))

library(parallel)
ally <- mclapply(years, ScrapeYear, mc.cores=4)

## AllTourneys <- rbindlist(ally)
## fwrite(AllTourneys,"MissingTourneys.csv")

to_scrape <- fread("MissingTourneys.csv")
AllRes <- mclapply(to_scrape$url, ScrapeTourney, mc.cores=4)

exc <- which(is.na(AllRes))
AllRes[exc] <- NULL
to_scrape <- to_scrape[-exc,]

newmatches <- rbindlist(lapply(seq_along(to_scrape$year), function(i) add_info_from_tour(tourney=to_scrape[i], matches=AllRes[[i]])))

newmatches$score <- gsub( "\r\n", "",newmatches$score) 
newmatches$score <- gsub( "   ", " ", newmatches$score) 


res <- ScrapeRankingForMatches(newmatches, save_html=FALSE)

final <- AddPlayerInfo(res, save=TRUE)
 
db <- ReadData("Data/dbtml.csv", davis=FALSE)
db1 <- db[1,]
newdb <- AppendMatches(final, db1)
newdb <- newdb[-1,]
fwrite(newdb, "20191215_MissingMatches.csv")
fwrite(to_scrape, "20191215_NewlyScrapedTourneys.csv")

