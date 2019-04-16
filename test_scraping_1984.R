## load functions
source("RFun_DataPrep.R")
source("RFun_Scraping.R")

### Scrape all tournaments played in a given year
atp1984 <- ScrapeYear(year=1984)

## Rotterdam is number 9
rot1984 <- ScrapeTourney(atp1984$url[9])
rot1984 <- ScrapeMatchStats(rot1984, cores=12)


OutputTableToPng(rot1984, "Rotterdam-1984.png")


## Tolosa 1984
tolo <- ScrapeTourney(atp1984$url[69])
tolo <- ScrapeMatchStats(tolo, cores=12)

y2019 <- ScrapeYear(year=2019)
tmp <- ScrapeTourney(y2019$url[2])
tmp <- ScrapeMatchStats(tmp, cores=12)
OutputTableToPng(tmp, "Brisbane2019.png")


## Kitzbuhel 1970
atp1970 <- ScrapeYear(year=1970)
tmp <- ScrapeTourney(atp1970$url[53])
tmp <- ScrapeMatchStats(tmp, cores=12)
atp1983 <- ScrapeYear(year=1983)
tmp <- ScrapeTourney(atp1983$url[61])
tmp <- ScrapeMatchStats(tmp, cores=12)


## Barcellona 68
y <- ScrapeYear(year=1968) ## not there

tb <- dbtop[year==1968 & tourney_name=="Barcelona", c(2,3,4,5,6,11,21,28,29,30,31)]
OutputTableToPng(tb, "Barca1968.png")


### debug 2019
atp19 <- ScrapeYear(year=2019)
tmp <- ScrapeTourney(atp19$url[1])

url <- atp19$url[1]
