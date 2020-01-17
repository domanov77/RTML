## load functions
source("RFun_DataPrep.R")
source("RFun_Scraping.R")

library(parallel)
library(readxl)
options(readxl.show_progress = FALSE)


## Function which performs the substitutions, "derived" from ScrapeRankingForMatches()
FixOfficialRankings <- function(db, list_ranking) {
    fmt_td   <- '%Y%m%d' ## string to interprete numeric date as R's Date
    ranking_dates <- names(list_ranking)
    ranks <- as.Date(as.character(ranking_dates), format=fmt_td)
    matchdate <- as.Date(as.character(db$tourney_date), format=fmt_td)

    ## Consider only matches between first and last ranking
    subset_matches <- which(matchdate > ranks[1] & matchdate < ranks[length(ranks)])
    matches <- db[ subset_matches, ]

    ## tourney date (unique occurrences, to iterate through later)
    unique_tourney_date <- as.Date(as.character(unique(matches$tourney_date)), format=fmt_td)

    ## find which available ranking immediately precedes the "tourney_date"
    ind <- rep(NA_integer_, length(unique_tourney_date))
    for (i in seq_along(unique_tourney_date))
        ind[i] <- which.max(ranks >= unique_tourney_date[i])

    ## unique_tourney_date[i] <-> ranks[ pointer_rank[i] ]
    pointer_rank <- ind-1
    pointer_rank[pointer_rank==0] <- NA

    ## fresh start by copying the data.table
    new <- copy(matches)

    ## prepare vector to store the filename of the ranking applied to the matches
    rank_file <- character(nrow(new))
    for (i in seq_along(pointer_rank)) {
        ## start the iteration
        if (!is.na(pointer_rank[i])) {
            selected_ranking_date <- as.character(ranking_dates[ pointer_rank[i] ])
            cat("Assigning to tourney(s) on ", as.character(unique_tourney_date[i]),
                " the rankings of ", selected_ranking_date, "\n")

            ## find all matches played in the current unique date
            ind_matches <- which(matches$tourney_date==gsub("-","", unique_tourney_date[i],fixed=TRUE))

            ## store the ranking date
            rank_file[ind_matches] <- selected_ranking_date

            w <- new$winner_name[ind_matches]
            l <- new$loser_name[ind_matches]
            ranking <- list_ranking[[selected_ranking_date]]

            indw <- match(w, ranking$player)
            indl <- match(l, ranking$player)

            set(new, ind_matches, "winner_rank", ranking$n[indw])
            set(new, ind_matches, "loser_rank",  ranking$n[indl])
            set(new, ind_matches, "winner_rank_points", ranking$points[indw])
            set(new, ind_matches, "loser_rank_points",  ranking$points[indl])

         } else {
            ## this should not occur!!
            cat(paste("Tourney date ", unique_tourney_date[i], " predates the first ATP ranking!\n"))
        }
    }

    ## reinject the matches into a new copy of the original db
    newdb <- copy(db)
    newdb[subset_matches] <- new

    sel_ranks <- rep(NA_character_, nrow(newdb))
    sel_ranks[subset_matches]  <- gsub("-","_", paste0("Rankings_", as.Date(rank_file, format=fmt_td)), fixed=TRUE)
    newdb[, selected_ranking:=sel_ranks]
    return(newdb)
}


####### application

## Top's new db
db <- fread("Data/newdb3.csv", fill=TRUE)

## convert winner_rank_points and loser_rank_points to double
db <- db[, winner_rank_points:=as.double(winner_rank_points)]
db <- db[, loser_rank_points:=as.double(loser_rank_points)]
## round age to 3 digits
db <- db[, winner_age:=round(winner_age,3)]
db <- db[, loser_age:=round(loser_age,3)]

## read all xlsx which in my pc are in "OfficialRankings/AllRelevants"
filelist <- list.files("OfficialRankings/AllRelevants", full.names = TRUE)
dates    <- gsub("_","", sub(".xlsx","", sub("Rankings_","", list.files("OfficialRankings/AllRelevants", full.names = FALSE)), fixed=TRUE))

## only reading columns A-N starting from 6th row (using cell_limits() in read_xlsx)
col_names <- c("n","player","age","country", "points_prevweek",
                "week_change","gain","eff","drop","points_nextweek","points",
                "tourney_points","bonus","country_rank") ## ,"current_round","current_tourney","past_avg","avg","n_tourney","rank_age_group")

## parallel, not working on Windows. Not much gain tho
tots <- mclapply(filelist, read_xlsx, range=cell_limits(c(6, NA), c(NA, 14)), col_names=col_names, mc.cores=8)

## serial version, works on Windows
## tots <- lapply(filelist, read_xlsx, range=cell_limits(c(6, NA), c(NA, 14)), col_names=col_names)
names(tots) <- dates

## call the function
newdb <- FixOfficialRankings(db, list_ranking=tots)
## remove the "spurious" 'Column 50'
newdb$'Column 50' <- NULL

## write file:
fwrite(newdb, "Data/newdb3_FixedRankings.csv")
db2 <- fread("Data/newdb3_FixedRankings.csv")

############################ NOT RUN ############################
## just some snippet for testing
## length(which(db$winner_rank == newdb$winner_rank))
## ss <- newdb[tourney_date > 19710101 & tourney_date < 19710301, ]
## ss <- newdb[tourney_date > 19770101 & tourney_date < 19770131, ]
## ss <- newdb[tourney_date > 19821201 & tourney_date < 19821231, ]
## ss <- newdb[tourney_date == 19821214]
test <- fread("Data/newdb3_FixedRankings.csv")
