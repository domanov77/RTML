## load functions
source("RFun_DataPrep.R")
source("RFun_Scraping.R")

db <- ReadData("Data/dbtml.csv", davis=FALSE, quali=TRUE, current=TRUE)


db <- UpdateDB(db, write_ended=TRUE, write_current=TRUE)


## bug fix
db <- ReadData("Data/dbtml.csv", davis=FALSE, quali=TRUE, current=FALSE)
colnames(db)[41]
db[tourney_id=="1991_329" & winner_name=="Stefan Edberg" & loser_name=="Ivan Lendl", l_bpSaved:=999] ## & is.na(l_bpFaced)]

db[tourney_id=="2016_96" & winner_name=="Rogerio Dutra Silva" & round=="R64" , w_bpSaved:=999] ## & is.na(l_bpFaced)]
db[tourney_id=="2016_96" & winner_name=="Rogerio Dutra Silva" & round=="R64" , l_bpSaved:=999] ## & is.na(l_bpFaced)]
db[tourney_id=="2016_96" & winner_name=="Rogerio Dutra Silva" & round=="R64" , w_bpFaced:=999] ## & is.na(l_bpFaced)]
db[tourney_id=="2016_96" & winner_name=="Rogerio Dutra Silva" & round=="R64" , l_bpFaced:=999] ## & is.na(l_bpFaced)]
db[tourney_id=="2016_96" & winner_name=="Rogerio Dutra Silva" & round=="R64" , w_SvGms:=999] ## & is.na(l_bpFaced)]
db[tourney_id=="2016_96" & winner_name=="Rogerio Dutra Silva" & round=="R64" , l_SvGms:=999] ## & is.na(l_bpFaced)]

db[tourney_id=="2016_96" & winner_name=="Gilles Simon" & round=="R64", w_bpSaved:=999] ## & is.na(l_bpFaced)]
db[tourney_id=="2016_96" & winner_name=="Gilles Simon" & round=="R64", l_bpSaved:=999] ## & is.na(l_bpFaced)]
db[tourney_id=="2016_96" & winner_name=="Gilles Simon" & round=="R64", w_bpFaced:=999] ## & is.na(l_bpFaced)]
db[tourney_id=="2016_96" & winner_name=="Gilles Simon" & round=="R64", l_bpFaced:=999] ## & is.na(l_bpFaced)]
db[tourney_id=="2016_96" & winner_name=="Gilles Simon" & round=="R64", w_SvGms:=999] ## & is.na(l_bpFaced)]
db[tourney_id=="2016_96" & winner_name=="Gilles Simon" & round=="R64", l_SvGms:=999] ## & is.na(l_bpFaced)]

db[w_SvGms==0 | l_SvGms ==0]
fwrite(db, "Data/dbtml.csv", quote=FALSE)
