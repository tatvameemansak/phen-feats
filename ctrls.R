# The strategy to obtain controls is to sample a very large
# pool of patient ids and then randomly pick a few from this pool
# Before adding these to the control pids list, we will check if they 
# belong to any of the phenotype pid lists that we have prepared.

library(RMySQL)

setwd("/home/vibhu/Documents/scpd/bmi217/code")

# gbm cases
gbmCases <- scan("gbm_pids.txt")
gbmCtrls <- vector()

# dr cases
drCases <- scan("dr_pids.txt")
drCtrls <- vector()

# mg cases
mgCases <- scan("mg_pids.txt")
mgCtrls <- vector()

con <- dbConnect(MySQL(), 
                 dbname='stride5',
                 host='ncbolabs-db1')

# Get a million pids

q1 <- "select distinct pid from note limit 1000000"
pids <- dbSendQuery(con, q1)
pidList <- fetch(pids, n=-1)

# We sample 50K pids randomly
candidates <- sample(pidList[,1], 50000)

# gbm ctrls
mgbm <- which(candidates %in% gbmCases)
if (length(mgbm)==0){
  gbmCtrls <- c(gbmCtrls, candidates)
} else {
  gbmCtrls <- c(gbmCtrls, candidates[-mgbm])
}

# We sample 100K pids randomly
candidates <- sample(pidList[,1], 100000)

# dr ctrls
mdr <- which(candidates %in% drCases)
if (length(mdr)==0){
  drCtrls <- c(drCtrls, candidates)
} else {
  drCtrls <- c(drCtrls, candidates[-mdr])
}

# We sample 100K pids randomly
candidates <- sample(pidList[,1], 100000)

# mg ctrls
mmg <- which(candidates %in% mgCases)
if (length(mmg)==0){
  mgCtrls <- c(mgCtrls, candidates)
} else {
  mgCtrls <- c(mgCtrls, candidates[-mmg])
}

# Write out the data

write(t(gbmCtrls), file="gbm_ctrl_pids.txt", ncolumns=1)
write(t(drCtrls), file="dr_ctrl_pids.txt", ncolumns=1)
write(t(mgCtrls), file="mg_ctrl_pids.txt", ncolumns=1)

dbDisconnect(con)
