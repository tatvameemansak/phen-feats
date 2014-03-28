# Meds for cases and controls
# pick these from the notes that were selected
# from each patient.

library(RMySQL)

setwd("/home/vibhu/Documents/scpd/bmi217/code")

poi <- "mg"

casePids <- scan(sprintf("%s_case_pidsforfeat.txt", poi))
casePids <- as.numeric(casePids)
ctrlPids <- scan(sprintf("%s_ctrl_pidsforfeat.txt", poi))
ctrlPids <- as.numeric(ctrlPids)

# cut off values for cases
ncdata <- scan(sprintf("%s_nc.txt", poi))
ncdata <- matrix(ncdata, nrow=1000, byrow=T)

con <- dbConnect(MySQL(), 
                 dbname='stride5',
                 host='ncbolabs-db1')

# Meds for case
caseSet <- paste(casePids, collapse=",")
q1 <- sprintf("select prescription.pid, prescription.drug_description, prescription.timeoffset from prescription inner join
              (select distinct pid from prescription where pid in (%s)) p
              on p.pid=prescription.pid;", caseSet) 
casemeds <- dbSendQuery(con, q1)
caseMedsList <- fetch(casemeds, n=-1)

# Remove drugs taken below time zero cutoff
pat <- unique(caseMedsList[,1])

CutOffMeds <- function(x){
  cutoff <- ncdata[(ncdata[,1]==x),2]
  ind <- which(caseMedsList[,1]==x)
  rmv <- sapply(as.list(ind), function(y) caseMedsList[y, 3]<cutoff)
  indrmv <- ind[unlist(rmv)]
  if (length(indrmv)==0) return(0)
  caseMedsList <<- caseMedsList[-indrmv,]
  return(length(indrmv))
}
co <- sapply(as.list(pat), CutOffMeds)

# Remove formulation and qty information from drugs
clean <- apply(caseMedsList, 1, function(x) gsub("[[:digit:]]+[[:print:]]+$","", x[2]))
clean <- lapply(clean, function(x) gsub(" +$","", x))
caseMedsList[,2] <- unlist(clean)

# Codes for controls
ctrlSet <- paste(ctrlPids, collapse=",")
q2 <- sprintf("select prescription.pid, prescription.drug_description from prescription inner join
              (select distinct pid from prescription where pid in (%s)) p
              on p.pid=prescription.pid;", ctrlSet) 
ctrlmeds <- dbSendQuery(con, q2)
ctrlMedsList <- fetch(ctrlmeds, n=-1)
# Remove formulation and qty information from drugs
clean <- apply(ctrlMedsList, 1, function(x) gsub("[[:digit:]]+[[:print:]]+$","", x[2]))
clean <- lapply(clean, function(x) gsub(" +$","", x))
ctrlMedsList[,2] <- unlist(clean)

dbDisconnect(con)

allMeds <- c(caseMedsList[,2], ctrlMedsList[,2])
medSet <- unique(allMeds)

tab <- table(factor(allMeds, medSet))
rmv <- which(tab<12)

#pruned set
medSet <- medSet[-rmv]

pats <- unique(caseMedsList[,1])
numCases <- length(pats)

ctrls <- unique(ctrlMedsList[,1])
numCtrls <- length(unique(ctrls))

featureMeds <- matrix(rep(0, length(medSet)*(numCases+numCtrls)),
                      ncol=length(medSet))

GetFeats <- function(x){
  # Whether each drub in the medSet is present/absent
  drugs <- frame[(frame[,1]==x),2]
  feats <- medSet %in% drugs
  return(as.numeric(feats))
}

frame <- caseMedsList
featureMeds[(1:numCases),] <- t(sapply(as.list(pats), GetFeats))

frame <- ctrlMedsList
featureMeds[(numCases+1):(numCases+numCtrls),] <- t(sapply(as.list(ctrls), GetFeats))

ids <- c(pats, ctrls)
labels <- c(rep(1, numCases), rep(0, numCtrls))
xData <- cbind(ids, featureMeds, labels)
write(t(xData), file=sprintf("%s_med_feats.txt", poi),
      ncolumns=dim(xData)[2])
