# icd9 codes for cases and controls
# pick these from the notes that were selected
# from each patient.

library(RMySQL)

setwd("/home/vibhu/Documents/scpd/bmi217/code")

poi <- "mg"

# ids and note counts for cases
ncdata <- scan(sprintf("%s_nc.txt", poi))
ncdata <- matrix(ncdata, nrow=1000, byrow=T)

ctrlFile <- sprintf("%s_ctrl_terms.txt", poi)

con  <- file(ctrlFile, open = "r")
ctrlPids <- vector()
casePids <- vector()

while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
  frame <- (strsplit(oneLine, " "))
  frame <- as.numeric(unlist(frame))
  
  casePids <- c(casePids, frame[1])
  ctrlPids <- c(ctrlPids, frame[2])

} 

close(con)

# Write out the case and control pids for easier access later
write(t(casePids),
      file=sprintf("%s_case_pidsforfeat.txt", poi))
write(t(ctrlPids),
      file=sprintf("%s_ctrl_pidsforfeat.txt", poi))

# DB connection
con <- dbConnect(MySQL(), 
                 dbname='stride5',
                 host='ncbolabs-db1')

# Codes for case
caseSet <- paste(casePids, collapse=",")
q1 <- sprintf("select visit.pid, visit.icd9, visit.timeoffset from visit inner join
              (select distinct pid from visit where pid in (%s)) v
              on v.pid=visit.pid;", caseSet) 
casecodes <- dbSendQuery(con, q1)
caseCodesList <- fetch(casecodes, n=-1)

# Remove codes taken below time zero cutoff
pat <- unique(caseCodesList[,1])

CutOffCodes <- function(x){
  cutoff <- ncdata[(ncdata[,1]==x),2]
  ind <- which(caseCodesList[,1]==x)
  rmv <- sapply(as.list(ind), function(y) caseCodesList[y, 3]<cutoff)
  indrmv <- ind[unlist(rmv)]
  if (length(indrmv)==0) return(0)
  caseCodesList <<- caseCodesList[-indrmv,]
  return(length(indrmv))
}
co <- sapply(as.list(pat), CutOffCodes)

# Codes for controls
ctrlSet <- paste(ctrlPids, collapse=",")
q2 <- sprintf("select visit.pid, visit.icd9 from visit inner join
              (select distinct pid from visit where pid in (%s)) v
              on v.pid=visit.pid;", ctrlSet) 
ctrlcodes <- dbSendQuery(con, q2)
ctrlCodesList <- fetch(ctrlcodes, n=-1)
dbDisconnect(con)

pats <- unique(caseCodesList[,1])
numCases <- length(pats)

ctrls <- unique(ctrlCodesList[,1])
numCtrls <- length(ctrls)

TruncateCode <- function(x){
  # Takes all the icd9 strings for a pid
  # splits the string into a vector of codes
  # truncates the decimal and numbers after decimal
  
  strings <- frame[(frame[,1]==x),2]
  oneString <- paste(strings, collapse=",")
  vecCodes <- strsplit(oneString, ",")
  truncCode <- sapply(vecCodes, function(y) gsub(".[0-9]+$", "", y))
  return(truncCode)  
}
frame <- caseCodesList
caseCodes <- lapply(as.list(pats), TruncateCode)

frame <- ctrlCodesList
ctrlCodes <- lapply(as.list(ctrls), TruncateCode)

allCodes <- c(unlist(caseCodes), unlist(ctrlCodes))
codeSet <- unique(allCodes)

tab <- table(factor(allCodes, codeSet))
rmv <- which(tab<40)

# pruned set
codeSet <- codeSet[-rmv]

featureCodes <- matrix(rep(0, length(codeSet)*(numCases+numCtrls)),
                       ncol=length(codeSet))

GetFeats <- function(x){
  feats <- codeSet %in% x
  return(as.numeric(feats))
}

featureCodes[1:numCases,] <- t(sapply(caseCodes, GetFeats))
featureCodes[(numCases+1):(numCases+numCtrls),] <- t(sapply(ctrlCodes, GetFeats))

ids <- c(pats, ctrls)
labels <- c(rep(1, numCases), rep(0, numCtrls))
xData <- cbind(ids, featureCodes, labels)
write(t(xData), file=sprintf("%s_code_feats.txt", poi),
      ncolumns=dim(xData)[2])
