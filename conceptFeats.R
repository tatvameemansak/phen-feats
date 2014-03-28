# Read the control terms file and for create
# a list of pids, each element a list of terms for the notes 
# in the pids record. Do the same for the matched case pids
# for the control pids.
# Create a vocabulary for the case-contol terms
# Convert to to a concepts vocabulary
library(RMySQL)
poi <- "mg"

setwd("/home/vibhu/Documents/scpd/bmi217/code")

# Constants
NBOUND <- -1

idTerms <- scan(sprintf("%s_idTerms.txt", poi))
ctrlFile <- sprintf("%s_ctrl_terms.txt", poi)
caseFile <- sprintf("%s_co.txt", poi)

con  <- file(ctrlFile, open = "r")

ctrlList <- list()
matchedCases <- vector()
ctrls <- vector()

while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
  frame <- (strsplit(oneLine, " "))
  frame <- as.numeric(unlist(frame))
  
  # The first item is the matched case pid
  matchedCases <- c(matchedCases, frame[1])
  
  # second term is the ctrl pid
  ctrls <- c(ctrls, frame[2])
  
  # Remove the first three terms (case pid, ctrl pid, num notes)
  termFrame <- frame[-c(1,2,3)]
  numNotes <- frame[3]
  
  # Positions of the note boundaries, add the last position as well
  nbPos <- which(termFrame==NBOUND)
  nbPos <- c(nbPos, (length(termFrame)+1))
  
  termList <- lapply(as.list(1:numNotes),
                     function(x) {
                       termFrame[(nbPos[x]+1):(nbPos[x+1]-1)]
                     })
  
  ctrlList <- c(ctrlList, list(termList))
  
} 

close(con)

con  <- file(caseFile, open = "r")
caseList <- list()
pats <- vector()

while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
  frame <- (strsplit(oneLine, " "))
  frame <- as.numeric(unlist(frame))
  
  # check if the pid has a matching case
  if (!is.element(frame[1], matchedCases)) {
    print(frame[1]) 
    next
  }
  
  # the first term is a pid that matched
  pats <- c(pats, frame[1])
  
  # Remove the first three terms (pid, cut off, num notes)
  termFrame <- frame[-c(1,2,3)]
  numNotes <- frame[3]
  
  # Positions of the note boundaries, add the last position as well
  nbPos <- which(termFrame==NBOUND)
  nbPos <- c(nbPos, (length(termFrame)+1))
  
  termList <- lapply(as.list(1:numNotes),
                     function(x) {
                       termFrame[(nbPos[x]+1):(nbPos[x+1]-1)]
                     })
  
  caseList <- c(caseList, list(termList))
  
} 

close(con)

allTerms <- c(unlist(ctrlList), unlist(caseList))
termVocab <- unique(allTerms)

# Get concept ids
# First some basic pruning (all terms whose freq is less than 5)

tab <- table(factor(allTerms, termVocab))
rmv <- which(tab<5)
termVocab <- termVocab[-rmv]

tidSet <- paste(termVocab, collapse=",")
con <- dbConnect(MySQL(), 
                 dbname='terminology3',
                 host='ncbolabs-db1')

q1 <- sprintf("select tid2cid.tid, tid2cid.cid, tid2cid.grp from tid2cid inner join
              (select distinct tid from tid2cid where tid in (%s)) t
              on t.tid=tid2cid.tid where tid2cid.grp is not NULL", tidSet)
tidcid <- dbSendQuery(con, q1)
tidCidList <- fetch(tidcid, n=-1)

# Convert to concepts

ConvCid <- function(x){
  
  # Censor concepts of id terms
  idtInds <- x %in% idTerms
  if (any(idtInds)) x<-x[-which(idtInds)]
  
  conceptList <- lapply(x, function(y){
    tidCidList[(tidCidList[,1] %in% y), 2]
  })
  return(conceptList)
}

ctrlConcList <- lapply(ctrlList, ConvCid)
caseConcList <- lapply(caseList, ConvCid)

allConc <- c(unlist(ctrlConcList), unlist(caseConcList))
concVocab <- unique(allConc)
tab <- table(factor(allConc, concVocab))

# Remove low prevalence concepts
rmv <- which(tab<=200)
concVocab <- concVocab[-rmv]

# We generate features for cases and controls
numCases <- length(caseConcList)
numCtrls <- length(ctrlConcList)
features <- matrix(rep(0, (length(concVocab)*(numCases+numCtrls))),
                       ncol=length(concVocab)
)

GetTfIdf <- function(x){
  
  tf <- table(factor(unlist(x), concVocab))
  df <- sapply(as.list(concVocab), function(y){
    sum(sapply(x, function(z) is.element(y,z))) + 1
  }
  )
  tfIdf <- tf/df
  return(tfIdf)
}

features[(1:numCases),] <- t(sapply(caseConcList, GetTfIdf))
features[(numCases+1):(numCases+numCtrls),] <- t(sapply(ctrlConcList, GetTfIdf))
labels <- c(rep(1, numCases), rep(0, numCtrls))
ids <- c(pats, ctrls)
xData <- cbind(ids, features, labels)
write(t(xData), file=sprintf("%s_conc_feats.txt", poi), ncolumns=dim(xData)[2])
                   
