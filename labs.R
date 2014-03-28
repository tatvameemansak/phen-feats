# Labs for cases and controls
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

# Labs for case
caseSet <- paste(casePids, collapse=",")
q1 <- sprintf("select lab.pid, lab.description, lab.result_inrange, lab.timeoffset from lab 
              where pid in (%s);", caseSet) 
caselabs <- dbSendQuery(con, q1)
caseLabsList <- fetch(caselabs, n=-1)

# Remove labs taken below time zero cutoff
pat <- unique(caseLabsList[,1])

CutOffLabs <- function(x){
  cutoff <- ncdata[(ncdata[,1]==x),2]
  ind <- which(caseLabsList[,1]==x)
  rmv <- sapply(as.list(ind), function(y) caseLabsList[y, 3]<cutoff)
  indrmv <- ind[unlist(rmv)]
  if (length(indrmv)==0) return(0)
  caseLabsList <<- caseLabsList[-indrmv,]
  return(length(indrmv))
}
co <- sapply(as.list(pat), CutOffLabs)

# Codes for controls
ctrlSet <- paste(ctrlPids, collapse=",")
q2 <- sprintf("select lab.pid, lab.description, lab.result_inrange from lab
              where lab.pid in (%s);", ctrlSet) 
ctrllabs <- dbSendQuery(con, q2)
ctrlLabsList <- fetch(ctrllabs, n=-1)

dbDisconnect(con)

allLabs <- c(caseLabsList[,2], ctrlLabsList[,2])
labSet <- unique(allLabs)

tab <- table(factor(allLabs, labSet))
rmv <- which(tab<40)

#pruned set
labSet <- labSet[-rmv]

pats <- unique(caseLabsList[,1])
numCases <- length(pats)

ctrls <- unique(ctrlLabsList[,1])
numCtrls <- length(unique(ctrls))

inrange_vals <- c("","N", "Y")

featureLabs <- matrix(rep(0, length(labSet)*(numCases+numCtrls)),
                      ncol=length(labSet))

GetFeats <- function(x){
  # Weighted average of Y/N/"" in_range value for each feature
  # "" is 1, N is 2, Y is 3
  # Feature absent is 0
  
  labs_inrange <- frame[(frame[,1]==x),2:3]
  FeatVal <- function(y){
    ind <- which(labs_inrange[,1]==y)
    if (length(ind)==0) return(0)
    
    vals <- labs_inrange[ind, 2]
    tab <- table(factor(vals, inrange_vals))
    featval <- ((tab[1]*1) + (tab[2]*2) + (tab[3]*3))/length(vals)
    return(featval)
    
  }
  feats  <- sapply(as.list(labSet), FeatVal)
  return(feats)
}

frame <- caseLabsList
featureLabs[1:numCases,] <- t(sapply(as.list(pats), GetFeats))

frame <- ctrlLabsList
featureLabs[(numCases+1):(numCases+numCtrls),] <- t(sapply(as.list(ctrls), GetFeats))

ids <- c(pats, ctrls)
labels <- c(rep(1, numCases), rep(0, numCtrls))
xData <- cbind(ids, featureLabs, labels)
write(t(xData), file=sprintf("%s_lab_feats.txt", poi), 
      ncolumns=dim(xData)[2])
