# We will get time zero cutoffs for each pid belonging
# to the cases group. The time zero cutoff is the time stampt
# of the note that saw the first occurance of any of the terms
# identifying the phenotype. The output is a file with a 
# pid, a cutoff value and the terms in the notes that follow

library(RMySQL)

setwd("/home/vibhu/Documents/scpd/bmi217/code")

# Constants
NBOUND <- -1

#phenotypes of interest
poiSet <- c("dr")


GetCutoff <- function(x){
  print(which(pids==x))
  q1 <- sprintf("select note.nid, note.timeoffset, mgrep.tid from note 
                inner join mgrep on note.nid=mgrep.nid 
                where note.pid=%d;", as.numeric(x))
  nidOffTids <- dbSendQuery(con, q1)
  nidOffTidList <- fetch(nidOffTids, n=-1)
  
  # Now we go through the notes and for each and scan which note has terms
  # containing the phenotype
  
  nidList <- unique(nidOffTidList[,1])
  
  ChkPhen <- function(y){
    tids <- nidOffTidList[(nidOffTidList[,1]==y),3]
    return(any(tids %in% allIdTerms))
    
  }
  hasPhenotype <- sapply(as.list(nidList), ChkPhen)
  notesWithPhenotype <- nidList[hasPhenotype]
  offSets <- nidOffTidList[(nidOffTidList[,1] %in% notesWithPhenotype),2]
  
  # Smallest offset is the cut off
  cutoff <- min(offSets)
  notes <- unique(nidOffTidList[(nidOffTidList[,2]>=cutoff), 1])
  terms <- lapply(as.list(notes), 
                  function(x) c(NBOUND, 
                                nidOffTidList[(nidOffTidList[,1]%in%x),3])
  )
  return(c(cutoff, length(notes), unlist(terms)))
  
}

WriteFile <- function(x){
  ind <- which(pids==x)
  print(ind)
  numnotes <- cutoffList[[ind]][2]
  frame <- c(x, cutoffList[[ind]])
  write(frame, append = T, 
        file = sprintf("%s_co.txt", poi), 
        ncolumns = length(frame))
  write(c(x, numnotes), append=T,
        file = sprintf("%s_nc.txt", poi),
        ncolumns = 2)
}

for(poi in poiSet){
  print(poi)
  allIdTerms <- scan(sprintf("%s_idTerms.txt", poi))
  pids <- scan(file=sprintf("%s_pids.txt", poi))
  
  # We take only a 1000 pids
  pids <- pids[1:1000]
  
  con <- dbConnect(MySQL(), 
                   dbname='stride5',
                   host='ncbolabs-db1')
  
  cutoffList1 <- lapply(as.list(pids[1:501]), GetCutoff)
  cutoffList2 <- lapply(as.list(pids[501:1000]), GetCutoff)
  cutoffList <- c(cutoffList1, cutoffList2)
  
  # Write the pid, cutoff and term data to a file
  lapply(as.list(pids), WriteFile) 
  dbDisconnect(con)
}



