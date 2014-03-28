# Get match ctrl pids to case pids s.t number of notes
# in the controls is the same as in cases 1-1

library(RMySQL)

setwd("/home/vibhu/Documents/scpd/bmi217/code")

# Constants
NBOUND <- -1

# Cases that remain unmatched after an epoch
unmatched <- list()

#phenotypes of interest
poiSet <- c("gbm", "dr", "mg")

GetTerms <- function(x){
  print(which(pids==x))
  
  # Check if there are any cases to match
  if (length(nc)==0) return(NULL)
  
  q1 <- sprintf("select note.nid, note.timeoffset, mgrep.tid from note 
                inner join mgrep on note.nid=mgrep.nid 
                where note.pid=%d;", as.numeric(x))
  nidOffTids <- dbSendQuery(con, q1)
  nidOffTidList <- fetch(nidOffTids, n=-1)
  
  # Empty record
  if (dim(nidOffTidList)[1]==0) return(NULL)
  
  notes <- unique(nidOffTidList[,1])
  totalNotes <- length(notes)
  
  # Check if the number of notes is approx equal to one of the
  # notecounts for cases
  
  ind <- abs(nc - totalNotes)/nc <= 0.25
  ind <- which(ind)
  
  if (length(ind)==0) return(NULL)
  
  # Otherwise it means that the totalNotes matches the note count
  # of at least one of the cases. Create a frame consisting of all
  # terms and mark note boundary with NBOUND. The frame returned contains
  # <Case pid> <ctrl pid> <num notes> <BDRY MARKER> <terms-note1> <BDRY MARKER> .. 
  
  terms <- sapply(as.list(notes), 
                  function(x) c(NBOUND, 
                                nidOffTidList[(nidOffTidList[,1]%in%x),3])
  )
  
  # Discard this case from the note count list as this has been matched
  nc <<- nc[-ind[1]]
  
  frame <- c(id[ind[1]], x, totalNotes, unlist(terms))
  id <<- id[-ind[1]]
  
  return(frame)
}

WriteFile <- function(x){
  frame <- terms[[x]]
  write(frame, append = T, 
        file = sprintf("%s_ctrl_terms.txt", poi), 
        ncolumns = length(frame))
  
}

con <- dbConnect(MySQL(), 
                 dbname='stride5',
                 host='ncbolabs-db1')

for(poi in poiSet){
  
  pids <- scan(sprintf("%s_ctrl_pids.txt", poi))
  ncdata <- scan(sprintf("%s_nc.txt", poi))
  ncdata <- matrix(ncdata, nrow=1000, byrow=T)
  nc <- ncdata[,2]
  id <- ncdata[,1]

  # We try and match a control pid with a case pid. If there is a match
  # we keep discarding from the to-be matched pids list
  terms1 <- lapply(as.list(pids[1:5000]), GetTerms)
  
  if (length(nc)>0) unmatched<- c(unmatched, list(nc))
  
  exc <- sapply(terms1, is.null)
  if (sum(exc)>0) terms<- terms1[-which(exc)] else terms <- terms1
 
  
  # Write the data and the case pids that were matched
  lapply(as.list(1:length(terms)), WriteFile) 

  
}
dbDisconnect(con)
