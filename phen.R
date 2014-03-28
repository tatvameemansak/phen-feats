# Get notes that have the id-term(s)
# for the phenotype and then the patient ids & terms linked to those notes.

library(RMySQL)

setwd("/home/vibhu/Documents/scpd/bmi217/code")

# String representations of the phenotypes of interest

phenotype_gbm <- c("gbm",
                "glioblastoma",                                                                                
                "glioblastomas",                                            
                "grade iv astrocytoma",                                     
                "grade iv astrocytomas",                                    
                "astrocytomas, grade iv",                                   
                "astrocytoma, grade iv",                                    
                "grade iv astrocytic neoplasm",                             
                "grade iv astrocytic tumor",                                
                "gbm (glioblastoma)",                                       
                "glioblastoma, nos",                                        
                "glioblastoma (morphologic abnormality)",                   
                "glioblastoma, no icd-o subtype (morphologic abnormality)", 
                "glioblastoma, no icd-o subtype",                           
                "[m]glioblastoma nos (morphologic abnormality)",            
                "[m]glioblastoma nos",                                      
                "glioblastoma [disease/finding]"
)

phenotype_dr <- c("dr",                                                
                  "diabetic retinopathy",                             
                  "retinopathy, diabetic",                             
                  "diabetic retinopathies",                            
                  "diabetic retinopathy nos",                          
                  "retinopathies, diabetic",                           
                  "retinopathy diabetic",                              
                  "diabetic retinopathy, nos",                         
                  "dr - diabetic retinopathy",                         
                  "diabetic retinopathy nos (disorder)",               
                  "diabetic retinopathy (disorder)",                   
                  "retinopathy - diabetic",                            
                  "retinal abnormality - diabetes-related (disorder)", 
                  "retinal abnormality - diabetes-related",            
                  "diabetic retinopathy [disease/finding]"        
)

phenotype_mg <- c("mg",                                  
                  "myasthenia gravis",                   
                  "myasthenia gravis paralytica",        
                  "myasthenia gravis, nos",              
                  "mg - myasthenia gravis",              
                  "erb-goldflam disease",                
                  "myasthenia gravis nos (disorder)",    
                  "myasthenia gravis nos",               
                  "myasthenia gravis (disorder)",        
                  "myasthenia gravis [disease/finding]" 
)

con <- dbConnect(MySQL(), 
                 dbname='terminology3',
                 host='ncbolabs-db1')
phen_list <- list(phenotype_gbm, phenotype_dr, phenotype_mg)

for(i in 1:length(phen_list)){
  
  print(i)
  
  # Get term ids that ientify the phenotype
  phenotype <- phen_list[[i]]
  phen <- sprintf("'%s'", phenotype)
  phen <- paste(phen, collapse=',')
  q1 <- sprintf("select tid, str from str2tid where str in (%s);", phen)
  tids <- dbSendQuery(con, q1)
  tidstrList <- fetch(tids, n=-1)
  tids <- tidstrList[,1]
  
  # Remove the 2 and 3 char terms as they are not reliable ids
  rmv <- which(nchar(tidstrList[,2]) <= 3)
  tidSet <- tids[-rmv]
  tidSet <- paste(tidSet, collapse=",")
  
  
  q5 <- sprintf("select stride5.note.nid, stride5.note.pid from stride5.note inner join
                (select nid from stride5.mgrep where tid in (%s)) n 
                on n.nid=stride5.note.nid;", tidSet)
  np <- dbSendQuery(con, q5)
  nidpidList <- fetch(np, n=-1)
  
  # save the data
  # patient list
  pidList <- unique(nidpidList[,2])
  filename<- sprintf("%s_pids.txt", phenotype[1])
  write(t(pidList), file=filename, ncolumns=1)
  
  # id terms
  filename<- sprintf("%s_idTerms.txt", phenotype[1])
  write(t(tids), file=filename, ncolumns=1)
}


dbDisconnect(con)
