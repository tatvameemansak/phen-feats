# Merge all 4 feature sets into one


library(glmnet)
poi <- "gbm"

setwd("/home/vibhu/Documents/scpd/bmi217/code")

concFile <- sprintf("%s_conc_feats.txt", poi)
medFile <- sprintf("%s_med_feats.txt", poi)
labFile <- sprintf("%s_lab_feats.txt", poi)
codeFile <- sprintf("%s_code_feats.txt", poi)

concFeats <- read.table(concFile, header=F, sep=" ")
medFeats <- read.table(medFile, header=F, sep=" ")
labFeats <- read.table(labFile, header=F, sep=" ")
codeFeats <- read.table(codeFile, header=F, sep=" ")

ccConc <- concFeats[,1]
ccMed <- medFeats[,1]
ccLab <- labFeats[,1]
ccCode <- codeFeats[,1]

interMedLabCode <- intersect(ccMed, intersect(ccCode, ccLab))
interAll <- intersect(ccConc, interMedLabCode)

# cbind the interesecting rows of all 4 sets and remove the
# first and last cols. Retain the last col in the last set
xData <- cbind(concFeats[(concFeats[,1] %in% interAll), -c(1, dim(concFeats)[2])],
               medFeats[(medFeats[,1] %in% interAll), -c(1, dim(medFeats)[2])],
               labFeats[(labFeats[,1] %in% interAll), -c(1, dim(labFeats)[2])],
               codeFeats[(codeFeats[,1] %in% interAll), -1]
)

# Write out the full feature set
write.table(xData, file=sprintf("%s_all_feats.txt", poi))





                        


