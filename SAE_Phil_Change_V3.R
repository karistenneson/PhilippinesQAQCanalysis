#Unbiased Area Estimation of Philippines Forest Change
#Written by Karis Tenneson and Crystal Wespestad (SIG) Feb, 2022
#####################################################################
#Notes: epochs are 2000-2005, 2006-2012, 2013-2018
#there are 850 points total, 150 have been reviewed in duplicate 

library(survey)
library(tidyverse)
library(knitr)
library(rmarkdown)
library(tidyr)
library(networkD3)
library(tidyverse)
library(dplyr)

setwd("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis")


##########Join points with map strata values##################
# #Map strata dictionary -- projects/sig-ee/Philippines/v2/strata/gapFillStrata-2-2-0
# 31: "deforested epoch 1",
# 32: "deforested epoch 2 & 3",
# 41: "reforested epoch 1",
# 42: "reforested epoch 2 & 3",
# 50: "stable forest",
# 60: "stable non forest",
# 70: "multiple events noise",
# 80: "multiple events ecologically possible",
## file created of extracted map strata from exported dataCEO_combo
data_mapstrata <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\PhilippinesPointsCombo_Strata.csv")
dim(data_mapstrata)
colnames(data_mapstrata)

############## Strata pixel counts #############
strataAreas <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\philippines_pixelcounts.csv")
head(strataAreas)
colnames(strataAreas)[1]<-"MapValue"
colnames(strataAreas)[2]<-"StrataName"
colnames(strataAreas)[3]<-"PixelCount"
#datamerged_FIXED <- merge(datamerged_FIXED, strataAreas, by.x = 'ChangeStrata_Map', by.y = 'MapValue', all.x = F)
dim(datamerged_FIXED)


###########################################################################
############################################################################
###########################################################################
############################################################################
##After review there were a few points that were double checked because
##there were more off-diagonal points than were expected in the confusion matrix
##Below is the updated analysis with those points altered

datamerged_FIXED_v2 <- read.csv('C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\ceo-Philippines_QAQCReview_MapInterpreterDisagreement-sample-data-FIXED.csv')
colnames(datamerged_FIXED_v2)
datamerged_FIXED_v2 <- datamerged_FIXED_v2[-c(1,3:80)]
colnames(datamerged_FIXED_v2)
colnames(datamerged_FIXED)
head(datamerged_FIXED$PLOTID)
head(datamerged_FIXED_v2$sampleid)

#########More points were reviewed by SMEs
SMEreviewdData_toreplaceold <- read.csv('C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\ceo-ALLPOINTS_Philippines_QAQCReview_MapInterpreterDisagreement-sample-data-2022-05-04_SMEreview.csv')
colnames(SMEreviewdData_toreplaceold)
#SMEreviewdData_toreplaceold <- SMEreviewdData_toreplaceold[-c(1,5:55)]
SMEreviewdData_toreplaceold <- SMEreviewdData_toreplaceold[-c(1,6:11,17:18)]
colnames(SMEreviewdData_toreplaceold)
dim(SMEreviewdData_toreplaceold)

colnames(SMEreviewdData_toreplaceold)[52]<-"Change1yearb"
colnames(SMEreviewdData_toreplaceold)[57]<-"Change2yearb"
colnames(SMEreviewdData_toreplaceold)[64]<-"Change3yearb"
colnames(SMEreviewdData_toreplaceold)[73]<-"Confidenceb"
colnames(SMEreviewdData_toreplaceold)[74]<-"ConfidenceReasoningb"
colnames(SMEreviewdData_toreplaceold)[47]<-"LC2000b"
colnames(SMEreviewdData_toreplaceold)[48]<-"ForestType2000b"
colnames(SMEreviewdData_toreplaceold)[49]<-"NonForestType2000b" 
colnames(SMEreviewdData_toreplaceold)[50]<-"CropType2000b" 
colnames(SMEreviewdData_toreplaceold)[68]<-"LC2018endb"
colnames(SMEreviewdData_toreplaceold)[51]<-"forestChangeEventb" 
colnames(SMEreviewdData_toreplaceold)[65]<-"ChangeType1b"
colnames(SMEreviewdData_toreplaceold)[67]<-"DegradationDriver1b"
colnames(SMEreviewdData_toreplaceold)[66]<-"LossDriver1b"
colnames(SMEreviewdData_toreplaceold)[53]<-"SecondForestChangeEventb"
colnames(SMEreviewdData_toreplaceold)[54]<-"ChangeType2b"
colnames(SMEreviewdData_toreplaceold)[55]<-"DegradationDriver2b"
colnames(SMEreviewdData_toreplaceold)[56]<-"LossDriver2b"
colnames(SMEreviewdData_toreplaceold)[58]<-"ThirdForestChangeEventb" 
colnames(SMEreviewdData_toreplaceold)[59]<-"ChangeType3b"
colnames(SMEreviewdData_toreplaceold)[61]<-"DegradationDriver3b"
colnames(SMEreviewdData_toreplaceold)[60]<-"LossDriver3b"
colnames(SMEreviewdData_toreplaceold)[62]<-"morethan3forestchangesb" 
colnames(SMEreviewdData_toreplaceold)[63]<-"DescribeExtraChangesb" 
colnames(SMEreviewdData_toreplaceold)[69]<-"ForestType2018b" 
colnames(SMEreviewdData_toreplaceold)[70]<-"NonForestType2018b" 
colnames(SMEreviewdData_toreplaceold)[71]<-"CropType2018b" 
colnames(SMEreviewdData_toreplaceold)[72]<-"mixedLCpixelb"
colnames(SMEreviewdData_toreplaceold)


####Fill in all the values for the rows that were not reviewed with the original answers
SMEreviewdData_toreplaceold$LC2000b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_lc2000a, SMEreviewdData_toreplaceold$LC2000b)
SMEreviewdData_toreplaceold$ForestType2000b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_foresttype2000, SMEreviewdData_toreplaceold$ForestType2000b)
SMEreviewdData_toreplaceold$NonForestType2000b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_nonforesttype2000, SMEreviewdData_toreplaceold$NonForestType2000b)
SMEreviewdData_toreplaceold$CropType2000b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_nonforesttype2000, SMEreviewdData_toreplaceold$CropType2000b)
SMEreviewdData_toreplaceold$forestChangeEventb <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_forestchangeevent, SMEreviewdData_toreplaceold$forestChangeEventb)
SMEreviewdData_toreplaceold$Change1yearb <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_change1year, SMEreviewdData_toreplaceold$Change1yearb)
SMEreviewdData_toreplaceold$SecondForestChangeEventb <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_secondforestchangeevent, SMEreviewdData_toreplaceold$SecondForestChangeEventb)
SMEreviewdData_toreplaceold$ChangeType2b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_changetype2, SMEreviewdData_toreplaceold$ChangeType2b)
SMEreviewdData_toreplaceold$DegradationDriver2b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_degradationdriver2, SMEreviewdData_toreplaceold$DegradationDriver2b)
SMEreviewdData_toreplaceold$LossDriver2b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_lossdriver2, SMEreviewdData_toreplaceold$LossDriver2b)
SMEreviewdData_toreplaceold$Change2yearb <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_change2year, SMEreviewdData_toreplaceold$Change2yearb)
SMEreviewdData_toreplaceold$ThirdForestChangeEventb <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_thirdforestchangeevent, SMEreviewdData_toreplaceold$ThirdForestChangeEventb)
SMEreviewdData_toreplaceold$ChangeType3b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_changetype3, SMEreviewdData_toreplaceold$ChangeType3b)
SMEreviewdData_toreplaceold$LossDriver3b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_lossdriver3, SMEreviewdData_toreplaceold$LossDriver3b)
SMEreviewdData_toreplaceold$DegradationDriver3b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_degradationdriver3, SMEreviewdData_toreplaceold$DegradationDriver3b)
SMEreviewdData_toreplaceold$morethan3forestchangesb <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_morethan3forestchanges, SMEreviewdData_toreplaceold$morethan3forestchangesb)
SMEreviewdData_toreplaceold$DescribeExtraChangesb <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_describeextrachanges, SMEreviewdData_toreplaceold$DescribeExtraChangesb)
SMEreviewdData_toreplaceold$Change3yearb <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_change3year, SMEreviewdData_toreplaceold$Change3yearb)
SMEreviewdData_toreplaceold$ChangeType1b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_changetype1, SMEreviewdData_toreplaceold$ChangeType1b)
SMEreviewdData_toreplaceold$LossDriver1b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_lossdriver1, SMEreviewdData_toreplaceold$LossDriver1b)
SMEreviewdData_toreplaceold$DegradationDriver1b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_degradationdriver1, SMEreviewdData_toreplaceold$DegradationDriver1b)
SMEreviewdData_toreplaceold$LC2018endb <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_lc2018end, SMEreviewdData_toreplaceold$LC2018endb)
SMEreviewdData_toreplaceold$ForestType2018b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_foresttype2018, SMEreviewdData_toreplaceold$ForestType2018b)
SMEreviewdData_toreplaceold$NonForestType2018b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_nonforesttype2018, SMEreviewdData_toreplaceold$NonForestType2018b)
SMEreviewdData_toreplaceold$CropType2018b <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_croptype2018, SMEreviewdData_toreplaceold$CropType2018b)
SMEreviewdData_toreplaceold$mixedLCpixelb <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_mixedlcpixel, SMEreviewdData_toreplaceold$mixedLCpixelb)
SMEreviewdData_toreplaceold$Confidenceb <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_confidence, SMEreviewdData_toreplaceold$Confidenceb)
SMEreviewdData_toreplaceold$ConfidenceReasoningb <- ifelse(SMEreviewdData_toreplaceold$email %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$pl_confidencereasoning, SMEreviewdData_toreplaceold$ConfidenceReasoningb)

write.csv(SMEreviewdData_toreplaceold, file = 'Results\\SMEreviewdData_toreplaceold_testmerge_v3.csv', row.names = F)

FINALDATASET <- SMEreviewdData_toreplaceold
dim(FINALDATASET)
colnames(FINALDATASET)

############THIS IS WHERE NEW REVIEWED POINTS SHOULD GO#################

#upload the final reviewed CEO points, and relabel of degradation
final41relabel <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\reviewed41_bestlabel.csv")
degradationrelabel <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\degradation_relabeled.csv")
colnames(final41relabel)
colnames(degradationrelabel)


FINALDATASET <- merge(FINALDATASET, final41relabel, by.x = c('lon','lat'), by.y = c('lonc','latc'), all.x=T)
dim(FINALDATASET)
write.csv(FINALDATASET, file = 'Results\\check_Temp.csv', row.names = F)
FINALDATASET <- merge(FINALDATASET, degradationrelabel[c('lon','lat','SecondaryLabel')] , by.x=c('lon','lat'), by.y = c('lon','lat'), all.x=T)
dim(FINALDATASET)
head(FINALDATASET)
write.csv(FINALDATASET, file = 'Results\\check_Temp.csv', row.names = F)
colnames(FINALDATASET)


########Now do it again for the last round of relabeling 41 points
#An inelegant way to combine the extra QAQC data with the original
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$PLOTID<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,3], 
                               ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,75],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$EMAIL<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,4], 
                              ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,77],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$Confidence<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,14], 
                                   ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,73],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$ConfidenceReasoning<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,74], 
                                            ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,79],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$LC2000<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,47], 
                                ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,80],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$ForestType2000<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,48], 
                                       ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,81],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$NonForestType2000<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,49], 
                                          ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,82],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$CropType2000<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,50], 
                                     ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,83],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$LC2018end<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,68], 
                                  ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,84],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$forestChangeEvent<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,51], 
                                          ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,85],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$Change1year<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,52], 
                                    ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,86],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$ChangeType1<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,65], 
                                    ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,87],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$DegradationDriver1<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,67], 
                                           ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,88],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$LossDriver1<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,66], 
                                    ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,89],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$SecondForestChangeEvent<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,53], 
                                                ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,90],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$Change2year<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,57], 
                                    ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,91],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$ChangeType2<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,54], 
                                    ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,92],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$DegradationDriver2<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,55], 
                                           ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,93],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$LossDriver2<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,56], 
                                    ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,94],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$ThirdForestChangeEvent<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,58], 
                                               ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,95],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$Change3year<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,64], 
                                    ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,96],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$ChangeType3<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,59], 
                                    ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,97],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$DegradationDriver3<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,61], 
                                           ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,98],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$LossDriver3<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,60], 
                                    ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,99],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$morethan3forestchanges<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,62], 
                                               ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,100],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$DescribeExtraChanges<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,63], 
                                             ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,101],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$ForestType2018<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,69], 
                                       ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,102],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$NonForestType2018<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,70], 
                                          ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,103],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$CropType2018<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,71], 
                                     ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,104],'nope'))
}
for(i in 1:nrow(FINALDATASET)) {
  FINALDATASET$mixedLCpixel<-ifelse(is.na(FINALDATASET[,75]), FINALDATASET[,72], 
                                     ifelse(!is.na(FINALDATASET[,75]), FINALDATASET[,105],'nope'))
}
dim(FINALDATASET)
FINALDATASET <- FINALDATASET[-c(4:105)]
write.csv(FINALDATASET, file = 'Results\\check_Temp.csv', row.names = F)


############Set up clean CEO labels - Version 4 NEW
#In version 4: Pixels with multiple events are combined, agroforestry pulled out
FINALDATASET$CEOreadable_v4_NEW <- ifelse(FINALDATASET$ChangeType1 == "Degradation" & FINALDATASET$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration"), 'Degradation',
                                          ifelse(FINALDATASET$ChangeType1 == "Deforestation" & FINALDATASET$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration"), 'Deforestation', 
                                                 ifelse(FINALDATASET$ChangeType1 == "Reforestation" & FINALDATASET$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration"), 'Reforestation',
                                                        ifelse(FINALDATASET$morethan3forestchanges == "no", 'multiple events',
                                                               ifelse(FINALDATASET$morethan3forestchanges == "yes", 'multiple events',
                                                                      ifelse(FINALDATASET$ChangeType2 %in% c("Degradation","Deforestation","Regeneration") & FINALDATASET$ThirdForestChangeEvent == "no", 'multiple events',
                                                                             ifelse(FINALDATASET$forestChangeEvent == "N/A non-forest entire time" & FINALDATASET$CropType2000 %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest Agroforestry",
                                                                                    ifelse(FINALDATASET$forestChangeEvent == "N/A non-forest entire time" & FINALDATASET$CropType2000 %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest",
                                                                                           ifelse(FINALDATASET$forestChangeEvent == "no", "stable forest", 'NotReviewed')))))))))
table(FINALDATASET$CEOreadable_v4_NEW)


write.csv(SMEreviewdData_toreplaceold, file = 'Results\\TEST_CEOreadable_v4output_revised.csv', row.names = F)

#In version 5: Pixels with multiple events are combined, agroforestry pulled out, separated by epoch
FINALDATASET$CEOreadable_v5_NEW <- ifelse(FINALDATASET$ChangeType1 == "Degradation" & FINALDATASET$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration"), 'Degradation',
                                           ifelse(FINALDATASET$ChangeType1 == "Deforestation" & FINALDATASET$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration") & FINALDATASET$Change1year %in% c(2000:2005), 'Deforestation Epoch1', 
                                           ifelse(FINALDATASET$ChangeType1 == "Deforestation" & FINALDATASET$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration") & FINALDATASET$Change1year %in% c(2006:2018), 'Deforestation Epoch23',
                                                  ifelse(FINALDATASET$ChangeType1 == "Reforestation" & FINALDATASET$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration") & FINALDATASET$Change1year %in% c(2000:2005), 'Reforestation Epoch1',
                                                         ifelse(FINALDATASET$ChangeType1 == "Reforestation" & FINALDATASET$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration") & FINALDATASET$Change1year %in% c(2006:2018), 'Reforestation Epoch23',       
                                                         ifelse(FINALDATASET$morethan3forestchanges == "no", 'multiple events eco possible',
                                                                ifelse(FINALDATASET$morethan3forestchanges == "yes", 'multiple events noise',
                                                                       ifelse(FINALDATASET$ChangeType2 %in% c("Degradation","Deforestation","Regeneration") & FINALDATASET$ThirdForestChangeEvent == "no", 'multiple events eco possible',
                                                                              ifelse(FINALDATASET$forestChangeEvent == "N/A non-forest entire time" & FINALDATASET$CropType2000 %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest Agroforestry",
                                                                                     ifelse(FINALDATASET$forestChangeEvent == "N/A non-forest entire time" & FINALDATASET$CropType2000 %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest",
                                                                                            ifelse(FINALDATASET$forestChangeEvent == "no", "stable forest", 'NotReviewed')))))))))))
table(FINALDATASET$CEOreadable_v5_NEW)


########Simplified Map data
FINALDATASET$ReadableChangeStrata_Map_v3<-"fixMe"
FINALDATASET$ReadableChangeStrata_Map_v3[FINALDATASET$ChangeStrata_Map == 31]<-"Deforestation"
FINALDATASET$ReadableChangeStrata_Map_v3[FINALDATASET$ChangeStrata_Map == 32]<-"Deforestation"
FINALDATASET$ReadableChangeStrata_Map_v3[FINALDATASET$ChangeStrata_Map == 41]<-"Reforestation"
FINALDATASET$ReadableChangeStrata_Map_v3[FINALDATASET$ChangeStrata_Map == 42]<-"Reforestation"
FINALDATASET$ReadableChangeStrata_Map_v3[FINALDATASET$ChangeStrata_Map == 50]<-"stable forest"
FINALDATASET$ReadableChangeStrata_Map_v3[FINALDATASET$ChangeStrata_Map == 60]<-"stable non forest"
FINALDATASET$ReadableChangeStrata_Map_v3[FINALDATASET$ChangeStrata_Map == 70]<-"multiple events"
FINALDATASET$ReadableChangeStrata_Map_v3[FINALDATASET$ChangeStrata_Map == 80]<-"multiple events"
unique(FINALDATASET$ReadableChangeStrata_Map_v3)
colnames(FINALDATASET)
FINALDATASET <- merge(FINALDATASET, strataAreas, by.x = 'ChangeStrata_Map', by.y = 'MapValue', all.x = F)

#cross tab of NEW simplified mapv3 and ceo v4 agroforestry removed strata
table(FINALDATASET$ReadableChangeStrata_Map_v3, FINALDATASET$CEOreadable_v4_NEW)
table_mapv3_ceov4 <- table(FINALDATASET$ReadableChangeStrata_Map_v3, FINALDATASET$CEOreadable_v4_NEW)
write.csv(table_mapv3_ceov4, file = 'Results\\CrossTable_mapv3_ceov4_simple_v3.csv', row.names = T)

#cross tab of NEW strataorig and ceov5, agroforestry pulled out, separated by epoch
table(FINALDATASET$StrataName, FINALDATASET$CEOreadable_v5_NEW)
table_strataname_ceov5 <- table(FINALDATASET$StrataName, FINALDATASET$CEOreadable_v5_NEWb)
write.csv(table_strataname_ceov5, file = 'Results\\CrossTable_strataname_ceov5_epochseparated_v2.csv', row.names = T)

###########select multiple events points for review
FINALDATASET$CEOreviewQAQC_multipleevents <- ifelse(FINALDATASET$StrataName == "multiple events ecologically possible"  & FINALDATASET$CEOreadable_v4_NEWb == "Deforestation", 'LabelLoss-MapMulti',
                                                           ifelse(FINALDATASET$StrataName == "multiple events ecologically possible" & FINALDATASET$CEOreadable_v4_NEWb == "Degradation", 'LabelDeg-MapMulti', 
                                                                  ifelse(FINALDATASET$StrataName == "multiple events ecologically possible" & FINALDATASET$CEOreadable_v4_NEWb == "Reforestation", 'LabelReforest-MapMulti',
                                                                         ifelse(FINALDATASET$StrataName == "multiple events ecologically possible" & FINALDATASET$CEOreadable_v4_NEWb == "stable forest", 'LabelForest-MapMulti',
                                                                                ifelse(FINALDATASET$StrataName == "multiple events ecologically possible" & FINALDATASET$CEOreadable_v4_NEWb == "multiple events", 'LabelMulti-MapMulti',
                                                                                       ifelse(FINALDATASET$StrataName == "multiple events ecologically possible" & FINALDATASET$CEOreadable_v4_NEWb == "stable nonforest", 'LabelNonforest-MapMulti',                                                                                      
                                                                                       ifelse(FINALDATASET$StrataName == "multiple events ecologically possible" & FINALDATASET$CEOreadable_v4_NEWb == "stable nonforest Agroforestry", 'LabelAgroforest-MapMulti','No')))))))
table(FINALDATASET$CEOreviewQAQC_multipleevents)
write.csv(FINALDATASET, file = 'Results\\CEOreviewQAQC_multipleevents_v2.csv', row.names = T)

###############################################

#######See how much review agreed with original interpretations
FINALDATASET$reviewagreementrate <- ifelse(FINALDATASET$CEOreadable_v4 == FINALDATASET$CEOreadable_v4_NEWb, 'Agree',
                                        ifelse(FINALDATASET$CEOreadable_v4 != FINALDATASET$CEOreadable_v4_NEWb, 'Dis','Error'))
table(FINALDATASET$reviewagreementrate)

write.csv(FINALDATASET, file = 'Results\\FINALDATASET_useforreview_v2.csv', row.names = F)
#96 points were reviewed???
#42 points = 'Dis', so 50% of the points reviewed had their interpretation changed
table(FINALDATASET$CEOreadable_v4,FINALDATASET$CEOreadable_v4_NEWb)
table_compare_SMEpart1_toorig <- table(FINALDATASET$CEOreadable_v4,FINALDATASET$CEOreadable_v4_NEWb)
write.csv(table_compare_SMEpart1_toorig, file = 'Results\\table_compare_SMEpart1_toorig_v2.csv', row.names = T)
##########################################################################################
##########################################################################################
## ANALYSIS REDO
##########################################################################################
##########################################################################################
#cross tab of simplified mapv3 and ceo v4 agroforestry removed strata
table(FINALDATASET$ReadableChangeStrata_Map_v3, FINALDATASET$CEOreadable_v4_NEWb)
table_mapv3_ceov4_NEW <- table(FINALDATASET$ReadableChangeStrata_Map_v3, FINALDATASET$CEOreadable_v4_NEWb)
write.csv(table_mapv3_ceov4_NEW, file = 'Results\\CrossTable_mapv3_ceov4_simple_NEW_v2.csv', row.names = T)



#cross tab of original strata to agroforestry removed strata, updated
table(FINALDATASET$StrataName, FINALDATASET$CEOreadable_v4_NEWb)
table_maporig_ceov4_NEW <- table(FINALDATASET$StrataName, FINALDATASET$CEOreadable_v4_NEWb)
write.csv(table_maporig_ceov4_NEW, file = 'Results\\CrossTable_maporigstrata_ceov4_NEW_v2.csv', row.names = T)

#########################################
## Set up sample design
#########################################
strat_design <- svydesign(id = ~1, strata = ~StrataName, fpc = ~PixelCount, 
                          data = FINALDATASET)
########################################
## once sample design is set up you can analyze the data
#########################################
## survey total (svytotal) calculates area weighted totals of data
?svydesign()

activityData <- svytotal(~CEOreadable_v4_NEWb, strat_design)
activityData

Change<-as.data.frame(activityData)
colnames(Change)<-c('Total, pixels','SE, pixels')
Change
rownames(Change)<-c("Deforestation", 
                    "Degradation", 
                    "multiple events",
                    "Reforestation",
                    "stable forest", 
                    "stable non-forest",
                    "stable non-forest agroforestry")
Change


## convert to ha
Change<-Change* 30 * 30 / 10000
Change
Change<-round(Change, digits = 0)
Change

colnames(Change)<-c('Total, ha','SE, ha')
Change 

#######
## write results #########
write.csv(Change, file = 'Results\\AreasofDisturbance_ceoV4version_SAVE_NEW_v2.csv', row.names = T)

