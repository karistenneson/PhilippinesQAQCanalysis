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

# CEO data (Original with CEO duplicate bug)
#file replace with one with duplicates removed
dataCEO <- read.csv('C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\ceo-Philippines_Change_madeJan18_SingleInterpretation_Final_V2-sample-data-2022-03-02_no duplicates.csv')
#find the erroneously duplicated plots, which should not be duplicates. Asked which should be kept.
dataCEO$ï..plotid[duplicated(dataCEO$ï..plotid)] #found 274 282 288 288 330 393 868
sort(dataCEO$lon[duplicated(dataCEO$lon)])
duplicated(dataCEO[c("lon","lat")])
dataCEO$lon[duplicated(dataCEO$lat)]
dim(dataCEO)
table(dataCEO$Land.cover.in.2000.)
table(dataCEO$Type.of.forest.)
table(dataCEO$Type.of.non.forest.land.cover.)

#upload the additional CEO points (150)
#double interpretations with duplicates removed
dataCEO_TOADD <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\ceo-Philippines_Change_madeJan18_QAQC-sample-data-2022-01-28.csv")

#############Used this to find erroneous duplicates
dataCEO_TOADD$ï..plotid[duplicated(dataCEO_TOADD$ï..plotid)]
dim(dataCEO_TOADD)
colnames(dataCEO)==colnames(dataCEO_TOADD)
#make column names agree
dataCEO_TOADD$Is.this.a.mixed.land.cover.pixel. <- "Not Measured"
colnames(dataCEO)[colnames(dataCEO) == "ï..plotid"] <- "plotid"
colnames(dataCEO)==colnames(dataCEO_TOADD)

#group interpreted CEO points
dataCEO_combo <- merge(dataCEO, dataCEO_TOADD, by.x =c('lon','lat'), by.y = c('lon','lat'), all.x=T)
dim(dataCEO_combo)
##dataCEO_combo<-rbind(dataCEO_TOADD, dataCEO) ##removed because lon/lat of TOADD are in dataCEO_combo
head(dataCEO_combo)
tail(dataCEO_combo)

write.csv(dataCEO_combo, file = 'Results\\what_to_fix.csv', row.names = F)

###########RESOLVE THE DUPLICATES BETWEEN THE ADDED DATA AND SINGLE INTERPRETATIONS
###########THE QAQC POINTS WILL REPLACE THOSE DONE BY SINGLE INTERPRETERS
##########################################################
#An inelegant way to combine the QAQC data with the original
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$PLOTID<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,3], 
                                   ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,42],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$EMAIL<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,5], 
                                   ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,44],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$Confidence<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,14], 
                                   ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,53],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$ConfidenceReasoning<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,15], 
                           ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,54],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$LC2000a<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,16], 
                                             ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,55],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$ForestType2000<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,17], 
                                             ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,56],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$NonForestType2000<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,18], 
                                             ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,57],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$CropType2000<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,19], 
                                             ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,58],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$LC2018end<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,20], 
                                       ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,59],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$forestChangeEvent<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,21], 
                                       ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,60],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$Change1year<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,22], 
                                            ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,61],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$ChangeType1<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,23], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,62],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$DegradationDriver1<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,24], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,63],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$LossDriver1<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,25], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,64],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$SecondForestChangeEvent<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,26], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,65],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$Change2year<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,27], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,66],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$ChangeType2<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,28], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,67],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$DegradationDriver2<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,29], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,68],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$LossDriver2<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,30], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,69],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$ThirdForestChangeEvent<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,31], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,70],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$Change3year<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,32], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,71],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$ChangeType3<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,33], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,72],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$DegradationDriver3<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,34], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,73],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$LossDriver3<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,35], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,74],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$morethan3forestchanges<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,36], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,75],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$DescribeExtraChanges<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,37], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,76],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$ForestType2018<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,38], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,77],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$NonForestType2018<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,39], 
                                    ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,78],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$CropType2018<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,40], 
                                          ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,79],'nope'))
}
for(i in 1:nrow(dataCEO_combo)) {
  dataCEO_combo$mixedLCpixel<-ifelse(is.na(dataCEO_combo[,42]), dataCEO_combo[,41], 
                                          ifelse(!is.na(dataCEO_combo[,42]), dataCEO_combo[,80],'nope'))
}
dim(dataCEO_combo)
#Update the column names for original CEO data to shorten them
dataCEO_combo <- dataCEO_combo[-c(3:80)]
#output the cleaned CEO data
write.csv(dataCEO_combo, file = 'Results\\Philippines_CEO_FinalPoints_SAVE_v2.csv', row.names = F)

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

#####Join fire results to original CSV points based on plotid
datamerged <- merge(dataCEO_combo, data_mapstrata[c('pl_change_strata','plotid')], by.x =c('PLOTID'), by.y = c('plotid'), all.x=T)
head(datamerged)
dim(datamerged)
write.csv(datamerged, file = 'Results\\Datamerged_v2_needfix736940.csv', row.names = F)
#########################################################################################
####Found a couple illogical points which were removed, and the fixed file was reimported
#############Re-import created file after illogical points fixed by Trina and Liz
#datamerged_FIXED <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\Philippines_compiled_checked_TrinaLiz.csv")
datamerged_FIXED <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\Datamerged_v2_fixcompleted.csv")
dim(datamerged_FIXED)
colnames(datamerged_FIXED)
colnames(datamerged_FIXED)[33]<-"ChangeStrata_Map"
#datamerged_FIXED <- datamerged_FIXED[-c(43:48)]

#add a human readable column for change strata
datamerged_FIXED$ReadableChangeStrata_Map_v1<-"fixMe"
datamerged_FIXED$ReadableChangeStrata_Map_v1[datamerged_FIXED$ChangeStrata_Map == 31]<-"deforested epoch 1"
datamerged_FIXED$ReadableChangeStrata_Map_v1[datamerged_FIXED$ChangeStrata_Map == 32]<-"deforested epoch 2 & 3"
datamerged_FIXED$ReadableChangeStrata_Map_v1[datamerged_FIXED$ChangeStrata_Map == 41]<-"reforested epoch 1"
datamerged_FIXED$ReadableChangeStrata_Map_v1[datamerged_FIXED$ChangeStrata_Map == 42]<-"reforested epoch 2 & 3"
datamerged_FIXED$ReadableChangeStrata_Map_v1[datamerged_FIXED$ChangeStrata_Map == 50]<-"stable forest"
datamerged_FIXED$ReadableChangeStrata_Map_v1[datamerged_FIXED$ChangeStrata_Map == 60]<-"stable non forest"
datamerged_FIXED$ReadableChangeStrata_Map_v1[datamerged_FIXED$ChangeStrata_Map == 70]<-"multiple events noise"
datamerged_FIXED$ReadableChangeStrata_Map_v1[datamerged_FIXED$ChangeStrata_Map == 80]<-"multiple events ecologically possible"
unique(datamerged_FIXED$ReadableChangeStrata_Map_v1)
colnames(datamerged_FIXED)

##### Epoch 1 = 2000-2005
##### Epoch 2 = 2006-2012
##### Epoch 3 = 2013-2018

########Simplified Map data
datamerged_FIXED$ReadableChangeStrata_Map_v2<-"fixMe"
datamerged_FIXED$ReadableChangeStrata_Map_v2[datamerged_FIXED$ChangeStrata_Map == 31]<-"Deforestation"
datamerged_FIXED$ReadableChangeStrata_Map_v2[datamerged_FIXED$ChangeStrata_Map == 32]<-"Deforestation"
datamerged_FIXED$ReadableChangeStrata_Map_v2[datamerged_FIXED$ChangeStrata_Map == 41]<-"Reforestation"
datamerged_FIXED$ReadableChangeStrata_Map_v2[datamerged_FIXED$ChangeStrata_Map == 42]<-"Reforestation"
datamerged_FIXED$ReadableChangeStrata_Map_v2[datamerged_FIXED$ChangeStrata_Map == 50]<-"stable forest"
datamerged_FIXED$ReadableChangeStrata_Map_v2[datamerged_FIXED$ChangeStrata_Map == 60]<-"stable non forest"
datamerged_FIXED$ReadableChangeStrata_Map_v2[datamerged_FIXED$ChangeStrata_Map == 70]<-"multiple events noise"
datamerged_FIXED$ReadableChangeStrata_Map_v2[datamerged_FIXED$ChangeStrata_Map == 80]<-"multiple events ecologically possible"
unique(datamerged_FIXED$ReadableChangeStrata_Map_v2)
colnames(datamerged_FIXED)



########Simplified Map data


############## Strata pixel counts #############
strataAreas <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\philippines_pixelcounts.csv")
head(strataAreas)
colnames(strataAreas)[1]<-"MapValue"
colnames(strataAreas)[2]<-"StrataName"
colnames(strataAreas)[3]<-"PixelCount"
datamerged_FIXED <- merge(datamerged_FIXED, strataAreas, by.x = 'ChangeStrata_Map', by.y = 'MapValue', all.x = F)
dim(datamerged_FIXED)

############Set up clean CEO labels - Version 1
#Right now the order in which you define these matters
#In version 1: Pixels with multiple events are not separated out
datamerged_FIXED$CEOreadable_v1 <- ifelse(datamerged_FIXED$ChangeType1 == "Degradation" | datamerged_FIXED$ChangeType2 == "Degradation" | datamerged_FIXED$ChangeType3 == "Degradation" , 'Degradation',
                                  ifelse(datamerged_FIXED$ChangeType1 == "Deforestation"| datamerged_FIXED$ChangeType2 == "Deforestation" | datamerged_FIXED$ChangeType3 == "Deforestation", 'Deforestation', 
                                         ifelse(datamerged_FIXED$ChangeType1 == "Reforestation"| datamerged_FIXED$ChangeType2 == "Reforestation" | datamerged_FIXED$ChangeType3 == "Reforestation", 'Reforestation', 
                                                ifelse(datamerged_FIXED$forestChangeEvent == "N/A non-forest entire time", "stable non forest",
                                                       ifelse(datamerged_FIXED$forestChangeEvent == "no", "stable forest", 'NotReviewed')))))
table(datamerged_FIXED$CEOreadable_v1)
#write.csv(datamerged_FIXED, file = 'Results\\Philippines_Test1.csv', row.names = F)

############Set up clean CEO labels - Version 2
#In version 2: Pixels with multiple events ARE separated out
'%!in%' <- Negate('%in%')
datamerged_FIXED$CEOreadable_v2 <- ifelse(datamerged_FIXED$ChangeType1 == "Degradation" & datamerged_FIXED$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration"), 'Degradation',
                                    ifelse(datamerged_FIXED$ChangeType1 == "Deforestation" & datamerged_FIXED$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration"), 'Deforestation', 
                                           ifelse(datamerged_FIXED$ChangeType1 == "Reforestation" & datamerged_FIXED$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration"), 'Reforestation',
                                                  ifelse(datamerged_FIXED$morethan3forestchanges == "no", 'multiple events ecologically possible',
                                                         ifelse(datamerged_FIXED$morethan3forestchanges == "yes", 'multiple events noise',
                                                                ifelse(datamerged_FIXED$ChangeType2 %in% c("Degradation","Deforestation","Regeneration") & datamerged_FIXED$ThirdForestChangeEvent == "no", 'multiple events ecologically possible',
                                                                       ifelse(datamerged_FIXED$forestChangeEvent == "N/A non-forest entire time", "stable non forest",
                                                                              ifelse(datamerged_FIXED$forestChangeEvent == "no", "stable forest", 'NotReviewed'))))))))
table(datamerged_FIXED$CEOreadable_v2)
write.csv(datamerged_FIXED, file = 'Results\\Philippines_Test2.csv', row.names = F)

############Set up clean CEO labels - Version 3
#In version 3: Pixels with multiple events are combined
datamerged_FIXED$CEOreadable_v3 <- ifelse(datamerged_FIXED$ChangeType1 == "Degradation" & datamerged_FIXED$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration"), 'Degradation',
                                          ifelse(datamerged_FIXED$ChangeType1 == "Deforestation" & datamerged_FIXED$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration"), 'Deforestation', 
                                                 ifelse(datamerged_FIXED$ChangeType1 == "Reforestation" & datamerged_FIXED$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration"), 'Reforestation',
                                                        ifelse(datamerged_FIXED$morethan3forestchanges == "no", 'multiple events',
                                                               ifelse(datamerged_FIXED$morethan3forestchanges == "yes", 'multiple events',
                                                                      ifelse(datamerged_FIXED$ChangeType2 %in% c("Degradation","Deforestation","Regeneration") & datamerged_FIXED$ThirdForestChangeEvent == "no", 'multiple events',
                                                                             ifelse(datamerged_FIXED$forestChangeEvent == "N/A non-forest entire time", "stable non forest",
                                                                                    ifelse(datamerged_FIXED$forestChangeEvent == "no", "stable forest", 'NotReviewed'))))))))
table(datamerged_FIXED$CEOreadable_v3)
write.csv(datamerged_FIXED, file = 'Results\\Philippines_Test3.csv', row.names = F)

############Set up clean CEO labels - Version 4
#In version 3: Pixels with multiple events are combined, agroforestry pulled out
datamerged_FIXED$CEOreadable_v4 <- ifelse(datamerged_FIXED$ChangeType1 == "Degradation" & datamerged_FIXED$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration"), 'Degradation',
                                          ifelse(datamerged_FIXED$ChangeType1 == "Deforestation" & datamerged_FIXED$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration"), 'Deforestation', 
                                                 ifelse(datamerged_FIXED$ChangeType1 == "Reforestation" & datamerged_FIXED$ChangeType2 %!in% c("Degradation","Deforestation","Regeneration"), 'Reforestation',
                                                        ifelse(datamerged_FIXED$morethan3forestchanges == "no", 'multiple events',
                                                               ifelse(datamerged_FIXED$morethan3forestchanges == "yes", 'multiple events',
                                                                      ifelse(datamerged_FIXED$ChangeType2 %in% c("Degradation","Deforestation","Regeneration") & datamerged_FIXED$ThirdForestChangeEvent == "no", 'multiple events',
                                                                             ifelse(datamerged_FIXED$forestChangeEvent == "N/A non-forest entire time" & datamerged_FIXED$CropType2000 %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest Agroforestry",
                                                                                    ifelse(datamerged_FIXED$forestChangeEvent == "N/A non-forest entire time" & datamerged_FIXED$CropType2000 %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest",
                                                                                    ifelse(datamerged_FIXED$forestChangeEvent == "no", "stable forest", 'NotReviewed')))))))))
table(datamerged_FIXED$CEOreadable_v4)
write.csv(datamerged_FIXED, file = 'Results\\Philippines_Test4.csv', row.names = F)
#############################################################
#############################################################
#   ANALYSIS
#############################################################
#############################################################
head(datamerged_FIXED)

#cross tab of original strata to agroforestry removed strata
table(datamerged_FIXED$StrataName, datamerged_FIXED$CEOreadable_v4)
table_maporig_ceov4 <- table(datamerged_FIXED$StrataName, datamerged_FIXED$CEOreadable_v4)
write.csv(table_maporig_ceov4, file = 'Results\\CrossTable_maporigstrata_ceov4_v2.csv', row.names = T)
table(datamerged_FIXED$PixelCount,datamerged_FIXED$StrataName)

#cross tab of simplified mapv3 and ceo v4 agroforestry removed strata
table(datamerged_FIXED$ReadableChangeStrata_Map_v3, datamerged_FIXED$CEOreadable_v4)
table_mapv3_ceov4 <- table(datamerged_FIXED$ReadableChangeStrata_Map_v3, datamerged_FIXED$CEOreadable_v4)
write.csv(table_mapv3_ceov4, file = 'Results\\CrossTable_mapv3_ceov4_simple.csv', row.names = T)


#cross tab of strata v2
table(datamerged_FIXED$ReadableChangeStrata_Map_v2, datamerged_FIXED$CEOreadable_v2)
table_mapv2_CEOv2 <- table(datamerged_FIXED$ReadableChangeStrata_Map_v2, datamerged_FIXED$CEOreadable_v2)
head(strataAreas)
#cross tab of strata v3
table(datamerged_FIXED$ReadableChangeStrata_Map_v3, datamerged_FIXED$CEOreadable_v3)
table_mapv3_CEOv3 <- table(datamerged_FIXED$ReadableChangeStrata_Map_v3, datamerged_FIXED$CEOreadable_v3)
write.csv(table_mapv3_CEOv3, file = 'Results\\CrossTable_mapv3_ceov3.csv', row.names = T)

#cross tab of strata v4
table(datamerged_FIXED$ReadableChangeStrata_Map_v3, datamerged_FIXED$CEOreadable_v4)
table_mapv3_CEOv4 <- table(datamerged_FIXED$ReadableChangeStrata_Map_v3, datamerged_FIXED$CEOreadable_v4)
write.csv(table_mapv3_CEOv4, file = 'Results\\CrossTable_mapv3_ceov4.csv', row.names = T)

###########label the points that we want to review in CEO
datamerged_FIXED$CEOreviewQAQC <-"No"
datamerged_FIXED$CEOreviewQAQC <- ifelse(datamerged_FIXED$ReadableChangeStrata_Map_v3 == "multiple events" & datamerged_FIXED$CEOreadable_v4 == "Deforestation", 'LabelLoss-MapMulti', #9
                                          ifelse(datamerged_FIXED$ReadableChangeStrata_Map_v3 == "Reforestation" & datamerged_FIXED$CEOreadable_v4 == "Deforestation", 'LabelLoss-MapReforest', #2
                                                 ifelse(datamerged_FIXED$ReadableChangeStrata_Map_v3 == "stable forest" & datamerged_FIXED$CEOreadable_v4 == "Deforestation", 'LabelLoss-MapForest', #10
                                                        ifelse(datamerged_FIXED$ReadableChangeStrata_Map_v3 == "stable non forest" & datamerged_FIXED$CEOreadable_v4 == "Deforestation", 'LabelLoss-MapNonForest', #5
                                                                      ifelse(datamerged_FIXED$ReadableChangeStrata_Map_v3 == "Reforestation" & datamerged_FIXED$CEOreadable_v4 == "Degradation", 'LabelDeg-MapReforest', #4
                                                                             ifelse(datamerged_FIXED$ReadableChangeStrata_Map_v3 == "Deforestation" & datamerged_FIXED$CEOreadable_v4 == "Reforestation", 'LabelReforest-MapLoss', #4
                                                                                    ifelse(datamerged_FIXED$ReadableChangeStrata_Map_v3 == "Deforestation" & datamerged_FIXED$CEOreadable_v4 == "stable forest", 'LabelForest-MapLoss', #24
                                                                                                                ifelse(datamerged_FIXED$ReadableChangeStrata_Map_v3 == "Reforestation" & datamerged_FIXED$CEOreadable_v4 == "stable forest", 'LabelForest-MapReforest', #31
                                                                                           ifelse(datamerged_FIXED$ReadableChangeStrata_Map_v3 == "multiple events" & datamerged_FIXED$CEOreadable_v4 == "stable forest", 'LabelForest-MapMulti','No'))))))))) #26

table(datamerged_FIXED$CEOreviewQAQC)
write.csv(datamerged_FIXED, file = 'Results\\LabeledCSV_CEOtoReview.csv', row.names = T)

########select nonforest labeled points for review
datamerged_FIXED$CEOreviewQAQC_nonforestlabels <- ifelse(datamerged_FIXED$ReadableChangeStrata_Map_v3 == "Deforestation" & datamerged_FIXED$CEOreadable_v4 == "stable nonforest", 'LabelNonForest-MapLoss',
              ifelse(datamerged_FIXED$ReadableChangeStrata_Map_v3 == "multiple events" & datamerged_FIXED$CEOreadable_v4 == "stable nonforest", 'LabelNonForest-MapMulti',
                     ifelse(datamerged_FIXED$ReadableChangeStrata_Map_v3 == "Reforestation" & datamerged_FIXED$CEOreadable_v4 == "stable nonforest", 'LabelNonForest-MapReforest', 'no')))
table(datamerged_FIXED$CEOreviewQAQC_nonforestlabels)

########select mixednoise mapped points for review
datamerged_FIXED$CEOreviewQAQC_mappednoise <- ifelse(datamerged_FIXED$StrataName == "multiple events noise" & datamerged_FIXED$CEOreadable_v4 == "Degradation", 'LabelDegradation-MapMulti',
                                                         ifelse(datamerged_FIXED$StrataName == "multiple events noise" & datamerged_FIXED$CEOreadable_v4 == "Reforestation", 'LabelReforestation-MapMulti',
                                                                ifelse(datamerged_FIXED$StrataName == "multiple events noise" & datamerged_FIXED$CEOreadable_v4 == "multiple events", 'LabelMulti-MapMulti',
                                                                ifelse(datamerged_FIXED$StrataName == "multiple events noise" & datamerged_FIXED$CEOreadable_v4 == "Deforestation", 'LabelDeforestation-MapMulti', 
                                                                       ifelse(datamerged_FIXED$StrataName == "multiple events noise" & datamerged_FIXED$CEOreadable_v4 == "stable forest", 'LabelForest-MapMulti', 
                                                                       ifelse(datamerged_FIXED$StrataName == "multiple events noise" & datamerged_FIXED$CEOreadable_v4 == "stable nonforest", 'LabelNonforest-MapMulti',      
                                                                ifelse(datamerged_FIXED$StrataName == "multiple events noise" & datamerged_FIXED$CEOreadable_v4 == "stable nonforest Agroforestry", 'LabelPerennial-MapMulti', 'no')))))))
table(datamerged_FIXED$CEOreviewQAQC_mappednoise)
write.csv(datamerged_FIXED, file = 'Results\\LabeledCSV_CEOtoReview_mappednoise.csv', row.names = F)

########select labeled perennial points for review
datamerged_FIXED$CEOreviewQAQC_labeledperennial <- ifelse(datamerged_FIXED$StrataName == "reforested epoch 1" & datamerged_FIXED$CEOreadable_v4 == "stable nonforest Agroforestry", 'LabelPerennial-MapREF1',
                                                     ifelse(datamerged_FIXED$StrataName == "reforested epoch 2 & 3" & datamerged_FIXED$CEOreadable_v4 == "stable nonforest Agroforestry", 'LabelPerennial-MapREF23',
                                                            ifelse(datamerged_FIXED$StrataName == "deforested epoch 1" & datamerged_FIXED$CEOreadable_v4 == "stable nonforest Agroforestry", 'LabelPerennial-MapDEF1',
                                                                   ifelse(datamerged_FIXED$StrataName == "deforested epoch 2 & 3" & datamerged_FIXED$CEOreadable_v4 == "stable nonforest Agroforestry", 'LabelPerennial-MapDEF23', 
                                                                          ifelse(datamerged_FIXED$StrataName == "multiple events ecologically possible" & datamerged_FIXED$CEOreadable_v4 == "stable nonforest Agroforestry", 'LabelPerennial-MapMulti',
                                                                                 ifelse(datamerged_FIXED$StrataName == "multiple events noise" & datamerged_FIXED$CEOreadable_v4 == "stable nonforest Agroforestry", 'LabelPerennial-MapMultiNoise',       
                                                                                 ifelse(datamerged_FIXED$StrataName == "stable forest" & datamerged_FIXED$CEOreadable_v4 == "stable nonforest Agroforestry", 'LabelPerennial-MapFOREST',      
                                                                                        ifelse(datamerged_FIXED$StrataName == "stable non forest" & datamerged_FIXED$CEOreadable_v4 == "stable nonforest Agroforestry", 'LabelPerennial-MapNONFOREST', 'no'))))))))
table(datamerged_FIXED$CEOreviewQAQC_labeledperennial)
write.csv(datamerged_FIXED, file = 'Results\\USETHIS4CEO_LabeledPerennial_CEOtoReview.csv', row.names = F)
table(duplicated(datamerged_FIXED[c("lon","lat")]))
table(duplicated(datamerged_FIXED$lon))
table(duplicated(datamerged_FIXED$lat))
#########################################
## Set up sample design
#########################################
strat_design <- svydesign(id = ~1, strata = ~StrataName, fpc = ~PixelCount, 
                          data = datamerged_FIXED)
########################################
## once sample design is set up you can analyze the data
#########################################
## survey total (svytotal) calculates area weighted totals of data
?svydesign()

activityData <- svytotal(~CEOreadable_v4, strat_design)
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
write.csv(Change, file = 'Results\\AreasofDisturbance_ceoV4version_SAVE_v2_850.csv', row.names = T)

###########################################################################
############################################################################
###########################################################################
############################################################################
##After review there were a few points that were double checked because
##there were more off-diagonal points than were expected in the confusion matrix
##Below is the updated analysis with those points altered

datamerged_FIXED_v2 <- read.csv('C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\ceo-Philippines_QAQCReview_MapInterpreterDisagreement-sample-data-FIXEDhandedit.csv')
colnames(datamerged_FIXED_v2)
datamerged_FIXED_v2 <- datamerged_FIXED_v2[-c(1,3:80)]
colnames(datamerged_FIXED_v2)
colnames(datamerged_FIXED)
head(datamerged_FIXED$PLOTID)
head(datamerged_FIXED_v2$sampleid)

#########More points were reviewed by SMEs
#SMEreviewdData_toreplaceold <- read.csv('C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\ceo-ALLPOINTS_Philippines_QAQCReview_MapInterpreterDisagreement-sample-data-2022-05-04_SMEreview.csv')
#SMEreviewdData_toreplaceold <- SMEreviewdData_toreplaceold[-c(1,5:55)]
#SMEreviewdData_toreplaceold <- SMEreviewdData_toreplaceold[-c(1,6:11,17:18)]


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


#FINALDATASET <- merge(datamerged_FIXED, SMEreviewdData_toreplaceold, by.x = c("PLOTID"), by.y = c("sampleid"), all.x = T)

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
############Set up clean CEO labels - Version 4 NEW
#In version 4: Pixels with multiple events are combined, agroforestry pulled out
FINALDATASET$CEOreadable_v4_NEWb <- ifelse(FINALDATASET$ChangeType1b == "Degradation" & FINALDATASET$ChangeType2b %!in% c("Degradation","Deforestation","Regeneration"), 'Degradation',
                                          ifelse(FINALDATASET$ChangeType1b == "Deforestation" & FINALDATASET$ChangeType2b %!in% c("Degradation","Deforestation","Regeneration"), 'Deforestation', 
                                                 ifelse(FINALDATASET$ChangeType1b == "Reforestation" & FINALDATASET$ChangeType2b %!in% c("Degradation","Deforestation","Regeneration"), 'Reforestation',
                                                        ifelse(FINALDATASET$morethan3forestchangesb == "no", 'multiple events',
                                                               ifelse(FINALDATASET$morethan3forestchangesb == "yes", 'multiple events',
                                                                      ifelse(FINALDATASET$ChangeType2b %in% c("Degradation","Deforestation","Regeneration") & FINALDATASET$ThirdForestChangeEventb == "no", 'multiple events',
                                                                             ifelse(FINALDATASET$forestChangeEventb == "N/A non-forest entire time" & FINALDATASET$CropType2000b %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest Agroforestry",
                                                                                    ifelse(FINALDATASET$forestChangeEventb == "N/A non-forest entire time" & FINALDATASET$CropType2000b %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest",
                                                                                           ifelse(FINALDATASET$forestChangeEventb == "no", "stable forest", 'NotReviewed')))))))))
table(FINALDATASET$CEOreadable_v4_NEWb)


write.csv(SMEreviewdData_toreplaceold, file = 'Results\\TEST_CEOreadable_v4output.csv', row.names = F)

#In version 5: Pixels with multiple events are combined, agroforestry pulled out, separated by epoch
FINALDATASET$CEOreadable_v5_NEWb <- ifelse(FINALDATASET$ChangeType1b == "Degradation" & FINALDATASET$ChangeType2b %!in% c("Degradation","Deforestation","Regeneration"), 'Degradation',
                                           ifelse(FINALDATASET$ChangeType1b == "Deforestation" & FINALDATASET$ChangeType2b %!in% c("Degradation","Deforestation","Regeneration") & FINALDATASET$Change1yearb %in% c(2000:2005), 'Deforestation Epoch1', 
                                           ifelse(FINALDATASET$ChangeType1b == "Deforestation" & FINALDATASET$ChangeType2b %!in% c("Degradation","Deforestation","Regeneration") & FINALDATASET$Change1yearb %in% c(2006:2018), 'Deforestation Epoch23',
                                                  ifelse(FINALDATASET$ChangeType1b == "Reforestation" & FINALDATASET$ChangeType2b %!in% c("Degradation","Deforestation","Regeneration") & FINALDATASET$Change1yearb %in% c(2000:2005), 'Reforestation Epoch1',
                                                         ifelse(FINALDATASET$ChangeType1b == "Reforestation" & FINALDATASET$ChangeType2b %!in% c("Degradation","Deforestation","Regeneration") & FINALDATASET$Change1yearb %in% c(2006:2018), 'Reforestation Epoch23',       
                                                         ifelse(FINALDATASET$morethan3forestchangesb == "no", 'multiple events eco possible',
                                                                ifelse(FINALDATASET$morethan3forestchangesb == "yes", 'multiple events noise',
                                                                       ifelse(FINALDATASET$ChangeType2b %in% c("Degradation","Deforestation","Regeneration") & FINALDATASET$ThirdForestChangeEventb == "no", 'multiple events eco possible',
                                                                              ifelse(FINALDATASET$forestChangeEventb == "N/A non-forest entire time" & FINALDATASET$CropType2000b %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest Agroforestry",
                                                                                     ifelse(FINALDATASET$forestChangeEventb == "N/A non-forest entire time" & FINALDATASET$CropType2000b %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest",
                                                                                            ifelse(FINALDATASET$forestChangeEventb == "no", "stable forest", 'NotReviewed')))))))))))
table(FINALDATASET$CEOreadable_v5_NEWb)


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


#cross tab of NEW simplified mapv3 and ceo v4 agroforestry removed strata
table(FINALDATASET$ReadableChangeStrata_Map_v3, FINALDATASET$CEOreadable_v4_NEWb)
table_mapv3_ceov4 <- table(FINALDATASET$ReadableChangeStrata_Map_v3, FINALDATASET$CEOreadable_v4_NEWb)
write.csv(table_mapv3_ceov4, file = 'Results\\CrossTable_mapv3_ceov4_simple_v2.csv', row.names = T)

#cross tab of NEW strataorig and ceov5, agroforestry pulled out, separated by epoch
table(FINALDATASET$StrataName, FINALDATASET$CEOreadable_v5_NEWb)
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

