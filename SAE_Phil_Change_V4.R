#Unbiased Area Estimation of Philippines Forest Change
#Written by Karis Tenneson and Crystal Wespestad (SIG) Feb, 2022
#####################################################################
#Notes: epochs are 2000-2005, 2006-2012, 2013-2018
# there are 850 points total, 150 have been reviewed in duplicate 

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

#original data
#not used because 150 duplicate answers had not yet been reviewed and removed
#contains 1000 points
#data_mapstrata <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\PhilippinesPointsCombo_Strata.csv")
#dim(data_mapstrata)
#colnames(data_mapstrata)


############## Strata pixel counts #############
strataAreas <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\philippines_pixelcounts.csv")
head(strataAreas)
colnames(strataAreas)[1]<-"MapValue"
colnames(strataAreas)[2]<-"StrataName"
colnames(strataAreas)[3]<-"PixelCount"
strataAreas <- strataAreas[-c(4:5)]


############needed later
'%!in%' <- Negate('%in%')

#this file will be merged with datamerged_FIXED_v2 later
#contains 850 points
#Duplicates have been reviewed and removed. 
datamerged_FIXED <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\Datamerged_v2_fixcompleted.csv")
dim(datamerged_FIXED)
colnames(datamerged_FIXED)



##After review there were a few points that were double checked because
##there were more off-diagonal points than were expected in the confusion matrix
##Below is the updated analysis with those points altered
#This data file has been cleaned up so column answers are in the right place
#only 100 points in this file
datamerged_FIXED_v2 <- read.csv('C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\ceo-Philippines_QAQCReview_MapInterpreterDisagreement-sample-data-2022-05-16_FIXforSMEpart1_DuplicatesRemoved.csv')
colnames(datamerged_FIXED_v2)
datamerged_FIXED_v2 <- datamerged_FIXED_v2[-c(1,6:80)]
colnames(datamerged_FIXED_v2)
dim(datamerged_FIXED_v2)
colnames(datamerged_FIXED)
#write.csv(datamerged_FIXED_v2, file = 'Results\\CHECK850.csv', row.names = F)
colnames(datamerged_FIXED_v2)[1]<-"PLOTIDb"
colnames(datamerged_FIXED_v2)[2]<-"lonb"
colnames(datamerged_FIXED_v2)[3]<-"latb"
colnames(datamerged_FIXED_v2)[4]<-"emailb"
colnames(datamerged_FIXED_v2)[10]<-"Change1yearb"
colnames(datamerged_FIXED_v2)[15]<-"Change2yearb"
colnames(datamerged_FIXED_v2)[22]<-"Change3yearb"
colnames(datamerged_FIXED_v2)[31]<-"Confidenceb"
colnames(datamerged_FIXED_v2)[32]<-"ConfidenceReasoningb"
colnames(datamerged_FIXED_v2)[5]<-"LC2000b"
colnames(datamerged_FIXED_v2)[6]<-"ForestType2000b"
colnames(datamerged_FIXED_v2)[7]<-"NonForestType2000b" 
colnames(datamerged_FIXED_v2)[8]<-"CropType2000b" 
colnames(datamerged_FIXED_v2)[26]<-"LC2018endb"
colnames(datamerged_FIXED_v2)[9]<-"forestChangeEventb" 
colnames(datamerged_FIXED_v2)[23]<-"ChangeType1b"
colnames(datamerged_FIXED_v2)[25]<-"DegradationDriver1b"
colnames(datamerged_FIXED_v2)[24]<-"LossDriver1b"
colnames(datamerged_FIXED_v2)[11]<-"SecondForestChangeEventb"
colnames(datamerged_FIXED_v2)[12]<-"ChangeType2b"
colnames(datamerged_FIXED_v2)[13]<-"DegradationDriver2b"
colnames(datamerged_FIXED_v2)[14]<-"LossDriver2b"
colnames(datamerged_FIXED_v2)[16]<-"ThirdForestChangeEventb" 
colnames(datamerged_FIXED_v2)[17]<-"ChangeType3b"
colnames(datamerged_FIXED_v2)[19]<-"DegradationDriver3b"
colnames(datamerged_FIXED_v2)[18]<-"LossDriver3b"
colnames(datamerged_FIXED_v2)[20]<-"morethan3forestchangesb" 
colnames(datamerged_FIXED_v2)[21]<-"DescribeExtraChangesb" 
colnames(datamerged_FIXED_v2)[27]<-"ForestType2018b" 
colnames(datamerged_FIXED_v2)[28]<-"NonForestType2018b" 
colnames(datamerged_FIXED_v2)[29]<-"CropType2018b" 
colnames(datamerged_FIXED_v2)[30]<-"mixedLCpixelb"
colnames(datamerged_FIXED_v2)


#########More points were reviewed by SMEs, 41 points
#SMEreviewdData_toreplaceold <- read.csv('C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\reviewed41_bestlabel.csv')
#in this version the 3 missing data cells were replaced
#PLOTID 234, 919, 650 had "no" added to "ThirdForestChangeEventd" instead of an empty cell. This was an error in the original data collection.
#ChangeYear3C was changed from "no" to "NA" for 234
SMEreviewdData_toreplaceold <- read.csv('C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\reviewed41_bestlabel_fixed3missingdatacells.csv')
dim(SMEreviewdData_toreplaceold)
colnames(SMEreviewdData_toreplaceold)
#write.csv(SMEreviewdData_toreplaceold, file = 'Results\\CHECK41.csv', row.names = F)

############# merging datasets 850 + 100 double checked from off-diagonals
datamerged_combo12 <- merge(datamerged_FIXED, datamerged_FIXED_v2, by.x = c('lon','lat'), by.y = c('lonb','latb'), all.x = T)
#find duplicates in PLOTID: 19  71  82 266 315 372 445 652 669 718 814 863 901 903 924
sort(datamerged_combo12$PLOTIDb[duplicated(datamerged_combo12$PLOTID)])
dim(datamerged_combo12)

############# merge (850+100review+41reviewed) --- the reviews will replace olds for a total of 850
SMEreviewdData_toreplaceold <- merge(datamerged_combo12, SMEreviewdData_toreplaceold, by.x = c('lon','lat'), by.y = c('lonc','latc'), all.x = T)
dim(SMEreviewdData_toreplaceold)
#write.csv(SMEreviewdData_toreplaceold, file = 'Results\\ARELATLONGSWRONG.csv', row.names = F)

####Fill in all the values for the rows that were not reviewed with the original answers
SMEreviewdData_toreplaceold$EMAILc <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$EMAILc, SMEreviewdData_toreplaceold$emailb)
SMEreviewdData_toreplaceold$LC2000c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$LC2000c, SMEreviewdData_toreplaceold$LC2000b)
SMEreviewdData_toreplaceold$ForestType2000c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$ForestType2000c, SMEreviewdData_toreplaceold$ForestType2000b)
SMEreviewdData_toreplaceold$NonForestType2000c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$NonForestType2000c, SMEreviewdData_toreplaceold$NonForestType2000b)
SMEreviewdData_toreplaceold$CropType2000c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$CropType2000c, SMEreviewdData_toreplaceold$CropType2000b)
SMEreviewdData_toreplaceold$forestChangeEventc <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$forestChangeEventc, SMEreviewdData_toreplaceold$forestChangeEventb)
SMEreviewdData_toreplaceold$Change1yearc <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$Change1yearc, SMEreviewdData_toreplaceold$Change1yearb)
SMEreviewdData_toreplaceold$SecondForestChangeEventc <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$SecondForestChangeEventc, SMEreviewdData_toreplaceold$SecondForestChangeEventb)
SMEreviewdData_toreplaceold$ChangeType2c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$ChangeType2c, SMEreviewdData_toreplaceold$ChangeType2b)
SMEreviewdData_toreplaceold$DegradationDriver2c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$DegradationDriver2c, SMEreviewdData_toreplaceold$DegradationDriver2b)
SMEreviewdData_toreplaceold$LossDriver2c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$LossDriver2c, SMEreviewdData_toreplaceold$LossDriver2b)
SMEreviewdData_toreplaceold$Change2yearc <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$Change2yearc, SMEreviewdData_toreplaceold$Change2yearb)
SMEreviewdData_toreplaceold$ThirdForestChangeEventc <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$ThirdForestChangeEventc, SMEreviewdData_toreplaceold$ThirdForestChangeEventb)
SMEreviewdData_toreplaceold$ChangeType3c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$ChangeType3c, SMEreviewdData_toreplaceold$ChangeType3b)
SMEreviewdData_toreplaceold$LossDriver3c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$LossDriver3c, SMEreviewdData_toreplaceold$LossDriver3b)
SMEreviewdData_toreplaceold$DegradationDriver3c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$DegradationDriver3c, SMEreviewdData_toreplaceold$DegradationDriver3b)
SMEreviewdData_toreplaceold$morethan3forestchangesc <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$morethan3forestchangesc, SMEreviewdData_toreplaceold$morethan3forestchangesb)
SMEreviewdData_toreplaceold$DescribeExtraChangesc <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$DescribeExtraChangesc, SMEreviewdData_toreplaceold$DescribeExtraChangesb)
SMEreviewdData_toreplaceold$Change3yearc <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$Change3yearc, SMEreviewdData_toreplaceold$Change3yearb)
SMEreviewdData_toreplaceold$ChangeType1c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$ChangeType1c, SMEreviewdData_toreplaceold$ChangeType1b)
SMEreviewdData_toreplaceold$LossDriver1c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$LossDriver1c, SMEreviewdData_toreplaceold$LossDriver1b)
SMEreviewdData_toreplaceold$DegradationDriver1c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$DegradationDriver1c, SMEreviewdData_toreplaceold$DegradationDriver1b)
SMEreviewdData_toreplaceold$LC2018endc <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$LC2018endc, SMEreviewdData_toreplaceold$LC2018endb)
SMEreviewdData_toreplaceold$ForestType2018c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$ForestType2018c, SMEreviewdData_toreplaceold$ForestType2018b)
SMEreviewdData_toreplaceold$NonForestType2018c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$NonForestType2018c, SMEreviewdData_toreplaceold$NonForestType2018b)
SMEreviewdData_toreplaceold$CropType2018c <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$CropType2018c, SMEreviewdData_toreplaceold$CropType2018b)
SMEreviewdData_toreplaceold$mixedLCpixelc <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$mixedLCpixelc, SMEreviewdData_toreplaceold$mixedLCpixelb)
SMEreviewdData_toreplaceold$Confidencec <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$Confidencec, SMEreviewdData_toreplaceold$Confidenceb)
SMEreviewdData_toreplaceold$ConfidenceReasoningc <- ifelse(SMEreviewdData_toreplaceold$emailb %!in% c('tgisorena@yahoo.com','leizeldelacruz@gmail.com'), SMEreviewdData_toreplaceold$ConfidenceReasoningc, SMEreviewdData_toreplaceold$ConfidenceReasoningb)

write.csv(SMEreviewdData_toreplaceold, file = 'Results\\CHECKCHANGEYEAR3.csv', row.names = F)

####Fill in all the values for the rows that were not reviewed with the original answers
SMEreviewdData_toreplaceold$EMAILd <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$EMAIL, SMEreviewdData_toreplaceold$EMAILc)
SMEreviewdData_toreplaceold$LC2000d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$LC2000a, SMEreviewdData_toreplaceold$LC2000c)
SMEreviewdData_toreplaceold$ForestType2000d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$ForestType2000, SMEreviewdData_toreplaceold$ForestType2000c)
SMEreviewdData_toreplaceold$NonForestType2000d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$NonForestType2000, SMEreviewdData_toreplaceold$NonForestType2000c)
SMEreviewdData_toreplaceold$CropType2000d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$CropType2000, SMEreviewdData_toreplaceold$CropType2000c)
SMEreviewdData_toreplaceold$forestChangeEventd <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$forestChangeEvent, SMEreviewdData_toreplaceold$forestChangeEventc)
SMEreviewdData_toreplaceold$Change1yeard <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$Change1year, SMEreviewdData_toreplaceold$Change1yearc)
SMEreviewdData_toreplaceold$SecondForestChangeEventd <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$SecondForestChangeEvent, SMEreviewdData_toreplaceold$SecondForestChangeEventc)
SMEreviewdData_toreplaceold$ChangeType2d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$ChangeType2, SMEreviewdData_toreplaceold$ChangeType2c)
SMEreviewdData_toreplaceold$DegradationDriver2d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$DegradationDriver2c, SMEreviewdData_toreplaceold$DegradationDriver2c)
SMEreviewdData_toreplaceold$LossDriver2d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$LossDriver2, SMEreviewdData_toreplaceold$LossDriver2c)
SMEreviewdData_toreplaceold$Change2yeard <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$Change2year, SMEreviewdData_toreplaceold$Change2yearc)
SMEreviewdData_toreplaceold$ThirdForestChangeEventd <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$ThirdForestChangeEvent, SMEreviewdData_toreplaceold$ThirdForestChangeEventc)
SMEreviewdData_toreplaceold$ChangeType3d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$ChangeType3, SMEreviewdData_toreplaceold$ChangeType3c)
SMEreviewdData_toreplaceold$LossDriver3d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$LossDriver3, SMEreviewdData_toreplaceold$LossDriver3c)
SMEreviewdData_toreplaceold$DegradationDriver3d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$DegradationDriver3, SMEreviewdData_toreplaceold$DegradationDriver3c)
SMEreviewdData_toreplaceold$morethan3forestchangesd <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$morethan3forestchanges, SMEreviewdData_toreplaceold$morethan3forestchangesc)
SMEreviewdData_toreplaceold$DescribeExtraChangesd <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$DescribeExtraChanges, SMEreviewdData_toreplaceold$DescribeExtraChangesc)
SMEreviewdData_toreplaceold$Change3yeard <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$Change3year, SMEreviewdData_toreplaceold$Change3yearc)
SMEreviewdData_toreplaceold$ChangeType1d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$ChangeType1, SMEreviewdData_toreplaceold$ChangeType1c)
SMEreviewdData_toreplaceold$LossDriver1d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$ChangeType1, SMEreviewdData_toreplaceold$LossDriver1c)
SMEreviewdData_toreplaceold$DegradationDriver1d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$DegradationDriver1, SMEreviewdData_toreplaceold$DegradationDriver1c)
SMEreviewdData_toreplaceold$LC2018endd <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$LC2018end, SMEreviewdData_toreplaceold$LC2018endc)
SMEreviewdData_toreplaceold$ForestType2018d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$ForestType2018, SMEreviewdData_toreplaceold$ForestType2018c)
SMEreviewdData_toreplaceold$NonForestType2018d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$NonForestType2018, SMEreviewdData_toreplaceold$NonForestType2018c)
SMEreviewdData_toreplaceold$CropType2018d <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$CropType2018, SMEreviewdData_toreplaceold$CropType2018c)
SMEreviewdData_toreplaceold$mixedLCpixeld <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$mixedLCpixel, SMEreviewdData_toreplaceold$mixedLCpixelc)
SMEreviewdData_toreplaceold$Confidenced <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$Confidence, SMEreviewdData_toreplaceold$Confidencec)
SMEreviewdData_toreplaceold$ConfidenceReasoningd <- ifelse(SMEreviewdData_toreplaceold$LC2000c %!in% c('forest','non-forest'), SMEreviewdData_toreplaceold$ConfidenceReasoning, SMEreviewdData_toreplaceold$ConfidenceReasoningc)


#write.csv(SMEreviewdData_toreplaceold, file = 'Results\\AllDataMerged_unalteredforcomparison_CHECK.csv', row.names = F)
write.csv(SMEreviewdData_toreplaceold, file = 'Results\\AllDataMerged.csv', row.names=F)
FINALDATASET <- SMEreviewdData_toreplaceold

FINALDATASET_TEST <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\Results\\AllDataMerged.csv")
dim(FINALDATASET)
dim(SMEreviewdData_toreplaceold)
table(SMEreviewdData_toreplaceold$LC2018endd)
#calling it FINALDATASET now

#FINALDATASET <- SMEreviewdData_toreplaceold #just load static verison
colnames(FINALDATASET)
FINALDATASET <- FINALDATASET[-c(4:32,34:94)]
colnames(FINALDATASET)
dim(FINALDATASET)
#merge the sample pixel counts with the mapped strata of each point
FINALDATASET <- merge(FINALDATASET, strataAreas, by.x = 'pl_change_strata', by.y = 'MapValue', all.x = F)
dim(FINALDATASET)
remove(FINALDATASET)

####################
# ILLOGICAL LABELS WERE DETECTED  (~LINE 250), REVIEWED IN CEO, AND CORRECTED IN AN EXPORTED SPREADSHEET (14 points altered)
# REPLACING FINALDATASET WITH THE FILE WITH THOSE 14 POINTS CORRECTED.
FINALDATASET <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\FINALDATASET_original850CEO_TrippleCheckedLogic_Oct22.csv")
dim(FINALDATASET)
colnames(FINALDATASET)
FINALDATASET <- FINALDATASET[-c(36:37,39:42)] #remove columns with CEO sumarization labels and remake them
colnames(FINALDATASET)


################ Find illogical label combination points for review
# e.g. where a reforestation event was followed by a land cover of perennial crop, which is not forest

#DOUBLE CHECK ALL ILLOGICAL LABELS ARE GONE
FINALDATASET$ReviewIllogicalLabels_V2<-"MakesSense"
table(FINALDATASET$ReviewIllogicalLabels_V2)
FINALDATASET$ReviewIllogicalLabels_V2 <- ifelse(FINALDATASET$NonForestType2018d == "perennial crop" & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$SecondForestChangeEventd == "no", 'Ref event1 ending perennial',
                                                 ifelse(FINALDATASET$NonForestType2018d == "perennial crop" & FINALDATASET$ChangeType2d == "Reforestation" & FINALDATASET$ThirdForestChangeEventd == "no", 'Ref event2 ending perennial', 
                                                        ifelse(FINALDATASET$NonForestType2018d == "perennial crop" & FINALDATASET$ChangeType3d == "Reforestation" & FINALDATASET$morethan3forestchangesd == "no", 'Ref event3 ending perennial', 
                                                               ifelse(FINALDATASET$forestChangeEventd == "no" & FINALDATASET$LC2000d == "forest" & FINALDATASET$LC2018endd == "non-forest", 'LC-forest to LC-nonforest but no change', 
                                                                      ifelse(FINALDATASET$forestChangeEventd == "no" & FINALDATASET$LC2000d == "non-forest" & FINALDATASET$LC2018endd == "forest", 'LC-nonforest to LC-forest but no change',
                                                                             ifelse(FINALDATASET$NonForestType2000d == "perennial crop" & FINALDATASET$NonForestType2018d == "perennial crop" & FINALDATASET$forestChangeEventd == "yes", 'change marked but perennial beginning and end', 
                                                                                    ifelse(FINALDATASET$NonForestType2000d == "perennial crop" & FINALDATASET$ChangeType1d == "Deforestation", 'LC-Perennial expereinceing Def1', FINALDATASET$ReviewIllogicalLabels_V2)))))))

table(FINALDATASET$ReviewIllogicalLabels_V2)


############Set up clean CEO labels - Version 4 NEW
#In version 4: Pixels with multiple events are combined, agroforestry pulled out
FINALDATASET$CEOreadable_v4_NEWd <- ifelse(FINALDATASET$ChangeType1d == "Degradation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation"), 'Degradation',
                                          ifelse(FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation"), 'Deforestation', 
                                                 ifelse(FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation"), 'Reforestation',
                                                        ifelse(FINALDATASET$morethan3forestchangesd == "no", 'multiple events',
                                                               ifelse(FINALDATASET$morethan3forestchangesd == "yes", 'multiple events',
                                                                      ifelse(FINALDATASET$ChangeType2d %in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$ThirdForestChangeEventd == "no", 'multiple events',
                                                                             ifelse(FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest Agroforestry",
                                                                                    ifelse(FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest",
                                                                                           ifelse(FINALDATASET$forestChangeEventd == "no", "stable forest", 'NotReviewed')))))))))
table(FINALDATASET$CEOreadable_v4_NEWd)


#write.csv(SMEreviewdData_toreplaceold, file = 'Results\\TEST_CEOreadable_v4output.csv', row.names = F)

#In version 5: Pixels with multiple events are seperated, agroforestry pulled out, separated by epoch
#getting close to sampling strata, but degradation still present in CEO labels
FINALDATASET$CEOreadable_v5_NEWd_perennialasnonforest <- ifelse(FINALDATASET$ChangeType1d == "Degradation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation"), 'Degradation',
                                           ifelse(FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation Epoch1', 
                                           ifelse(FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                  ifelse(FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Reforestation Epoch1',
                                                         ifelse(FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Reforestation Epoch23',       
                                                         ifelse(FINALDATASET$morethan3forestchangesd == "no", 'multiple events eco possible',
                                                                ifelse(FINALDATASET$morethan3forestchangesd == "yes", 'multiple events noise',
                                                                       ifelse(FINALDATASET$ChangeType2d %in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$ThirdForestChangeEventd == "no", 'multiple events eco possible',
                                                                              ifelse(FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest", #not labeled agro
                                                                                     ifelse(FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest",
                                                                                            ifelse(FINALDATASET$forestChangeEventd == "no", "stable forest", 'NotReviewed')))))))))))
#not needed anymore
#table(FINALDATASET$CEOreadable_v5_NEWd_perennialasnonforest)c("Degradation","Deforestation","Reforestation"), 'Degradation',
#                                                                ifelse(FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation Epoch1', 
 #                                                                      ifelse(FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation Epoch23',
   #                                                                           ifelse(FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Reforestation Epoch1',
  #                                                                                   ifelse(FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Reforestation Epoch23',       
    #                                                                                        ifelse(FINALDATASET$morethan3forestchangesd == "no", 'multiple events eco possible',
    #                                                                                               ifelse(FINALDATASET$morethan3forestchangesd == "yes", 'multiple events noise',
    #                                                                                                      ifelse(FINALDATASET$ChangeType2d %in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$ThirdForestChangeEventd == "no", 'multiple events eco possible',
     #                                                                                                            ifelse(FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable forest", #not labeled agro
 #     
#                                                                                                                  ifelse(FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest",
  #                                                                                                                             ifelse(FINALDATASET$forestChangeEventd == "no", "stable forest", 'NotReviewed')))))))))))
#table(FINALDATASET$CEOreadable_v6_NEWd_perennialasforest)


########Simplified Map data
FINALDATASET$ReadableChangeStrata_Map_v4<-"fixMe"
FINALDATASET$ReadableChangeStrata_Map_v4[FINALDATASET$pl_change_strata == 31]<-"Deforestation"
FINALDATASET$ReadableChangeStrata_Map_v4[FINALDATASET$pl_change_strata == 32]<-"Deforestation"
FINALDATASET$ReadableChangeStrata_Map_v4[FINALDATASET$pl_change_strata == 41]<-"Reforestation"
FINALDATASET$ReadableChangeStrata_Map_v4[FINALDATASET$pl_change_strata == 42]<-"Reforestation"
FINALDATASET$ReadableChangeStrata_Map_v4[FINALDATASET$pl_change_strata == 50]<-"stable forest"
FINALDATASET$ReadableChangeStrata_Map_v4[FINALDATASET$pl_change_strata == 60]<-"stable non forest"
FINALDATASET$ReadableChangeStrata_Map_v4[FINALDATASET$pl_change_strata == 70]<-"multiple events"
FINALDATASET$ReadableChangeStrata_Map_v4[FINALDATASET$pl_change_strata == 80]<-"multiple events"
unique(FINALDATASET$ReadableChangeStrata_Map_v4)
colnames(FINALDATASET)


#add in new degradation labels, for assessing the accuracy, making CEO match map strata
#simple file of all points, but only values for those where the answer was changed from degradation to something else
degradation_relabel <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\Degradation_Relabel.csv")
dim(degradation_relabel)
colnames(degradation_relabel)
#add a column with the new labels for everything previously listed as degradation to FINALDATASET
FINALDATASET <- merge(FINALDATASET, degradation_relabel, by.x = c('PLOTID','lon','lat'), by.y = c('PLOTID','lon','lat'), all.x = F)
dim(FINALDATASET)
colnames(FINALDATASET)

############## I think some Degradation points were not relabeled. Find them #################
FINALDATASET$DegradationMissedLabels<-"fixme"
FINALDATASET$DegradationMissedLabels <- ifelse(FINALDATASET$ChangeType1d == "Degradation" & FINALDATASET$FinalLabel %!in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), "NEEDS LABEL",
                                              ifelse(FINALDATASET$ChangeType2d == "Degradation" & FINALDATASET$FinalLabel %!in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), "NEEDS LABEL",
                                                     ifelse(FINALDATASET$ChangeType3d == "Degradation" & FINALDATASET$FinalLabel %!in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), "NEEDS LABEL",FINALDATASET$DegradationMissedLabels)));

table(FINALDATASET$DegradationMissedLabels)
write.csv(FINALDATASET, file = 'Results\\TEST_ANYMISSEDDEGRADATIONS.csv', row.names = F)


FINALDATASET$CEOreadable_v6_perennialasnonforest_DEGrelabel<-"fixme"
FINALDATASET$CEOreadable_v6_perennialasnonforest_DEGrelabel <- ifelse(FINALDATASET$CEOreadable_v5_NEWd_perennialasnonforest == "Degradation" & FINALDATASET$FinalLabel != "Deforestation", FINALDATASET$FinalLabel,
                                                                      ifelse(FINALDATASET$CEOreadable_v5_NEWd_perennialasnonforest == "Degradation" & FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation Epoch1',
                                                                             ifelse(FINALDATASET$CEOreadable_v5_NEWd_perennialasnonforest == "Degradation" & FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                                             FINALDATASET$CEOreadable_v5_NEWd_perennialasnonforest)))
table(FINALDATASET$CEOreadable_v6_perennialasnonforest_DEGrelabel)

table(degradation_relabel$FinalLabel)



#Find 'final labels' first so they are not missed.

#############3 NOW FIND ILLOGICAL LABELS WHERE NONFOREST 
FINALDATASET$ReviewIllogicalLabels_V3<-"MakesSense"
FINALDATASET$ReviewIllogicalLabels_V3 <- ifelse(FINALDATASET$LC2018endd == "non-forest" & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$SecondForestChangeEventd == "no" & FINALDATASET$FinalLabel %!in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), 'Ref event1 ending nonforest',
                                                ifelse(FINALDATASET$LC2018endd == "non-forest" & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$SecondForestChangeEventd == "no" & FINALDATASET$FinalLabel %in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), 'Ref event1 ending nonforest FIXED',
                                                    ifelse(FINALDATASET$LC2018endd == "non-forest" & FINALDATASET$ChangeType2d == "Reforestation" & FINALDATASET$ThirdForestChangeEventd == "no" & FINALDATASET$FinalLabel %!in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), 'Ref event2 ending nonforest', 
                                                    ifelse(FINALDATASET$LC2018endd == "non-forest" & FINALDATASET$ChangeType2d == "Reforestation" & FINALDATASET$ThirdForestChangeEventd == "no" & FINALDATASET$FinalLabel %in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), 'Ref event2 ending nonforest FIXED', 
                                                        ifelse(FINALDATASET$LC2018endd == "non-forest" & FINALDATASET$ChangeType3d == "Reforestation" & FINALDATASET$morethan3forestchangesd == "no" & FINALDATASET$FinalLabel %!in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), 'Ref event3 ending nonforest',
                                                        ifelse(FINALDATASET$LC2018endd == "non-forest" & FINALDATASET$ChangeType3d == "Reforestation" & FINALDATASET$morethan3forestchangesd == "no" & FINALDATASET$FinalLabel %in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), 'Ref event3 ending nonforest FIXED',
                                                               ifelse(FINALDATASET$LC2000d == "non-forest" & FINALDATASET$LC2018endd == "non-forest" & FINALDATASET$forestChangeEventd == "yes" & FINALDATASET$SecondForestChangeEventd == "no" & FINALDATASET$FinalLabel %!in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), 'one change marked but nonforest beginning and end',
                                                               ifelse(FINALDATASET$LC2000d == "non-forest" & FINALDATASET$LC2018endd == "non-forest" & FINALDATASET$forestChangeEventd == "yes" & FINALDATASET$SecondForestChangeEventd == "no" & FINALDATASET$FinalLabel %in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), 'one change marked but nonforest beginning and end FIXED',
                                                                      ifelse(FINALDATASET$LC2000d == "forest" & FINALDATASET$LC2018endd == "forest" & FINALDATASET$forestChangeEventd == "yes" & FINALDATASET$SecondForestChangeEventd == "no" & FINALDATASET$FinalLabel %!in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), 'one change marked but forest beginning and end', 
                                                                      ifelse(FINALDATASET$LC2000d == "forest" & FINALDATASET$LC2018endd == "forest" & FINALDATASET$forestChangeEventd == "yes" & FINALDATASET$SecondForestChangeEventd == "no" & FINALDATASET$FinalLabel %in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), 'one change marked but forest beginning and end FIXED', 
                                                                          ifelse(FINALDATASET$LC2000d == "non-forest" & FINALDATASET$ChangeType1d == "Deforestation"& FINALDATASET$FinalLabel %!in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), 'nonforest expereinceing Def', 
                                                                          ifelse(FINALDATASET$LC2000d == "non-forest" & FINALDATASET$ChangeType1d == "Deforestation"& FINALDATASET$FinalLabel %in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), 'nonforest expereinceing Def FIXED', 
                                                                                 FINALDATASET$ReviewIllogicalLabels_V3))))))))))))

table(FINALDATASET$ReviewIllogicalLabels_V3)

write.csv(FINALDATASET, file = 'Results\\FINALDATASET_ColumnForReview_PerennialFixed_NonforestIssuesRemain.csv', row.names = F)


########## NEEDED RESULT
########## To use for analysis when epoch distinctions for CEO labels are needed
FINALDATASET$CEOreadable_v7CORRECT_NEWRobStrata_perennialseparate_epoch1and23 <-  ifelse(FINALDATASET$FinalLabel %in% c("stable forest","stable nonforest"),FINALDATASET$FinalLabel,
                                                                                 ifelse(FINALDATASET$FinalLabel == "multiple events eco possible","multiple events",
                                                                                 ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation Epoch1',
                                                                                 ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                                                 ifelse(FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation Epoch1', 
                                                                                 ifelse(FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                                                        ifelse(FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Reforestation Epoch1',
                                                                                               ifelse(FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Reforestation Epoch23',       
                                                                                                      ifelse(FINALDATASET$morethan3forestchangesd == "no", 'multiple events', #this is currently catching remaining Degradation labels and marking them as multiple events
                                                                                                             ifelse(FINALDATASET$morethan3forestchangesd == "yes", 'multiple events',
                                                                                                                    ifelse(FINALDATASET$ChangeType2d %in% c("Deforestation","Reforestation") & FINALDATASET$ThirdForestChangeEventd == "no", 'multiple events',
                                                                                                                           ifelse(FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest", #not labeled agro
                                                                                                                                  ifelse(FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "perennial crop",
                                                                                                                                         ifelse(FINALDATASET$forestChangeEventd == "no", "stable forest",'NotReviewed'))))))))))))))
table(FINALDATASET$CEOreadable_v7CORRECT_NEWRobStrata_perennialseparate_epoch1and23)

FINALDATASET$CEOreadable_v8CORRECT_NEWRobStrata_perennialseparate_epoch1and23 <-  ifelse(FINALDATASET$FinalLabel %in% c("stable forest","stable nonforest"),FINALDATASET$FinalLabel,
                                                                                         ifelse(FINALDATASET$FinalLabel == "multiple events eco possible","multiple events",
                                                                                                ifelse(FINALDATASET$FinalLabel == "Deforestation", "HELP",
                                                                                                ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation Epoch1',
                                                                                                ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                                                                ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation Epoch1',
                                                                                                ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                                                                      ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2000:2005), 'Deforestation Epoch1',
                                                                                                      ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                                                                      ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2000:2005), 'Deforestation Epoch1',
                                                                                                      ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                                                                            ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2000:2005), 'Deforestation Epoch1',
                                                                                                            ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                                                                            ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2000:2005), 'Deforestation Epoch1',
                                                                                                            ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2006:2018), 'Deforestation Epoch23',                                                                                                             
                                                                                                                 ifelse(FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                                                                                            ifelse(FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Reforestation Epoch1',
                                                                                                                                   ifelse(FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Reforestation Epoch23',       
                                                                                                                                          ifelse(FINALDATASET$morethan3forestchangesd == "no", 'multiple events', #this is currently catching remaining Degradation labels and marking them as multiple events
                                                                                                                                                 ifelse(FINALDATASET$morethan3forestchangesd == "yes", 'multiple events',
                                                                                                                                                        ifelse(FINALDATASET$ChangeType2d %in% c("Deforestation","Reforestation") & FINALDATASET$ThirdForestChangeEventd == "no", 'multiple events',
                                                                                                                                                               ifelse(FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest", #not labeled agro
                                                                                                                                                                      ifelse(FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "perennial crop",
                                                                                                                                                                             ifelse(FINALDATASET$forestChangeEventd == "no", "stable forest",'NotReviewed'))))))))))))))))))))))))
table(FINALDATASET$CEOreadable_v8CORRECT_NEWRobStrata_perennialseparate_epoch1and23)

########## NEEDED RESULT
############ To use for analysis when epochs of CEO labels are grouped
FINALDATASET$CEOreadable_v8CORRECT_NEWRobStrata_perennialseparate_epochsgrouped <- ifelse(FINALDATASET$FinalLabel %in% c("stable forest","stable nonforest"),FINALDATASET$FinalLabel,
                                                                                              ifelse(FINALDATASET$FinalLabel == "multiple events eco possible","multiple events",
                                                                                                     ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation',
                                                                                                            ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation',
                                                                                                                   ifelse(FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation', 
                                                                                                                          ifelse(FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation',
                                                                                                                                 ifelse(FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Reforestation',
                                                                                                                                        ifelse(FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Reforestation',       
                                                                                                                                               ifelse(FINALDATASET$morethan3forestchangesd == "no", 'multiple events',
                                                                                                                                                      ifelse(FINALDATASET$morethan3forestchangesd == "yes", 'multiple events',
                                                                                                                                                             ifelse(FINALDATASET$ChangeType2d %in% c("Deforestation","Reforestation") & FINALDATASET$ThirdForestChangeEventd == "no", 'multiple events',
                                                                                                                                                                    ifelse(FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest", #not labeled agro
                                                                                                                                                                           ifelse(FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "perennial crop",
                                                                                                                                                                                  ifelse(FINALDATASET$forestChangeEventd == "no", "stable forest",'NotReviewed'))))))))))))))
table(FINALDATASET$CEOreadable_v8CORRECT_NEWRobStrata_perennialseparate_epochsgrouped)


#THIS IS THE SPREADSHEET WHERE ILLOGICAL LABELS WERE NOTICED AND NEED TO BE REVIEWED AND REIMPORTED
#write.csv(FINALDATASET, file = 'Results\\FINALDATASET_withROBclasses_Sep22_v3.csv', row.names = F)



##################### No Necessary to Run Past Here
#############Use the spreadsheet to make a confusion matrix

#########################################################################################################
#########################################################################################################

#THIS IS THE EASY TO READ ONE
#cross tab of NEW simplified mapv3 and ceo v4 agroforestry removed strata
table(FINALDATASET$ReadableChangeStrata_Map_v4, FINALDATASET$CEOreadable_v4_NEWd)
table_mapv4_ceov4 <- table(FINALDATASET$ReadableChangeStrata_Map_v4, FINALDATASET$CEOreadable_v4_NEWd)
write.csv(table_mapv4_ceov4, file = 'Results\\CrossTable_mapv4_ceov4_simple_NEW.csv', row.names = T)

#THIS IS THE ORIGINAL STRATA VS SIMPLE CEO ONE
#cross tab of NEW simplified mapv3 and ceo v4 agroforestry removed strata
table(FINALDATASET$StrataName, FINALDATASET$CEOreadable_v4_NEWd)
table_strataorig_ceov4 <- table(FINALDATASET$StrataName, FINALDATASET$CEOreadable_v4_NEWd)
write.csv(table_strataorig_ceov4, file = 'Results\\CrossTable_STRATAorig_ceov4_simple_NEW.csv', row.names = T)


#THIS IS THE ONE WHERE STRATA AND CEO MATCH, FOR ACCURACCY CHECKS
#cross tab of NEW strataorig and ceov5, agroforestry pulled out, separated by epoch, degradation relabeled, perennial stable nonforest
table(FINALDATASET$StrataName, FINALDATASET$CEOreadable_v6_perennialasnonforest_DEGrelabel)
table_strataname_StrataCEOMatch <- table(FINALDATASET$StrataName, FINALDATASET$CEOreadable_v6_perennialasnonforest_DEGrelabel)
write.csv(table_strataname_StrataCEOMatch, file = 'Results\\CrossTable_StrataCEOMatch_perennialnonforest.csv', row.names = T)

#THIS IS THE ONE WHERE STRATA AND CEO MATCH, FOR ACCURACCY CHECKS - STABLE PERENNIAL RELABELED FOREST HERE
#cross tab of NEW strataorig and ceov5, agroforestry pulled out, separated by epoch, degradation relabeled, perennial stable nonforest
#table(FINALDATASET$StrataName, FINALDATASET$CEOreadable_v6_perennialasFOREST_DEGrelabel)
#table_strataname_StrataCEOMatch_perennialFOREST <- table(FINALDATASET$StrataName, FINALDATASET$CEOreadable_v6_perennialasFOREST_DEGrelabel)
#write.csv(table_strataname_StrataCEOMatch_perennialFOREST , file = 'Results\\CrossTable_StrataCEOMatch_perennialFOREST.csv', row.names = T)

#THIS IS THE ONE WITH DEGRADATION RELABELED BUT PERENNIAL CROPS SEPARATE
table(FINALDATASET$StrataName, FINALDATASET$CEOreadable_v7_perennialseparate_DEGrelabel)
table_strataname_RelabelDEG_perennialFORESTseparate <- table(FINALDATASET$StrataName, FINALDATASET$CEOreadable_v7_perennialseparate_DEGrelabel)
write.csv(table_strataname_RelabelDEG_perennialFORESTseparate , file = 'Results\\CORRECT_table_RelabelDEG_StrataCEOMatch_perennialFORESTseparate.csv', row.names = T)



#THIS IS THE ONE FOR ROB
#NEW STRATA, EPOCHS SEPARATED INTO 1 AND 2/3, STABLE PERENNIAL CROP SEPARATE, ALL MULTIPLE EVENTS GROUPED, DEGRADATION RELABELED
table(FINALDATASET$StrataName,FINALDATASET$CEOreadable_v7_NEWRobStrata_perennialseparate_epoch1and23)
table_strataname_CEOreadable_v7_NEWRobStrata_perennialseparate_epoch1and23 <- table(FINALDATASET$StrataName,FINALDATASET$CEOreadable_v7_NEWRobStrata_perennialseparate_epoch1and23)
write.csv(table_strataname_CEOreadable_v7_NEWRobStrata_perennialseparate_epoch1and23, file = 'Results\\FINAL_TABLE_ROBclasses_RelabelDEG_perennialseparate_epoch1and23_multichangegrouped.csv',row.names = T)

#THIS IS THE ONE FOR ROB with alternative of grouped change epochs
#NEW STRATA, EPOCHS SEPARATED INTO 1 AND 2/3, STABLE PERENNIAL CROP SEPARATE, ALL MULTIPLE EVENTS GROUPED, DEGRADATION RELABELED
table(FINALDATASET$StrataName,FINALDATASET$CEOreadable_v8_NEWRobStrata_perennialseparate_epochsgroupedalternative)
table_v8_NEWRobStrata_perennialseparate_epochsgroupedalternative <- table(FINALDATASET$StrataName,FINALDATASET$CEOreadable_v8_NEWRobStrata_perennialseparate_epochsgroupedalternative)
write.csv(table_v8_NEWRobStrata_perennialseparate_epochsgroupedalternative, file = 'Results\\FINAL_TABLE_ROBclasses_v8_NEWRobStrata_perennialseparate_epochsgroupedalternative.csv',row.names = T)

write.csv(FINALDATASET, file = 'Results\\FINALDATASET_ALLREVIEWSCOMPLETED_NEWsep22.csv', row.names = T)
####################### NO REVIEW DONE PAST THIS POINT ###########


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
## DO NOT RECOMMEND USING THIS METHOD, SPREADSHEET AE/CI ESTIMATION IS EASIER TO ADAPT
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

