#Unbiased Area Estimation of Philippines Forest Change
#Written by Karis Tenneson and Crystal Wespestad (SIG) Feb, 2022
#In this version of the script all reviews have been applied 
#and 79 new points are added using a modified sample design
#the outputs are cleaned full tables of the data and cross tabulations 
#of the map labels vs the reviewed CEO labels
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
#strataAreas <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\philippines_pixelcounts.csv")
#head(strataAreas)
#colnames(strataAreas)[1]<-"MapValue"
#colnames(strataAreas)[2]<-"StrataName"
#colnames(strataAreas)[3]<-"PixelCount"
#strataAreas <- strataAreas[-c(4:5)]


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
#FINALDATASET <- merge(FINALDATASET, strataAreas, by.x = 'pl_change_strata', by.y = 'MapValue', all.x = F)
dim(FINALDATASET)
remove(FINALDATASET)


####################
# ILLOGICAL LABELS WERE DETECTED  (~LINE 250), REVIEWED IN CEO, AND CORRECTED IN AN EXPORTED SPREADSHEET (19 points altered)
# REPLACING FINALDATASET WITH THE FILE WITH THOSE 17 POINTS CORRECTED. And two more Degradation relabels added which were missed
#double checked by hand that finallabel notes matched finallabel, (added 3? more finallabels)
##FINALDATASET <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\FINALDATASET_original850CEO_TrippleCheckedLogic_Oct22.csv")
##FINALDATASET <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\FINALDATASET_AllReviewsCompleted_850points_USETHISONE.csv")
FINALDATASET <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\TEST_FIX_FINALLABEL_DEFORESTATION_yearofchange_TGI.csv")
#PLOTID 840 fixed to have N/A nonforest entire time instead, and blank in ChangeType1d instead of NA
dim(FINALDATASET)
colnames(FINALDATASET)
FINALDATASET <- FINALDATASET[-c(39:42)] #remove columns with CEO sumarization labels and remake them
colnames(FINALDATASET) 
colnames(FINALDATASET)[1]<-"PLOTIDd"
write.csv(FINALDATASET, file = 'Results\\Clean_Old850_ForGettingNewMapValueColumns.csv', row.names = F)


#Now upload the cleaned 850 points after adding columns with labels for all the new maps, using GEE script with reduce regions
#Old850_clean_withnewmaplabels 
#Old850_clean_withnewmaplabels  <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\PhilippinesPoints_old850_allmaplabels.csv")
#TEST
Old850_clean_withnewmaplabels  <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\PhilippinesPoints_old850_allmaplabels_plotid_4326proj.csv")

colnames(Old850_clean_withnewmaplabels)
colnames(Old850_clean_withnewmaplabels)
table(Old850_clean_withnewmaplabels$Modified_stratamap)
#Old850_clean_withnewmaplabels <- Old850_clean_withnewmaplabels[-c(1:19,23:24,26:28,30:40,42)]
head(Old850_clean_withnewmaplabels)
#as long as the column names match between this and the new 79 points, the order of the columns should not matter.
head(sort(Old850_clean_withnewmaplabels$LAT))
head(sort(FINALDATASET$lat))
#slight difference in lon lat (~0.3 feet), so will use PLOTID to join
#FINALDATASET_mapdataadded <- merge(FINALDATASET, Old850_clean_withnewmaplabels, by.x = c('lon','lat'), by.y = c('LON','LAT'), all.x = T)
FINALDATASET_mapdataadded <- merge(FINALDATASET, Old850_clean_withnewmaplabels, by.x = 'PLOTIDd', by.y = 'PLOTIDd', all.x = T)
head(FINALDATASET_mapdataadded)
colnames(FINALDATASET_mapdataadded)
FINALDATASET_mapdataadded <- FINALDATASET_mapdataadded[-c(4,34:36,39:40)]
######################################################################################################################
#Adding in new 79 points collected using updated strata design, 35 minimum in each strata, which is 79 new points

new79points <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\ceo-Philippines_AddedPoints_35pointMinimum-sample-data-2022-10-11.csv")
dim(new79points)
colnames(new79points)
new79points <- new79points[-c(2, 6:12)]
#point 1047 manually changed to deforestation from degradation


colnames(new79points)[1]<-"PLOTIDd"
colnames(new79points)[2]<-"lon"
colnames(new79points)[3]<-"lat"
colnames(new79points)[4]<-"EMAILd"
colnames(new79points)[5]<-"old_strata"
colnames(new79points)[6]<-"Intended_1B_no2000"
colnames(new79points)[7]<-"Modified_stratamap"
colnames(new79points)[8]<-"Intended_1A_yes2000"
colnames(new79points)[14]<-"Change1yeard"
colnames(new79points)[19]<-"Change2yeard"
colnames(new79points)[26]<-"Change3yeard"
colnames(new79points)[34]<-"Confidenced"
colnames(new79points)[35]<-"ConfidenceReasoningd"
colnames(new79points)[9]<-"LC2000d"
colnames(new79points)[10]<-"ForestType2000d"
colnames(new79points)[11]<-"NonForestType2000d" 
colnames(new79points)[12]<-"CropType2000d" 
colnames(new79points)[30]<-"LC2018endd"
colnames(new79points)[13]<-"forestChangeEventd" 
colnames(new79points)[27]<-"ChangeType1d"
colnames(new79points)[29]<-"DegradationDriver1d"
colnames(new79points)[28]<-"LossDriver1d"
colnames(new79points)[15]<-"SecondForestChangeEventd"
colnames(new79points)[16]<-"ChangeType2d"
colnames(new79points)[17]<-"DegradationDriver2d"
colnames(new79points)[18]<-"LossDriver2d"
colnames(new79points)[20]<-"ThirdForestChangeEventd" 
colnames(new79points)[21]<-"ChangeType3d"
colnames(new79points)[23]<-"DegradationDriver3d"
colnames(new79points)[22]<-"LossDriver3d"
colnames(new79points)[24]<-"morethan3forestchangesd" 
colnames(new79points)[25]<-"DescribeExtraChangesd" 
colnames(new79points)[31]<-"ForestType2018d" 
colnames(new79points)[32]<-"NonForestType2018d" 
colnames(new79points)[33]<-"CropType2018d" 
new79points$mixedLCpixeld <- "Not Measured"
new79points$FinalLabelNotes <- "none"
new79points$FinalLabel <- "none"
new79points$morethan3forestchangesd <- ""
#colnames(new79points)[30]<-"mixedLCpixeld"
dim(new79points)
dim(FINALDATASET_mapdataadded)
colnames(new79points)
colnames(FINALDATASET_mapdataadded)


###############FIND IF THERE ARE ANY VALUES IN 79NEW THAT SHOULD NOT BE NA and don't match original 850
sum(table(FINALDATASET_mapdataadded$LC2000d))
sum(table(new79points$LC2000d))
sum(table(FINALDATASET_mapdataadded$ForestType2000))
sum(table(new79points$ForestType2000))
sum(table(FINALDATASET_mapdataadded$forestChangeEventd))
sum(table(new79points$forestChangeEventd))
sum(table(FINALDATASET_mapdataadded$Change1yeard)) #112
sum(table(new79points$Change1yeard)) #21
sum(table(FINALDATASET_mapdataadded$ChangeType2d))
sum(table(new79points$ChangeType2d))
sum(table(FINALDATASET_mapdataadded$morethan3forestchangesd))
sum(table(new79points$morethan3forestchangesd))
sum(table(FINALDATASET_mapdataadded$ChangeType1d))
sum(table(new79points$ChangeType1d))
sum(table(FINALDATASET_mapdataadded$NonForestType2018d))
sum(table(new79points$NonForestType2018d))
sum(table(FINALDATASET_mapdataadded$morethan3forestchangesd))
sum(table(new79points$morethan3forestchangesd))
sum(table(FINALDATASET_mapdataadded$morethan3forestchangesd))
sum(table(new79points$morethan3forestchangesd))

###### Combine the old 850 and new 79
FINALDATASET <- rbind(FINALDATASET_mapdataadded, new79points)
table(FINALDATASET$ChangeType1d)
sum(table(FINALDATASET$ChangeType1d))
#should total 929 = 850+79

################ Find illogical label combination points for review
# e.g. where a reforestation event was followed by a land cover of perennial crop, which is not forest

#### NO ILLOGICAL LABELS SHOULD BE LEFT
#DOUBLE CHECK ALL ILLOGICAL LABELS ARE GONE
FINALDATASET$ReviewIllogicalLabels_V2<-"MakesSense"
table(FINALDATASET$ReviewIllogicalLabels_V2)
FINALDATASET$ReviewIllogicalLabels_V2 <- ifelse(FINALDATASET$NonForestType2018d == "perennial crop" & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$SecondForestChangeEventd == "no", 'Ref event1 ending perennial',
                                                 ifelse(FINALDATASET$NonForestType2018d == "perennial crop" & FINALDATASET$ChangeType2d == "Reforestation" & FINALDATASET$ThirdForestChangeEventd == "no", 'Ref event2 ending perennial', 
                                                        ifelse(FINALDATASET$NonForestType2018d == "perennial crop" & FINALDATASET$ChangeType3d == "Reforestation" & FINALDATASET$morethan3forestchangesd == "no", 'Ref event3 ending perennial', 
                                                               ifelse(FINALDATASET$forestChangeEventd == "no" & FINALDATASET$LC2000d == "forest" & FINALDATASET$LC2018endd == "non-forest", 'LC-forest to LC-nonforest but no change', 
                                                                      ifelse(FINALDATASET$forestChangeEventd == "no" & FINALDATASET$LC2000d == "non-forest" & FINALDATASET$LC2018endd == "forest", 'LC-nonforest to LC-forest but no change',
                                                                             ifelse(FINALDATASET$NonForestType2000d == "perennial crop" & FINALDATASET$NonForestType2018d == "perennial crop" & FINALDATASET$forestChangeEventd == "yes", 'change marked but perennial beginning and end', 
                                                                                    ifelse(FINALDATASET$NonForestType2000d == "perennial crop" & FINALDATASET$ChangeType1d == "Deforestation", 'LC-Perennial expereinceing Def1', "makes sense")))))))

table(FINALDATASET$ReviewIllogicalLabels_V2)
sum(table(FINALDATASET$ReviewIllogicalLabels_V2))
############# ???? Why did we lose 34 points 
### perennial crop in new79? for NoforestType2018d

########Simplified Map dataold
FINALDATASET$ReadableChangeStrata_Map_old_simp<-"fixMe"
FINALDATASET$ReadableChangeStrata_Map_old_simp[FINALDATASET$old_strata == 31]<-"Deforestation"
FINALDATASET$ReadableChangeStrata_Map_old_simp[FINALDATASET$old_strata == 32]<-"Deforestation"
FINALDATASET$ReadableChangeStrata_Map_old_simp[FINALDATASET$old_strata == 41]<-"Reforestation"
FINALDATASET$ReadableChangeStrata_Map_old_simp[FINALDATASET$old_strata == 42]<-"Reforestation"
FINALDATASET$ReadableChangeStrata_Map_old_simp[FINALDATASET$old_strata == 50]<-"stable forest"
FINALDATASET$ReadableChangeStrata_Map_old_simp[FINALDATASET$old_strata == 60]<-"stable non forest"
FINALDATASET$ReadableChangeStrata_Map_old_simp[FINALDATASET$old_strata == 70]<-"multiple events"
FINALDATASET$ReadableChangeStrata_Map_old_simp[FINALDATASET$old_strata == 80]<-"multiple events"
unique(FINALDATASET$ReadableChangeStrata_Map_old_simp)
colnames(FINALDATASET)

########Detailed Map dataold
FINALDATASET$ReadableChangeStrata_Map_old<-"fixMe"
FINALDATASET$ReadableChangeStrata_Map_old[FINALDATASET$old_strata == 31]<-"Deforestation E1"
FINALDATASET$ReadableChangeStrata_Map_old[FINALDATASET$old_strata == 32]<-"Deforestation E23"
FINALDATASET$ReadableChangeStrata_Map_old[FINALDATASET$old_strata == 41]<-"Reforestation E1"
FINALDATASET$ReadableChangeStrata_Map_old[FINALDATASET$old_strata == 42]<-"Reforestation E23"
FINALDATASET$ReadableChangeStrata_Map_old[FINALDATASET$old_strata == 50]<-"stable forest"
FINALDATASET$ReadableChangeStrata_Map_old[FINALDATASET$old_strata == 60]<-"stable non forest"
FINALDATASET$ReadableChangeStrata_Map_old[FINALDATASET$old_strata == 70]<-"3+ multiple events"
FINALDATASET$ReadableChangeStrata_Map_old[FINALDATASET$old_strata == 80]<-"2/3 multiple events"
unique(FINALDATASET$ReadableChangeStrata_Map_old)
colnames(FINALDATASET)


########Detailed Map modified strata
FINALDATASET$ReadableChangeStrata_Map_Modified<-"fixMe"
FINALDATASET$ReadableChangeStrata_Map_Modified[FINALDATASET$Modified_stratamap == 11]<-"Deforested E1 (agreement between maps)"
FINALDATASET$ReadableChangeStrata_Map_Modified[FINALDATASET$Modified_stratamap == 17]<-"Multiple events (relabeled from Def E1)"
FINALDATASET$ReadableChangeStrata_Map_Modified[FINALDATASET$Modified_stratamap == 22]<-"Deforested E2/3 (agreement between maps)"
FINALDATASET$ReadableChangeStrata_Map_Modified[FINALDATASET$Modified_stratamap == 27]<-"Multiple events (relabeled from Def E2/3)"
FINALDATASET$ReadableChangeStrata_Map_Modified[FINALDATASET$Modified_stratamap == 33]<-"Reforested E1 (agreement between maps)"
FINALDATASET$ReadableChangeStrata_Map_Modified[FINALDATASET$Modified_stratamap == 37]<-"Multiple events (relabeled from Eef E1)"
FINALDATASET$ReadableChangeStrata_Map_Modified[FINALDATASET$Modified_stratamap == 44]<-"Reforested E2/3 (agreement between maps)"
FINALDATASET$ReadableChangeStrata_Map_Modified[FINALDATASET$Modified_stratamap == 47]<-"Multiple events (relabeled from Ref E2/3)"
FINALDATASET$ReadableChangeStrata_Map_Modified[FINALDATASET$Modified_stratamap == 55]<-"stable forest"
FINALDATASET$ReadableChangeStrata_Map_Modified[FINALDATASET$Modified_stratamap == 66]<-"stable nonforest"
FINALDATASET$ReadableChangeStrata_Map_Modified[FINALDATASET$Modified_stratamap == 77]<-"3+ multiple events"
FINALDATASET$ReadableChangeStrata_Map_Modified[FINALDATASET$Modified_stratamap == 88]<-"2/3 multiple events"
unique(FINALDATASET$ReadableChangeStrata_Map_Modified)
colnames(FINALDATASET)

########Simplified Map modified strata, not for anlysis
FINALDATASET$ReadableChangeStrata_Map_Modified_simp<-"fixMe"
FINALDATASET$ReadableChangeStrata_Map_Modified_simp[FINALDATASET$Modified_stratamap == 11]<-"Deforested"
FINALDATASET$ReadableChangeStrata_Map_Modified_simp[FINALDATASET$Modified_stratamap == 17]<-"Multiple events"
FINALDATASET$ReadableChangeStrata_Map_Modified_simp[FINALDATASET$Modified_stratamap == 22]<-"Deforested"
FINALDATASET$ReadableChangeStrata_Map_Modified_simp[FINALDATASET$Modified_stratamap == 27]<-"Multiple events"
FINALDATASET$ReadableChangeStrata_Map_Modified_simp[FINALDATASET$Modified_stratamap == 33]<-"Reforested"
FINALDATASET$ReadableChangeStrata_Map_Modified_simp[FINALDATASET$Modified_stratamap == 37]<-"Multiple events"
FINALDATASET$ReadableChangeStrata_Map_Modified_simp[FINALDATASET$Modified_stratamap == 44]<-"Reforested"
FINALDATASET$ReadableChangeStrata_Map_Modified_simp[FINALDATASET$Modified_stratamap == 47]<-"Multiple events"
FINALDATASET$ReadableChangeStrata_Map_Modified_simp[FINALDATASET$Modified_stratamap == 55]<-"stable forest"
FINALDATASET$ReadableChangeStrata_Map_Modified_simp[FINALDATASET$Modified_stratamap == 66]<-"stable nonforest"
FINALDATASET$ReadableChangeStrata_Map_Modified_simp[FINALDATASET$Modified_stratamap == 77]<-"Multiple events"
FINALDATASET$ReadableChangeStrata_Map_Modified_simp[FINALDATASET$Modified_stratamap == 88]<-"Multiple events"
unique(FINALDATASET$ReadableChangeStrata_Map_Modified_simp)
colnames(FINALDATASET)



#add in new degradation labels, for assessing the accuracy, making CEO match map strata
#simple file of all points, but only values for those where the answer was changed from degradation to something else
############# COMMENTED OUT BECAUSE ALREADY ADDED AND REVIEWED IN FINALDATASET_AllReviewsCompleted_850points_USETHISONE.csv
#degradation_relabel <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\Degradation_Relabel.csv")
#dim(degradation_relabel)
#colnames(degradation_relabel)
#add a column with the new labels for everything previously listed as degradation to FINALDATASET
#FINALDATASET <- merge(FINALDATASET, degradation_relabel, by.x = c('PLOTID','lon','lat'), by.y = c('PLOTID','lon','lat'), all.x = F)
#dim(FINALDATASET)
#colnames(FINALDATASET)

############## I think some Degradation points were not relabeled. Find them #################
#########COMMENTED OUT BECAUSE ALL FOUND AND UPDATED#################
#FINALDATASET$DegradationMissedLabels<-"fixme"
#FINALDATASET$DegradationMissedLabels <- ifelse(FINALDATASET$ChangeType1d == "Degradation" & FINALDATASET$FinalLabel %!in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), "NEEDS LABEL",
#                                              ifelse(FINALDATASET$ChangeType2d == "Degradation" & FINALDATASET$FinalLabel %!in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), "NEEDS LABEL",
#                                                     ifelse(FINALDATASET$ChangeType3d == "Degradation" & FINALDATASET$FinalLabel %!in%  c("Deforestation","Reforestation","multiple events eco possible","stable forest","stable nonforest"), "NEEDS LABEL",FINALDATASET$DegradationMissedLabels)));

#table(FINALDATASET$DegradationMissedLabels)
#write.csv(FINALDATASET, file = 'Results\\TEST_ANYMISSEDDEGRADATIONS.csv', row.names = F)


#FINALDATASET$CEOreadable_v6_perennialasnonforest_DEGrelabel<-"fixme"
#FINALDATASET$CEOreadable_v6_perennialasnonforest_DEGrelabel <- ifelse(FINALDATASET$CEOreadable_v5_NEWd_perennialasnonforest == "Degradation" & FINALDATASET$FinalLabel != "Deforestation", FINALDATASET$FinalLabel,
#                                                                      ifelse(FINALDATASET$CEOreadable_v5_NEWd_perennialasnonforest == "Degradation" & FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation Epoch1',
#                                                                             ifelse(FINALDATASET$CEOreadable_v5_NEWd_perennialasnonforest == "Degradation" & FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation Epoch23',
#                                                                             FINALDATASET$CEOreadable_v5_NEWd_perennialasnonforest)))
#table(FINALDATASET$CEOreadable_v6_perennialasnonforest_DEGrelabel)
#table(degradation_relabel$FinalLabel)



#Find 'final labels' first so they are not missed.

############# NOW FIND ILLOGICAL LABELS WHERE NONFOREST 
# ones that end in FIXED have alread been dealt with
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
sum(table(FINALDATASET$ReviewIllogicalLabels_V3))

#write.csv(FINALDATASET, file = 'Results\\FINALDATASET_ColumnForReview_PerennialFixed_NonforestIssuesRemain.csv', row.names = F)



#Fixing so that it does not assume Deforestation was the first event if FinalLabel is Deforestation
#FINALDATASET$CEOreadable_v8testing_NEWstrata_perennialseparate_epoch1and23 <-  ifelse(FINALDATASET$FinalLabel %in% c("stable forest","stable nonforest"),FINALDATASET$FinalLabel,
#                                                                                         ifelse(FINALDATASET$FinalLabel == "multiple events eco possible","multiple events - FIXED",
#                                                                                                ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation Epoch1 - FIXED',
#                                                                                                ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation Epoch23 - FIXED',
#                                                                                                ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation Epoch1 - FIXED',
#                                                                                                ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation Epoch23 - FIXED',
#                                                                                                      ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2000:2005), 'Deforestation Epoch1 - FIXED',
#                                                                                                      ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2006:2018), 'Deforestation Epoch23 - FIXED',
#                                                                                                      ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2000:2005), 'Deforestation Epoch1 - FIXED',
#                                                                                                      ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2006:2018), 'Deforestation Epoch23 - FIXED',
#                                                                                                            ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2000:2005), 'Deforestation Epoch1 - FIXED',
#                                                                                                            ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2006:2018), 'Deforestation Epoch23 - FIXED',
#                                                                                                            ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2000:2005), 'Deforestation Epoch1 - FIXED',
#                                                                                                            ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2006:2018), 'Deforestation Epoch23 - FIXED',   
#                                                                                                                   ifelse(FINALDATASET$FinalLabel == "Deforestation", "NEEDS DEFORESTATION YEAR",
#                                                                                                                 ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation Epoch1',
#                                                                                                                 ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation Epoch23',
#                                                                                                                            ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Reforestation Epoch1',
#                                                                                                                                   ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Reforestation Epoch23',       
#                                                                                                                                          ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$morethan3forestchangesd == "no", 'multiple events', #this is currently catching remaining Degradation labels and marking them as multiple events
#                                                                                                                                                 ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible")& FINALDATASET$morethan3forestchangesd == "yes", 'multiple events',
#                                                                                                                                                        ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$ChangeType2d %in% c("Deforestation","Reforestation") & FINALDATASET$ThirdForestChangeEventd == "no", 'multiple events',
#                                                                                                                                                               ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest - OLD", #not labeled agro
#                                                                                                                                                                      ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "perennial crop",
#                                                                                                                                                                             ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$forestChangeEventd == "no", "stable forest - OLD",'NotReviewed')))))))))))))))))))))))))
#table(FINALDATASET$CEOreadable_v8testing_NEWstrata_perennialseparate_epoch1and23)
#sum(table(FINALDATASET$CEOreadable_v8testing_NEWstrata_perennialseparate_epoch1and23))

########## NEEDED RESULT
#final labels for NEW STRATA without testing details
FINALDATASET$CEOreadable_850and79_v8CORRECT_NEWstrata_perennialseparate_epoch1and23 <-  ifelse(FINALDATASET$FinalLabel %in% c("stable forest","stable nonforest"),FINALDATASET$FinalLabel,
                                                                                      ifelse(FINALDATASET$FinalLabel == "multiple events eco possible","multiple events",
                                                                                             ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation Epoch1',
                                                                                                    ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                                                                           ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation Epoch1',
                                                                                                                  ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                                                                                         ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2000:2005), 'Deforestation Epoch1',
                                                                                                                                ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                                                                                                       ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2000:2005), 'Deforestation Epoch1',
                                                                                                                                              ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                                                                                                                     ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change3yeard %in% c(2000:2005), 'Deforestation Epoch1',
                                                                                                                                                            ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change3yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                                                                                                                                   ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change3yeard %in% c(2000:2005), 'Deforestation Epoch1',
                                                                                                                                                                          ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change3yeard %in% c(2006:2018), 'Deforestation Epoch23',   
                                                                                                                                                                                 ifelse(FINALDATASET$FinalLabel == "Deforestation", "NEEDS DEFORESTATION YEAR",
                                                                                                                                                                                        ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation Epoch1',
                                                                                                                                                                                               ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation Epoch23',
                                                                                                                                                                                                      ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Reforestation Epoch1',
                                                                                                                                                                                                             ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Reforestation Epoch23',       
                                                                                                                                                                                                                    ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$morethan3forestchangesd == "no", 'multiple events', #this is currently catching remaining Degradation labels and marking them as multiple events
                                                                                                                                                                                                                           ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible")& FINALDATASET$morethan3forestchangesd == "yes", 'multiple events',
                                                                                                                                                                                                                                  ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$ChangeType2d %in% c("Deforestation","Reforestation") & FINALDATASET$ThirdForestChangeEventd == "no", 'multiple events',
                                                                                                                                                                                                                                         ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest", #not labeled agro
                                                                                                                                                                                                                                                ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "perennial crop",
                                                                                                                                                                                                                                                       ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible") & FINALDATASET$forestChangeEventd == "no", "stable forest",'NotReviewed')))))))))))))))))))))))))
table(FINALDATASET$CEOreadable_850and79_v8CORRECT_NEWstrata_perennialseparate_epoch1and23)
sum(table(FINALDATASET$CEOreadable_850and79_v8CORRECT_NEWstrata_perennialseparate_epoch1and23))

####### NEEDED RESULT
#final NEWSTRATA with epochs grouped
#final labels for NEW STRATA without testing details, split by new and old
#FINALDATASET$CEOreadable_v8testing_NEWstrata_perennialseparate_epochsgrouped <-  ifelse(FINALDATASET$FinalLabel %in% c("stable forest","stable nonforest"),FINALDATASET$FinalLabel,
#                                                                                      ifelse(FINALDATASET$FinalLabel == "multiple events eco possible","multiple events",
#                                                                                             ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation',
#                                                                                                    ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation',
#                                                                                                           ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation',
#                                                                                                                  ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation',
#                                                                                                                         ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2000:2005), 'Deforestation',
#                                                                                                                                ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2006:2018), 'Deforestation',
#                                                                                                                                       ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2000:2005), 'Deforestation',
#                                                                                                                                              ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2006:2018), 'Deforestation',
#                                                                                                                                                     ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change3yeard %in% c(2000:2005), 'Deforestation',
#                                                                                                                                                            ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change3yeard %in% c(2006:2018), 'Deforestation',
#                                                                                                                                                                   ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change3yeard %in% c(2000:2005), 'Deforestation',
#                                                                                                                                                                          ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change3yeard %in% c(2006:2018), 'Deforestation',   
#                                                                                                                                                                                 ifelse(FINALDATASET$FinalLabel == "Deforestation", "NEEDS DEFORESTATION YEAR", 
#                                                                                                                                                                                        
#                                                                                                                                                                                        
#                                                                                                                                                                                        ifelse(FINALDATASET$FinalLabel == "none" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation - New',
#                                                                                                                                                                                               ifelse(FINALDATASET$FinalLabel == "none" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation - New',
#                                                                                                                                                                                                      ifelse(FINALDATASET$FinalLabel== "none" & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Reforestation - New',
#                                                                                                                                                                                                             ifelse(FINALDATASET$FinalLabel == "none" & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Reforestation - New',       
#                                                                                                                                                                                                                    ifelse(FINALDATASET$FinalLabel== "none" & FINALDATASET$morethan3forestchangesd == "no", 'multiple events - New', #this is currently catching remaining Degradation labels and marking them as multiple events
#                                                                                                                                                                                                                           ifelse(FINALDATASET$FinalLabel == "none" & FINALDATASET$morethan3forestchangesd == "yes", 'multiple events - New',
#                                                                                                                                                                                                                                  ifelse(FINALDATASET$FinalLabel == "none" & FINALDATASET$ChangeType2d %in% c("Deforestation","Reforestation") & FINALDATASET$ThirdForestChangeEventd == "no", 'multiple events - New',
#                                                                                                                                                                                                                                         ifelse(FINALDATASET$FinalLabel== "none" & FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest - New", #not labeled agro
#                                                                                                                                                                                                                                                ifelse(FINALDATASET$FinalLabel== "none" & FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "perennial crop - New",
#                                                                                                                                                                                                                                                       ifelse(FINALDATASET$FinalLabel == "none" & FINALDATASET$forestChangeEventd == "no",  "stable forest - New",                                                      
#                                                                                                                                                                                        
#                                                                                                                                                                                        
#                                                                                                                                                                                        ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation',
#                                                                                                                                                                                               ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation',
#                                                                                                                                                                                                      ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Reforestation',
#                                                                                                                                                                                                             ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Reforestation',       
#                                                                                                                                                                                                                    ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$morethan3forestchangesd == "no", 'multiple events', #this is currently catching remaining Degradation labels and marking them as multiple events
#                                                                                                                                                                                                                           ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none")& FINALDATASET$morethan3forestchangesd == "yes", 'multiple events',
#                                                                                                                                                                                                                                  ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$ChangeType2d %in% c("Deforestation","Reforestation") & FINALDATASET$ThirdForestChangeEventd == "no", 'multiple events',
#                                                                                                                                                                                                                                         ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest", #not labeled agro
#                                                                                                                                                                                                                                                ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "perennial crop",
#                                                                                                                                                                                                                                                       ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$forestChangeEventd == "no", "stable forest",'NotReviewed')))))))))))))))))))))))))))))))))))
#table(FINALDATASET$CEOreadable_v8testing_NEWstrata_perennialseparate_epochsgrouped)
#sum(table(FINALDATASET$CEOreadable_v8testing_NEWstrata_perennialseparate_epochsgrouped))


#final labels for NEW STRATA without testing details --- FINAL RESULTS
FINALDATASET$CEOreadable_850and79_FINAL_NEWstrata_perennialseparate_epochsgrouped <-  ifelse(FINALDATASET$FinalLabel %in% c("stable forest","stable nonforest"),FINALDATASET$FinalLabel,
                                                                                        ifelse(FINALDATASET$FinalLabel == "multiple events eco possible","multiple events",
                                                                                               ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation',
                                                                                                      ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation',
                                                                                                             ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation',
                                                                                                                    ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation',
                                                                                                                           ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2000:2005), 'Deforestation',
                                                                                                                                  ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2006:2018), 'Deforestation',
                                                                                                                                         ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2000:2005), 'Deforestation',
                                                                                                                                                ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType2d == "Deforestation" & FINALDATASET$Change2yeard %in% c(2006:2018), 'Deforestation',
                                                                                                                                                       ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change3yeard %in% c(2000:2005), 'Deforestation',
                                                                                                                                                              ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change3yeard %in% c(2006:2018), 'Deforestation',
                                                                                                                                                                     ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change3yeard %in% c(2000:2005), 'Deforestation',
                                                                                                                                                                            ifelse(FINALDATASET$FinalLabel == "Deforestation" & FINALDATASET$ChangeType1d != "Deforestation" & FINALDATASET$ChangeType3d == "Deforestation" & FINALDATASET$Change3yeard %in% c(2006:2018), 'Deforestation',   
                                                                                                                                                                                   ifelse(FINALDATASET$FinalLabel == "Deforestation", "NEEDS DEFORESTATION YEAR", 
                                                                                                                                                                                          
                                                                                                                                                                                          
                                                                                                                                                                                          ifelse(FINALDATASET$FinalLabel == "none" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation',
                                                                                                                                                                                                 ifelse(FINALDATASET$FinalLabel == "none" & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation',
                                                                                                                                                                                                        ifelse(FINALDATASET$FinalLabel== "none" & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Reforestation',
                                                                                                                                                                                                               ifelse(FINALDATASET$FinalLabel == "none" & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Reforestation',       
                                                                                                                                                                                                                      ifelse(FINALDATASET$FinalLabel== "none" & FINALDATASET$morethan3forestchangesd == "no", 'multiple events', #this is currently catching remaining Degradation labels and marking them as multiple events
                                                                                                                                                                                                                             ifelse(FINALDATASET$FinalLabel == "none" & FINALDATASET$morethan3forestchangesd == "yes", 'multiple events',
                                                                                                                                                                                                                                    ifelse(FINALDATASET$FinalLabel == "none" & FINALDATASET$ChangeType2d %in% c("Deforestation","Reforestation") & FINALDATASET$ThirdForestChangeEventd == "no", 'multiple events',
                                                                                                                                                                                                                                           ifelse(FINALDATASET$FinalLabel== "none" & FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest", #not labeled agro
                                                                                                                                                                                                                                                  ifelse(FINALDATASET$FinalLabel== "none" & FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "perennial crop",
                                                                                                                                                                                                                                                         ifelse(FINALDATASET$FinalLabel == "none" & FINALDATASET$forestChangeEventd == "no",  "stable forest",                                                      
                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                
                                                                                                                                                                                                                                                                ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Deforestation',
                                                                                                                                                                                                                                                                       ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$ChangeType1d == "Deforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Deforestation',
                                                                                                                                                                                                                                                                              ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2000:2005), 'Reforestation',
                                                                                                                                                                                                                                                                                     ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$ChangeType1d == "Reforestation" & FINALDATASET$ChangeType2d %!in% c("Degradation","Deforestation","Reforestation") & FINALDATASET$Change1yeard %in% c(2006:2018), 'Reforestation',       
                                                                                                                                                                                                                                                                                            ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$morethan3forestchangesd == "no", 'multiple events', #this is currently catching remaining Degradation labels and marking them as multiple events
                                                                                                                                                                                                                                                                                                   ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none")& FINALDATASET$morethan3forestchangesd == "yes", 'multiple events',
                                                                                                                                                                                                                                                                                                          ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$ChangeType2d %in% c("Deforestation","Reforestation") & FINALDATASET$ThirdForestChangeEventd == "no", 'multiple events',
                                                                                                                                                                                                                                                                                                                 ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %!in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "stable nonforest", #not labeled agro
                                                                                                                                                                                                                                                                                                                        ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$forestChangeEventd == "N/A non-forest entire time" & FINALDATASET$CropType2000d %in% c("coconut / other palm","unsure","fruit trees (mango, cashew, avocado, rambutan durian)","banana"), "perennial crop",
                                                                                                                                                                                                                                                                                                                               ifelse(FINALDATASET$FinalLabel %!in% c("stable forest","stable nonforest","Deforestation","multiple events eco possible","none") & FINALDATASET$forestChangeEventd == "no", "stable forest",'NotReviewed')))))))))))))))))))))))))))))))))))
table(FINALDATASET$CEOreadable_850and79_FINAL_NEWstrata_perennialseparate_epochsgrouped)
sum(table(FINALDATASET$CEOreadable_850and79_FINAL_NEWstrata_perennialseparate_epochsgrouped))

table(FINALDATASET_mapdataadded$ChangeType1d)
table(new79points$ChangeType1d)
table(new79points$ChangeType2d)

#write.csv(FINALDATASET, file = 'Results\\FIGUREOUT_WHYVALUESBLANK.csv', row.names = F)


#output all results of original data, everything cleaned up
write.csv(FINALDATASET, file = 'Results\\COMPLETEDDATA_850and79_FINAL.csv', row.names = F)
table(FINALDATASET$ReadableChangeStrata_Map_old,FINALDATASET$CEOreadable_850and79_FINAL_NEWstrata_perennialseparate_epochsgrouped)
crosstab_originaldata_allclean <- table(FINALDATASET$ReadableChangeStrata_Map_old,FINALDATASET$CEOreadable_850and79_FINAL_NEWstrata_perennialseparate_epochsgrouped)
write.csv(crosstab_originaldata_allclean, file = 'Results\\OriginalData_CleanCrossTab_FINAL_79added.csv', row.names = T)

#JUST FOR EASY VIEWING, NOT ANALYSIS, SIMPLIFIED STRATA
table(FINALDATASET$ReadableChangeStrata_Map_old_simp,FINALDATASET$CEOreadable_850and79_FINAL_NEWstrata_perennialseparate_epochsgrouped)
crosstab_originaldata_allclean_simp <- table(FINALDATASET$ReadableChangeStrata_Map_old_simp,FINALDATASET$CEOreadable_850and79_FINAL_NEWstrata_perennialseparate_epochsgrouped)
write.csv(crosstab_originaldata_allclean_simp, file = 'Results\\OriginalDataSimplified_CleanCrossTab_FINAL_79added.csv', row.names = T)


#FINAL CROSS TAB, MODIFIED STRATA VS CEO LABELS
table(FINALDATASET$ReadableChangeStrata_Map_Modified,FINALDATASET$CEOreadable_850and79_FINAL_NEWstrata_perennialseparate_epochsgrouped)
crosstab_allclean_MODIFIEDstrata_USEFORANALYSIS <- table(FINALDATASET$ReadableChangeStrata_Map_Modified,FINALDATASET$CEOreadable_850and79_FINAL_NEWstrata_perennialseparate_epochsgrouped)
write.csv(crosstab_allclean_MODIFIEDstrata_USEFORANALYSIS, file = 'Results\\CROSSTAB_FINAL_MODIFIEDstrata_USEFORANALYSIS_850and79.csv', row.names = T)

#FINAL CROSS TAB, MODIFIED STRATA VS CEO LABELS
table(FINALDATASET$ReadableChangeStrata_Map_Modified_simp,FINALDATASET$CEOreadable_850and79_FINAL_NEWstrata_perennialseparate_epochsgrouped)
crosstab_allclean_MODIFIEDstrata_simp <- table(FINALDATASET$ReadableChangeStrata_Map_Modified_simp,FINALDATASET$CEOreadable_850and79_FINAL_NEWstrata_perennialseparate_epochsgrouped)
write.csv(crosstab_allclean_MODIFIEDstrata_simp, file = 'Results\\CROSSTAB_MODIFIEDstrata_simp_850and79.csv', row.names = T)

########## Let's reduce the cleaned dataset to the original 850 to do a comparison or the cross tabs
FINALDATASET_no79 <- (FINALDATASET[order(FINALDATASET$PLOTIDd),][-c(851:929), ])

write.csv(FINALDATASET_no79, file = 'Results\\COMPLETEDDATA_850only_FINAL.csv', row.names = F)
table(FINALDATASET_no79$ReadableChangeStrata_Map_old,FINALDATASET_no79$CEOreadable_850and79_FINAL_NEWstrata_perennialseparate_epochsgrouped)
crosstab_originaldata_allclean_no79 <- table(FINALDATASET_no79$ReadableChangeStrata_Map_old,FINALDATASET_no79$CEOreadable_850and79_FINAL_NEWstrata_perennialseparate_epochsgrouped)
write.csv(crosstab_originaldata_allclean_no79, file = 'Results\\CROSSTAB_ModStrata_FINAL_850only_forComparison.csv', row.names = T)





##################### Not Necessary to Run Past Here
############# Can use the spreadsheet to make a confusion matrix and do analysis

#########################################################################################################
#########################################################################################################


##########################################
### Set up sample design
##########################################
#strat_design <- svydesign(id = ~1, strata = ~StrataName, fpc = ~PixelCount, 
#                          data = FINALDATASET)
#########################################
### once sample design is set up you can analyze the data
##########################################
### survey total (svytotal) calculates area weighted totals of data
#?svydesign()
#
#activityData <- svytotal(~CEOreadable_v4_NEWb, strat_design)
#activityData
#
#Change<-as.data.frame(activityData)
#colnames(Change)<-c('Total, pixels','SE, pixels')
#Change
#rownames(Change)<-c("Deforestation", 
#                    "Degradation", 
#                    "multiple events",
#                    "Reforestation",
#                    "stable forest", 
#                    "stable non-forest",
#                    "stable non-forest agroforestry")
#Change
#
#
### convert to ha
#Change<-Change* 30 * 30 / 10000
#Change
#Change<-round(Change, digits = 0)
#Change
#
#colnames(Change)<-c('Total, ha','SE, ha')
#Change 
#
########
### write results #########
#write.csv(Change, file = 'Results\\AreasofDisturbance_ceoV4version_SAVE_NEW_v2.csv', row.names = T)
#
