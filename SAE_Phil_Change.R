#Unbiased Area Estimation of Philippines Forest Change
#Written by Karis Tenneson and Crystal Wespestad (SIG) Feb, 2022
#####################################################################

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
dataCEO$lon[duplicated(dataCEO$lon)]
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
dataCEO_combo<-rbind(dataCEO_TOADD, dataCEO)
nrow(dataCEO_combo)
head(dataCEO_combo)
tail(dataCEO_combo)

#Update the columnn names for original CEO data to shorten them
colnames(dataCEO_combo)
colnames(dataCEO_combo)[14]<-"confidence"
colnames(dataCEO_combo)[15]<-"confidence.reasoning"
colnames(dataCEO_combo)[16]<-"LC2000"
colnames(dataCEO_combo)[17]<-"Forest.Type.2000"
colnames(dataCEO_combo)[18]<-"NonForest.Type.2000" 
colnames(dataCEO_combo)[19]<-"Crop.Type.2000" 
colnames(dataCEO_combo)[20]<-"LC2018end"
colnames(dataCEO_combo)[21]<-"forest.change.event" 
colnames(dataCEO_combo)[23]<-"ChangeType.1"
colnames(dataCEO_combo)[24]<-"Degradation.Driver.1"
colnames(dataCEO_combo)[25]<-"Loss.Driver.1"
colnames(dataCEO_combo)[26]<-"second.forest.change.event"
colnames(dataCEO_combo)[28]<-"ChangeType.2"
colnames(dataCEO_combo)[29]<-"Degradation.Driver.2"
colnames(dataCEO_combo)[30]<-"Loss.Driver.2"
colnames(dataCEO_combo)[31]<-"third.forest.change.event" 
colnames(dataCEO_combo)[33]<-"ChangeType.3"
colnames(dataCEO_combo)[34]<-"Degradation.Driver.3"
colnames(dataCEO_combo)[35]<-"Loss.Driver.3"
colnames(dataCEO_combo)[36]<-"morethan3.forest.changes" 
colnames(dataCEO_combo)[37]<-"describe.extra.changes" 
colnames(dataCEO_combo)[38]<-"Forest.Type.2018" 
colnames(dataCEO_combo)[39]<-"NonForest.Type.2018" 
colnames(dataCEO_combo)[40]<-"Crop.Type.2018" 
colnames(dataCEO_combo)[41]<-"mixed.lc.pixel"
colnames(dataCEO_combo)

#output the cleaned CEO data
write.csv(dataCEO_combo, file = 'Results\\Philippines_CEO_FinalPoints_SAVE.csv', row.names = F)

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
datamerged <- merge(dataCEO_combo, data_mapstrata[c('pl_change_strata','plotid')], by.x =c('plotid'), by.y = c('plotid'), all.x=T)
head(datamerged)
dim(datamerged)

#add a human readable column for change strata
colnames(datamerged)[42]<-"ChangeStrata_Map"
datamerged$ReadableChangeStrata_Map<-"fixMe"
datamerged$ReadableChangeStrata_Map[datamerged$ChangeStrata_Map == 31]<-"deforested epoch 1"
datamerged$ReadableChangeStrata_Map[datamerged$ChangeStrata_Map == 32]<-"deforested epoch 2 & 3"
datamerged$ReadableChangeStrata_Map[datamerged$ChangeStrata_Map == 41]<-"reforested epoch 1"
datamerged$ReadableChangeStrata_Map[datamerged$ChangeStrata_Map == 42]<-"reforested epoch 2 & 3"
datamerged$ReadableChangeStrata_Map[datamerged$ChangeStrata_Map == 50]<-"stable forest"
datamerged$ReadableChangeStrata_Map[datamerged$ChangeStrata_Map == 60]<-"stable non forest"
datamerged$ReadableChangeStrata_Map[datamerged$ChangeStrata_Map == 70]<-"multiple events noise"
datamerged$ReadableChangeStrata_Map[datamerged$ChangeStrata_Map == 80]<-"multiple events ecologically possible"
unique(datamerged$ReadableChangeStrata_Map)
colnames(datamerged)

##### Epoch 1 = 2000-2005
##### Epoch 2 = 2006-2012
##### Epoch 3 = 2013-2018

########Simplified Map data
datamerged$ReadableChangeStrata_Map_v1<-"fixMe"
datamerged$ReadableChangeStrata_Map_v1[datamerged$ChangeStrata_Map == 31]<-"Deforestation"
datamerged$ReadableChangeStrata_Map_v1[datamerged$ChangeStrata_Map == 32]<-"Deforestation"
datamerged$ReadableChangeStrata_Map_v1[datamerged$ChangeStrata_Map == 41]<-"Reforestation"
datamerged$ReadableChangeStrata_Map_v1[datamerged$ChangeStrata_Map == 42]<-"Reforestation"
datamerged$ReadableChangeStrata_Map_v1[datamerged$ChangeStrata_Map == 50]<-"stable forest"
datamerged$ReadableChangeStrata_Map_v1[datamerged$ChangeStrata_Map == 60]<-"stable non forest"
datamerged$ReadableChangeStrata_Map_v1[datamerged$ChangeStrata_Map == 70]<-"multiple events noise"
datamerged$ReadableChangeStrata_Map_v1[datamerged$ChangeStrata_Map == 80]<-"multiple events ecologically possible"
unique(datamerged$ReadableChangeStrata_Map_v1)
colnames(datamerged)

#############Re-import created file after illogial points fixed by Trina and Liz
datamerged_FIXED <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\Philippines_compiled_checked_TrinaLiz.csv")


############## Strata pixel counts #############
strataAreas <- read.csv("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\data\\philippines_pixelcounts.csv")
head(strataAreas)
datamerged_FIXED <- merge(datamerged_FIXED, strataAreas, by.x = 'ChangeStrata_Map', by.y = 'Map.value', all.x = F)
dim(datamerged_FIXED)

############Set up clean CEO labels - Version 1
#Right now the order in which you define these matters
#In version 1: Pixels with multiple events are not separated out
datamerged$CEOreadable_v1 <- ifelse(datamerged$ChangeType.1 == "Degradation" | datamerged$ChangeType.2 == "Degradation" | datamerged$ChangeType.3 == "Degradation" , 'Degradation',
                                  ifelse(datamerged$ChangeType.1 == "Deforestation"| datamerged$ChangeType.2 == "Deforestation" | datamerged$ChangeType.3 == "Deforestation", 'Deforestation', 
                                         ifelse(datamerged$ChangeType.1 == "Reforestation"| datamerged$ChangeType.2 == "Reforestation" | datamerged$ChangeType.3 == "Reforestation", 'Reforestation', 
                                                ifelse(datamerged$forest.change.event == "N/A non-forest entire time", "stable non forest",
                                                       ifelse(datamerged$forest.change.event == "no", "stable forest", 'NotReviewed')))))
table(datamerged$CEOreadable_v1)
write.csv(datamerged, file = 'Results\\Philippines_Test1.csv', row.names = F)

############Set up clean CEO labels - Version 2
#In version 2: Pixels with multiple events ARE separated out
'%!in%' <- Negate('%in%')
datamerged$CEOreadable_v2 <- ifelse(datamerged$ChangeType.1 == "Degradation" & datamerged$ChangeType.2 %!in% c("Degradation","Deforestation","Regeneration"), 'Degradation',
                                    ifelse(datamerged$ChangeType.1 == "Deforestation" & datamerged$ChangeType.2 %!in% c("Degradation","Deforestation","Regeneration"), 'Deforestation', 
                                           ifelse(datamerged$ChangeType.1 == "Reforestation" & datamerged$ChangeType.2 %!in% c("Degradation","Deforestation","Regeneration"), 'Reforestation',
                                                  ifelse(datamerged$morethan3.forest.changes == "no", 'multiple events ecologically possible',
                                                         ifelse(datamerged$morethan3.forest.changes == "yes", 'multiple events noise',
                                                                ifelse(datamerged$ChangeType.2 %in% c("Degradation","Deforestation","Regeneration") & datamerged$third.forest.change.event == "no", 'multiple events ecologically possible',
                                                                       ifelse(datamerged$forest.change.event == "N/A non-forest entire time", "stable non forest",
                                                                              ifelse(datamerged$forest.change.event == "no", "stable forest", 'NotReviewed'))))))))
table(datamerged$CEOreadable_v2)
write.csv(datamerged, file = 'Results\\Philippines_Test2.csv', row.names = F)

#############################################################
#############################################################
#   ANALYSIS
#############################################################
#############################################################

#cross tab of strata 
table(datamerged$ReadableChangeStrata_Map_v1, datamerged$CEOreadable_v2)
table_mapv1_CEOv2 <- table(datamerged$ReadableChangeStrata_Map_v1, datamerged$CEOreadable_v2)
write.csv(table_mapv1_CEOv2, file = 'Results\\CrossTable_mapv1_ceov2.csv', row.names = T)
#########################################
## Set up sample design
#########################################
strat_design <- svydesign(id = ~1, strata = ~ReadableChangeStrata_Map_v1, fpc = ~pixel.count, 
                          data = datamerged)
#########################################
## once sample design is set up you can analyze the data
#########################################
colnames(datamerged)
## survey total (svytotal) calculates area weighted totals of data
#?svytotal()

activityData <- svytotal(~CEOreadable_v1, strat_design)
activityData

Change<-as.data.frame(activityData)
colnames(Change)<-c('Total, pixels','SE, pixels')
Change
rownames(Change)<-c("forest degradation", 
                    "forest loss", 
                    "more than one type of event", 
                    "reforestation",
                    "stable forest", 
                    "stable non-forest")
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
write.csv(Change, file = 'Results\\AreasofDisturbance_INCLUDINGQAQC_SAVE.csv', row.names = T)















#########################################################

################# data set cleaning ################
## fill records with empty disturbance type with the stable label from disturbance column
unique(dataCEO_QAQC$distType)
table(dataCEO_QAQC$distType)
colnames(dataCEO_QAQC)
dataCEO_QAQC$distType_QAQC[dataCEO_QAQC$distType_QAQC==""]<-dataCEO_QAQC$disturbance_QAQC[dataCEO_QAQC$distType_QAQC==""]
## remove empty data row
nrow(dataCEO_QAQC)
dataCEO_QAQC<-dataCEO_QAQC[dataCEO_QAQC$distType!="",]
nrow(dataCEO_QAQC)



colnames(dataCEO_combo)
colnames(dataMapValues)
colnames(strataAreas)
nrow(dataCEO_combo)
nrow(dataMapValues)
## FCP 
# look at unique classes of integer strata labels in each file (CEO and pixel counts)
tail(unique(dataCEO_combo$CEO_PLOTID_NEW)) 
tail(unique(dataMapValues$CEO_PLOTID_sudo))
tail(sort(dataCEO_combo$lon)) 
tail(sort(dataMapValues$LON))

# look at unique classes of integer strata labels in each file (CEO and pixel counts)
unique(dataCEO_combo$pl_changestrata)
unique(dataMapValues$ChangeStrata)
unique(strataAreas$map_value)
#Check
#write.csv(dataCEO_combo, file = 'Results\\dataCEO_combo_beforemerge.csv', row.names = T)


######test whether rounding removes disagreement 
#>> didn't work, not enough didgets to be unique
#dataCEO_combo$lon <- round(dataCEO_combo$lon,5)
#dataCEO_combo$lat <- round(dataCEO_combo$lat,5)
#dataMapValues2$LON <- round(dataMapValues2$LON,5)
#dataMapValues2$LAT <- round(dataMapValues2$LAT,5)
#Any duplicates?
#sort(dataMapValues2$LON[duplicated(dataMapValues2$LON)])
#write.csv(dataCEO_combo, file = 'Results\\dataCEO_combo_round_nomerge.csv', row.names = F)
##############################################

## Merge dataset with CEO answers with strata info
#OLD>>> dataCEO_combo <- merge(dataCEO_combo, dataMapValues, by.x = c("lon","lat"), by.y = c("LON","LAT"), all.x = F)
FINALDATASET <- merge(FINALDATASET, dataMapValues, by.x = c("lon","lat"), by.y = c("LON","LAT"), all.x = F)

#OLD >>> write.csv(dataCEO_combo, file = 'Results\\dataCEO_combo_FINALTEST.csv', row.names = F)
write.csv(dataCEO_combo, file = 'Results\\FINALDATASET_FINALTEST.csv', row.names = F)



## fill records with empty disturbance type with the stable label from disturbance column
FINALDATASET$DISTTYPE[FINALDATASET$DISTTYPE==""]<-FINALDATASET$DISTURBANCE[FINALDATASET$DISTTYPE==""]
#write.csv(FINALDATASET, file = 'Results\\FINALDATASET_beforeemptyanswersremoved.csv', row.names = F)
unique(FINALDATASET$DISTTYPE)
table(FINALDATASET$DISTTYPE)

## remove empty data row
FINALDATASET<-FINALDATASET[FINALDATASET$DISTTYPE!="",]
nrow(FINALDATASET)
write.csv(FINALDATASET, file = 'Results\\FINALDATASET_JUSTBEFOREANALYSIS.csv', row.names = F)

#################Get ready to calculate agreement for each map with CEO data#######################

##############################################################################
#################################################################################
######## Analysis #####################################################################
#################################################################################
##############################################################################

################
## Cross tabulations
################
colnames(FINALDATASET)
head(FINALDATASET)

# counts of answers
table(FINALDATASET$DISTURBANCE)
table(FINALDATASET$DISTTYPE)
table(FINALDATASET$strata)


#################################
## agreement matrix
#################################
#cross tab of strata by disturbance type
table(FINALDATASET$strata, FINALDATASET$DISTTYPE)

#create a column that has reclassified map labels to match simplified CEO
unique(FINALDATASET$DISTTYPE)
unique(FINALDATASET$strata)

FINALDATASET$SimpleMapClass<-"fixMe"
FINALDATASET$SimpleMapClass[FINALDATASET$strata == "stable forest"]<-"SF"
FINALDATASET$SimpleMapClass[FINALDATASET$strata == "nonforest "]<-"SNF"

FINALDATASET$SimpleMapClass[FINALDATASET$strata == "1LOSS" | 
                         FINALDATASET$strata == "2MATCHLOSS" | 
                         FINALDATASET$strata == "3MATCHLOSS"]<-"Loss"

FINALDATASET$SimpleMapClass[FINALDATASET$strata == "1DEG" | 
                         FINALDATASET$strata == "2MATCHDEG" | 
                         FINALDATASET$strata == "3MATCHDEG"]<-"Degradation"

FINALDATASET$SimpleMapClass[FINALDATASET$strata == "1DEG1LOSS"]<-"multi-disturbance"

#create a column that has simplified CEO
unique(FINALDATASET$DISTTYPE)
unique(FINALDATASET$strata)

FINALDATASET$SimpleCEOClass<-"fixMe"
FINALDATASET$SimpleCEOClass[FINALDATASET$DISTTYPE == "No - Stable Forest"]<-"SF"
FINALDATASET$SimpleCEOClass[FINALDATASET$DISTTYPE == "No - Stable Non Forest"]<-"SNF"
FINALDATASET$SimpleCEOClass[FINALDATASET$DISTTYPE == "Deforestation (loss)"] <-"Loss"
FINALDATASET$SimpleCEOClass[FINALDATASET$DISTTYPE == "Degradation"]<-"Degradation"
FINALDATASET$SimpleCEOClass[FINALDATASET$DISTTYPE == "more than one type"]<-"multi-disturbance"
FINALDATASET$SimpleCEOClass[FINALDATASET$DISTTYPE == "Reforestation"]<-"Reforestation"

table(FINALDATASET$DISTTYPE)
table(FINALDATASET$strata)
table(FINALDATASET$SimpleMapClass)
table(FINALDATASET$SimpleCEOClass)

#agreement<-table(dataCEO_combo$strata, dataCEO_combo$distType)
agreement<-table(FINALDATASET$SimpleMapClass, FINALDATASET$SimpleCEOClass)
names(dimnames(agreement)) <- c('Map','CEO')
agreement
write.csv(agreement, file = 'Results\\Agreements_INCLUDINGQAQC_SAVE.csv', row.names = T)



###############################################################
#post stratification
#switch up the 'strata' for post stratification
########LandTrendr
activityData <- svytotal(~interaction(DISTTYPE,LTreadable), strat_design)
activityData
Change<-as.data.frame(activityData)
colnames(Change)<-c('Total, pixels','SE, pixels')
Change
## convert to ha
Change<-Change* 30 * 30 / 10000
Change
Change<-round(Change, digits = 0)
Change
colnames(Change)<-c('Total, ha','SE, ha')
Change 
write.csv(Change, file = 'Results\\AreasofDisturbance_PostStrat_LTreadable_SAVE.csv', row.names = T)
########MachineLearning
activityData <- svytotal(~interaction(DISTTYPE,MLreadable), strat_design)
activityData
Change<-as.data.frame(activityData)
colnames(Change)<-c('Total, pixels','SE, pixels')
Change
## convert to ha
Change<-Change* 30 * 30 / 10000
Change
Change<-round(Change, digits = 0)
Change
colnames(Change)<-c('Total, ha','SE, ha')
Change 
write.csv(Change, file = 'Results\\AreasofDisturbance_PostStrat_MLreadable_SAVE.csv', row.names = T)

########CODED
activityData <- svytotal(~interaction(DISTTYPE,CODEDreadable), strat_design)
activityData
Change<-as.data.frame(activityData)
colnames(Change)<-c('Total, pixels','SE, pixels')
Change
## convert to ha
Change<-Change* 30 * 30 / 10000
Change
Change<-round(Change, digits = 0)
Change
colnames(Change)<-c('Total, ha','SE, ha')
Change 
write.csv(Change, file = 'Results\\AreasofDisturbance_PostStrat_CODEDreadable_SAVE.csv', row.names = T)

#############STACKEDreadable_likely
activityData <- svytotal(~interaction(DISTTYPE,STACKEDreadable_likely), strat_design)
activityData
cross_Likely <- table(FINALDATASET$STACKEDreadable_likely, FINALDATASET$DISTTYPE)
write.csv(cross_Likely, file = 'Results\\CrossTable_Likely_SAVE.csv', row.names = T)
Change<-as.data.frame(activityData)
colnames(Change)<-c('Total, pixels','SE, pixels')
Change
## convert to ha
Change<-Change* 30 * 30 / 10000
Change
Change<-round(Change, digits = 0)
Change
colnames(Change)<-c('Total, ha','SE, ha')
Change 
write.csv(Change, file = 'Results\\AreasofDisturbance_PostStrat_STACKEDreadable_likely_SAVE.csv', row.names = T)

##########STACKEDreadable_verylikely
activityData <- svytotal(~interaction(DISTTYPE,STACKEDreadable_verylikely), strat_design)
activityData
cross_VeryLikely <- table(FINALDATASET$STACKEDreadable_verylikely, FINALDATASET$DISTTYPE)
write.csv(cross_VeryLikely, file = 'Results\\CrossTable_VeryLikely_SAVE.csv', row.names = T)
Change<-as.data.frame(activityData)
colnames(Change)<-c('Total, pixels','SE, pixels')
Change
## convert to ha
Change<-Change* 30 * 30 / 10000
Change
Change<-round(Change, digits = 0)
Change
colnames(Change)<-c('Total, ha','SE, ha')
Change 
write.csv(Change, file = 'Results\\AreasofDisturbance_PostStrat_STACKEDreadable_verylikely_SAVE.csv', row.names = T)