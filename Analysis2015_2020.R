library(survey)
library(tidyverse)
library(knitr)
library(rmarkdown)
library(tidyr)
library(networkD3)
library(tidyverse)
library(dplyr)


setwd('C:\\Users\\karis\\Documents\\Laos\\LaosDegradation\\6ERPDprovinces')

# CEO data
dataMrgd_15 <- read.csv('Results2015_2020\\ERPD-6-provinces-2015---2020-agreement-2021-12-14.csv')
dataMrgd_15 <- read.csv('Results2015_2020\\JFplotReview1520.csv')
colnames(dataMrgd_15)[1]<-"CEO_plotID"

##############################
## 2015 - 2020
##############################
mapAreas15 <- read.csv('Results2015_2020\\countsReadableSumLaos.csv')
head(mapAreas15)

colnames(dataMrgd_15)
head(dataMrgd_15)

## FCP 
unique(mapAreas15$readable)
unique(dataMrgd_15$readable)

dataMrgd_15 <- merge(dataMrgd_15, mapAreas15, by.x = 'readable', by.y = 'readable', all.x = T)
head(dataMrgd_15)

unique(dataMrgd_15$readable)
dataMrgd_15$map_label<-'update'
dataMrgd_15$map_label[dataMrgd_15$map_value == 0] <- '0_no_data_map'
dataMrgd_15$map_label[dataMrgd_15$map_value == 1] <- '1_stable_forest_map'
dataMrgd_15$map_label[dataMrgd_15$map_value == 2] <- '2_degradation_map'
dataMrgd_15$map_label[dataMrgd_15$map_value == 3] <- '3_deforestation_map'
dataMrgd_15$map_label[dataMrgd_15$map_value == 4] <- '4_reforestation_map'
dataMrgd_15$map_label[dataMrgd_15$map_value == 5] <- '5_not_forest_map'
dataMrgd_15$map_label


##############################
## CEO labels, 2015 - 2020
##############################
colnames(dataMrgd_15)
unique(dataMrgd_15$JF.Final.review)
unique(dataMrgd_15$JF.Land.Cover.2015)
unique(dataMrgd_15$map_label)
#dataMrgd_15$Change_type_label[dataMrgd_15$JF.Final.review=="No Change" |dataMrgd_15$JF.Final.review=="No change " ]<-"1_stable_forest"
#dataMrgd_15$Change_type_label[dataMrgd_15$Change_type_label=="1_stable_forest" & 
#                           dataMrgd_15$LC_2020_final == "Non Forest"]<-"5_not_forest"

dataMrgd_15$Change_type_label[dataMrgd_15$JF.Final.review=="No change" | dataMrgd_15$JF.Final.review=="No change " ]<-"1_stable"
dataMrgd_15$Change_type_label[dataMrgd_15$JF.Final.review=="Degradation" | dataMrgd_15$JF.Final.review=="Degradation "]<-"2_degradation"
dataMrgd_15$Change_type_label[dataMrgd_15$JF.Final.review=="Loss"]<-"3_deforestation"
dataMrgd_15$Change_type_label[dataMrgd_15$JF.Final.review=="Restoration"]<-"4_restoration"
dataMrgd_15$Change_type_label[dataMrgd_15$JF.Final.review=="Reforestation"]<-"4_reforestation"
#dataMrgd_15$Change_type_label[dataMrgd_15$JF.Final.review=="other"]<-"6_other"

dataMrgd_15[,c("Change_type_label","JF.Final.review")]
##############################################################################
#################################################################################
######## Analysis #####################################################################
#################################################################################
##############################################################################

################
## Cross tabulations
################
colnames(dataMrgd_15)
head(dataMrgd_15)
table(dataMrgd_15$Change_type_label)
table(dataMrgd_15$map_label)
table(dataMrgd_15$map_label, dataMrgd_15$Change_type_label)

write.csv(table(dataMrgd_15$map_label, dataMrgd_15$Change_type_label), file = 'Results2015_2020\\Results\\agreement.csv')

################
## Area Estimates of forest changes
################

## CODED
strat_design_15 <- svydesign(id = ~1, strata = ~map_label, fpc = ~count, 
                             data = dataMrgd_15)
strat_design_15

activityData_15 <- svytotal(~Change_type_label, strat_design_15)
activityData_15

Change<-as.data.frame(activityData_15)
colnames(Change)<-c('CCDC SMA Total, ha','CCDC SMA SE, ha')
#rownames(Change)<-c("stable forest", "forest degradation", 
                    "forest loss", "forest restoration/reforestation", 
                    "stable non-forest")

rownames(Change)<-c("stable LC", "forest degradation", 
                    "forest loss", "reforestation", 
                    "restoration/")

Change


## convert to ha
Change<-Change* 30 * 30 / 10000
Change
Change<-round(Change, digits = 0)
Change

#######
## write results #########
write.csv(Change, file = 'Results2015_2020\\Results\\Change1520QAQC.csv', row.names = T)
#######


################################################################
################################################################
## Area Estimates of drivers of degradation
################
unique(dataMrgd_15$Change_type_label) 
colnames(dataMrgd_15)
unique(dataMrgd_15$JF.Driver.of.degradation[dataMrgd_15$Change_type_label=='2_degradation'])

dataMrgd_15$JF.Driver.of.degradation[dataMrgd_15$Change_type_label !='2_degradation']<-'none'
unique(dataMrgd_15$JF.Driver.of.degradation)

strat_design_15 <- svydesign(id = ~1, strata = ~map_label, fpc = ~count, 
                             data = dataMrgd_15)

DriverDeg_15 <- svytotal(~JF.Driver.of.degradation, strat_design_15)
DriverDeg_15

## format into table
Driver<-as.data.frame(DriverDeg_15)
Driver
colnames(Driver)<-c('CCDC SMA total, ha','CCDC SMA SE, ha')
rownames(Driver)<-c("edge", 
                    "fire",
                    "frost",
                    "landslide",
                    "logging",
                    "no forest degradation", 
                    "shifting agriculture")
Driver

## convert to ha
Driver<-Driver* 30 * 30 / 10000
Driver
Driver<-round(Driver, digits = 0)
Driver

Driver <- Driver[c(6, 7, 5, 1, 3, 4, 2),]
write.csv(Driver, file = 'Results2015_2020\\Results\\DriverofDeg1520QAQC.csv', row.names = T)
