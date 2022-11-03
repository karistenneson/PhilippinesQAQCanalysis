#Make a bar chart
#Crystal Wespestad (SIG) March, 2022
#####################################################################

library(survey)
library(tidyverse)
library(knitr)
library(rmarkdown)
library(tidyr) 
library(networkD3)
library(tidyverse)
library(dplyr)
library(scales)
#library(lubridate)
setwd("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis")


# area csv
#dataarea_NEW <- read.csv('Results\\AreasofDisturbance_ceoV4version_SAVE_NEW.csv') #R results
#dataarea_NEW <- read.csv('Results\\AreasofDisturbance_ceoV4version_SAVE_NEW_OlafssonWay.csv')
#dataarea_NEW <- read.csv('Results\\AreasofDisturbance_ceoV4version_SAVE_NEW_OlafssonWay_v4.csv')
#dataarea_NEW <- read.csv('Results\\NewStratMap_AE_CI_forgraph_v1.csv')
#dataarea_NEW <- read.csv('Results\\SimRealistic35min_AE_CI_forgraph_v1.csv.csv')
#dataarea_NEW$Options<-'2. 35 minimum (Realistic Simulation)'
#dataarea_NEW
# area csv
#dataarea_old <- read.csv('Results\\AreasofDisturbance_ceoV4version_SAVE_Old.csv') #R results
#dataarea_old <- read.csv('Results\\AreasofDisturbance_ceoV4version_SAVE_OLD_OlafssonWay.csv')
#dataarea_old <- read.csv('Results\\AreasofDisturbance_ceoV4version_SAVE_OLD_OlafssonWay_v2.csv')
dataarea_old <- read.csv('Results\\oldStratMap_AE_CI_forgraph_v1.csv')
dataarea_old$Options<-'1.Original Strata Map V1'
colnames(dataarea_old)[1]<-"Strata"
dataarea_old


dataarea_sim50ideal <- read.csv('Results\\BestCase50NewMapSimulation_AE_CI_forgraph_v1.csv')
dataarea_sim50ideal$Options <- '6.Simulation50_AllNewPointsAgreeWithMap'
colnames(dataarea_sim50ideal)[1] <- "Strata"

dataarea_sim35ideal <- read.csv('Results\\BestCase35NewMapSimulation_AE_CI_forgraph_v1.csv')
dataarea_sim35ideal$Options <- '4.Simulation35_AllNewPointsAgreeWithMap'
colnames(dataarea_sim35ideal)[1] <- "Strata"


dataarea_sim51realistic <- read.csv('Results\\realisticSim51_AE_CI_forgraph_v1.csv')
dataarea_sim51realistic$Options <- '3. Minimum  (51Realistic Simulation)'
colnames(dataarea_sim50realistic)[1] <- "Strata"


dataarea_sim35realistic <- read.csv('Results\\realisticSim35_AE_CI_forgraph_v1.csv')
dataarea_sim35realistic$Options <- '2. Minimum 35 (Realistic Simulation)t'
colnames(dataarea_sim35realistic)[1] <- "Strata"

?comma_format()
#type.convert(dataarea_NEW$Area, as.is=TRUE)   ###kinda helped


##########################################################################
#     SWITCH THE UNCOMMENT DATATEST ROW HERE FOR PLOTTING DIFFERENT COMBOS
##########################################################################

#This is all of them
#dataTest <- rbind(dataarea_old, dataarea_NEW, dataarea_sim50ideal,dataarea_sim50realistic, dataarea_sim35ideal,dataarea_sim35realistic)

#This is just Old and New
dataTest <- rbind(dataarea_old, dataarea_sim35realistic, dataarea_sim51realistic)

#This is just for 35 points plus new
#dataTest <- rbind(dataarea_old, dataarea_NEW, dataarea_sim35ideal,dataarea_sim35realistic)


dataTest$AElab <- comma_format()(dataTest$AE)
#dataTest$Arealab <- 1


dataTest$CIlab <- paste0('+/- ', comma_format()(dataTest$CI)) 
###above got error: Error in x * scale : non-numeric argument to binary operator

dataTest$MOElab <- comma_format()(dataTest$MOE*100)
dataTest$MOElab <- as.numeric(dataTest$MOElab)
dataTest$MOElab <- sprintf(dataTest$MOElab,fmt = '%#.1f')

#dataTest$MOElab <- as.character(dataTest$MOElab)
data <- data.frame(dataTest)
#vLABELH <- c(145813,152997,90409,160376,286795,355990,346318)

# Calculate y position, placing it in the middle
####BELOW GETS LOTS OF ERRORS####
data <- data %>%
  group_by(Strata) %>%
  #mutate(label_y = cumsum(Area) - 0.5 * Area)
  mutate(label_y = cumsum(AE) -cumsum(AE))
# Calculate error position, placing it above
data <- data %>%
  group_by(Strata) %>%
  mutate(label_error = CI+AE)

# Most basic error bar
AE_CI_plot <- ggplot(data, aes(fill= Options, x = Strata, y = AE )) +
  geom_bar(position="dodge", stat="identity", alpha=1) +
  geom_errorbar(aes(ymin = AE - CI, ymax = AE + CI),
                width = 0.2, colour = "black", 
                position = position_dodge(.9)) +
  
  geom_text(aes(label = AElab, y = label_error), position = position_dodge(.9), 
            vjust = -0.4, hjust = -0.24, colour = "black", size=2.8, angle=90)+
  geom_text(aes(label = CIlab, y = label_error), position = position_dodge(.9), 
            vjust = 0.9, hjust = -0.27, colour="black", size=2.8, angle=90) +
  #geom_text(aes(label = MOElab, y = label_error), position = position_dodge(0.9), 
  #          vjust = -0.4, colour = "black", size=2.5)+
  xlab('Activity Classes') + # for the x axis label
  ylab('Area, ha') + scale_y_continuous(labels = comma)+
  expand_limits(y=16900000)
AE_CI_plot
ggsave("my_AE_CI_plot_sims_v2.jpeg",width = 12, height = 6, units = c("in"), dpi = 300)
dev.off()
graphics.off()


#plot the MOE values, the uncertainty %, for all categories
dataMOE <- data.frame(dataTest)
dataMOE <- dataMOE %>%
  group_by(Strata) %>%
  mutate(label_error = MOE*100)

MOEplot <- ggplot(dataMOE, aes(fill= Options, x = Strata, y = MOE*100 )) +
  geom_bar(position="dodge", stat="identity", alpha=1) +
  geom_text(aes(label = MOElab, y = label_error), position = position_dodge(0.9), 
            hjust = -0.3, colour = "black", size=2.8, angle = 90)+
  xlab('Activity Classes') + # for the x axis label
  ylab('Margin of Error (%)') + scale_y_continuous(labels = comma)+
  expand_limits(y=45)

MOEplot
ggsave("my_MOE_sims_V2.jpeg",width = 10, height = 6, units = c("in"), dpi = 300)
dev.off()
graphics.off()  
