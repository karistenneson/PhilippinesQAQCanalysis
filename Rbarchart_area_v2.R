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
library(lubridate)
setwd("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis")


# area csv
#dataarea_NEW <- read.csv('Results\\AreasofDisturbance_ceoV4version_SAVE_NEW.csv') #R results
dataarea_NEW <- read.csv('Results\\AreasofDisturbance_ceoV4version_SAVE_NEW_OlafssonWay.csv')
dataarea_NEW$phase<-'review'
dataarea_NEW
# area csv
#dataarea_old <- read.csv('Results\\AreasofDisturbance_ceoV4version_SAVE_Old.csv') #R results
dataarea_old <- read.csv('Results\\AreasofDisturbance_ceoV4version_SAVE_OLD_OlafssonWay.csv')
dataarea_old$phase<-'initial'
colnames(dataarea_old)[1]<-"Strata"
dataarea_old



?comma_format()
#type.convert(dataarea_NEW$Area, as.is=TRUE)   ###kinda helped
dataTest <- rbind(dataarea_NEW, dataarea_old)
dataTest$Arealab <- comma_format()(dataTest$Area)
#dataTest$Arealab <- 1


dataTest$SElab <- paste0('+/- ', comma_format()(dataTest$SE)) 
###above got error: Error in x * scale : non-numeric argument to binary operator

data <- data.frame(dataTest)
#vLABELH <- c(145813,152997,90409,160376,286795,355990,346318)

# Calculate y position, placing it in the middle
####BELOW GETS LOTS OF ERRORS####
data <- data %>%
  group_by(Strata) %>%
  #mutate(label_y = cumsum(Area) - 0.5 * Area)
  mutate(label_y = cumsum(Area) -cumsum(Area))
# Calculate error position, placing it above
data <- data %>%
  group_by(Strata) %>%
  mutate(label_error = SE+Area)

# Most basic error bar
ggplot(data, aes(fill= phase, x = Strata, y = Area )) +
  geom_bar(position="dodge", stat="identity", alpha=1) +
  geom_errorbar(aes(ymin = Area - SE, ymax = Area + SE),
                width = 0.2, colour = "black", 
                position = position_dodge(.9)) +
  geom_text(aes(label = Arealab, y = label_error), position = position_dodge(.9), 
            vjust = -1.45, colour = "black", size=2.8)+
  geom_text(aes(label = SElab, y = label_error), position = position_dodge(.9), 
            vjust = -0.35, colour="black", size=2.5) +
  xlab('LU activities') + # for the x axis label
  ylab('Area, ha') + scale_y_continuous(labels = comma)


