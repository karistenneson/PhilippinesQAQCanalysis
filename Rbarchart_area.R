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

setwd("C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis")

# area csv
dataarea_NEW <- read.csv('C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\Results\\AreasofDisturbance_ceoV4version_SAVE_NEW.csv')
dataarea_NEW

# area csv
dataarea_old <- read.csv('C:\\Users\\cryst\\OneDrive\\Documents\\Philippines\\PhilippinesQAQCanalysis\\Results\\AreasofDisturbance_ceoV4version_SAVE.csv')
dataarea_old

# create dummy data
#data <- data.frame(
#  name=letters[1:5],
# value=sample(seq(4,15),5),
#  sd=c(1,0.2,3,2,4)
#)

data <- data.frame(dataarea_old)
#vLABELH <- c(145813,152997,90409,160376,286795,355990,346318)

# Calculate y position, placing it in the middle
data <- data %>%
  group_by(Strata) %>%
  #mutate(label_y = cumsum(Area) - 0.5 * Area)
  mutate(label_y = cumsum(Area) -cumsum(Area))
# Calculate error position, placing it above
data <- data %>%
  group_by(Strata) %>%
  mutate(label_error = SE+Area)

# Most basic error bar
ggplot(data, aes(x = Strata, y = Area )) +
  geom_bar( aes(x=Strata, y=Area), stat="identity", fill="skyblue", alpha=1) +
  geom_errorbar( aes(x=Strata, ymin=Area-SE, ymax=Area+SE), width=0.4, colour="red", alpha=0.9, size=0.8) +
  geom_text(aes(label = SE, y = label_error), vjust = -0.5, colour="black", size=3.5) +
  geom_text(aes(y = label_y, label = Area), vjust = 1.2, colour = "black", size=4)


