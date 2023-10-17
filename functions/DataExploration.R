
#initialize location where you keep data
DataSource <- "./data" # input raw dataset goes here

#load the packages we need
library(tidyverse)     ## install.packages("tidyverse")

#read in raw data file

RawData <- read_csv(paste(DataSource,"/CostPortal_20230918.csv", sep = "")) 

View(RawData) 

dim(RawData)

str(RawData) # ?str - displays the structure of an object

# check info in columns to just to see what we have
# base R exploration

# Date field
table(RawData$`Date Published`)
summary(RawData$`Date Published`)

# data source field
table(RawData$`Data Source`)
length(which(is.na(RawData$`Data Source`)))

# action category
table(RawData$`Conservation Action Category`)
which(is.na(RawData$`Conservation Action Category`))

# time frame
unique(RawData$`time frame of cost data`) # ugh, what a mess

#spatial extent
table(RawData$`spatial extent of study`)
unique(RawData$`spatial extent of study`)  # needs help

# etc....

## tidyverse tools for wrangling
filter(RawData,`Date Published` ==2020)
select(RawData, `Citation`, `includes capital costs`,`includes consumables costs`,`includes labor costs`, `overhead included`)

# other tidyverse tools
#summarise()
#mutate()
#arrange()

## use pipes to put it all together
# how many papers each year mention each cost category?
CostData <- RawData %>%
  group_by(`Date Published`) %>%
  summarise(Capital=sum(`includes capital costs`=='Y'),
            Consumables=sum(`includes consumables costs`=='Y'),
            Labor=sum(`includes labor costs`=='Y'), 
            Overhead=sum(`overhead included`=='Y'))




