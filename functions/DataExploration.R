
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
            Overhead=sum(`overhead included`=='Y')
                         

# Filter the data frame to include only rows where 'your_column' has exactly four characters
CostDatasingleyear <- RawData %>%
  mutate(Time = ifelse(nchar(`time frame of cost data`) == 4, 1, 0))


View(CostData)

######################################################
## use mutate to create new columns based on others ##
######################################################


## new column that equals 1 if capital and labor costs are both included: 

colnames(RawData) # see column names 

RawData <- RawData %>% 
  mutate(cap_lab_costs = ifelse(`includes capital costs` == "Y" & `includes labor costs` == "Y", 1, 0)) ## ifelse inputs are "statement", "print 1 if true", and "print 0 if false"

## new column that specifies whether the spatial extent was a US state or not: 

# great use of chat gpt is to generate these types of strings for you! 
us_states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", 
               "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
               "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", 
               "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", 
               "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", 
               "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")

RawData <- RawData %>% 
  mutate(us_state = ifelse(`spatial extent of study` %in% us_states, "US State", "Not US State")) ## %in% can be read as "is the object on the left contained in the object of strings on the right


# new column to specify whether the word "protected area" is used in either the title or purpose of study
RawData <- RawData %>% 
  mutate(protected_areas = ifelse(str_detect(Title, "protected area") | str_detect(`purpose of study`, "protected area"), 1, 0)) ## | can be read as "or" 


## example of how to write clean data



#write.csv(RawData,paste(DataSource,"/CleanData.csv", sep = ""))


