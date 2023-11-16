
#initialize location where you keep data
DataSource <- "./data" # input raw dataset goes here

#load the packages we need
library(tidyverse)     ## install.packages("tidyverse")
library(data.table)
library(stringr)

#read in raw data file

RawData <- read_csv(paste(DataSource,"/CleanData.csv", sep = "")) 

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

# =========== currency column ==========
which(is.na(RawData$Currency))
unique(RawData$Currency)

# if US then USD
# if AU or A$ then AUD
# if NZ then NZD


#RawData %>% filter(grepl("US", Currency))
length(unique(RawData$Currency))
unique(RawData$Currency)

data0 <- RawData
RawData <- data0

RawData[with(RawData, grepl("and", Currency)), ]$Currency <- 'two currencies'
RawData[with(RawData, grepl("US", Currency)), ]$Currency <- 'USD'
RawData[with(RawData, grepl("AU", Currency)), ]$Currency <- 'AUD'
RawData[with(RawData, grepl("NZ", Currency)), ]$Currency <- 'NZD'
RawData[with(RawData, grepl("uro", Currency)), ]$Currency <- 'Euro'
RawData[with(RawData, grepl("€", Currency)), ]$Currency <- 'GBP'
RawData[(RawData$Currency == "A$") & !is.na(RawData$Currency), ]$Currency <- "AUD"
RawData[(RawData$Currency == "N$") & !is.na(RawData$Currency), ]$Currency <- "NGN" # not so sure
RawData[(RawData$Currency == "two currencies") & !is.na(RawData$Currency), ]$Currency <- "two currencies (USD included)"
unique(RawData$Currency)


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

View(CostData)

<<<<<<< Updated upstream
######################################################
## use mutate to create new columns based on others ##
######################################################
=======
# CleanData <- RawData
>>>>>>> Stashed changes


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

# ========== time frame of cost data - dashes ========

RawData$`Number of dashes` <- str_count(RawData$`time frame of cost data`, "-")
unique(RawData$`Number of dashes`) # 0, NA, 1, 2

RawData$`start year` <- NA
RawData$`end year` <- NA
RawData$`duration` <- NA

#RawData[is.na(RawData$`Number of dashes`),]$`Number of dashes` = -1
#unique(RawData$`Number of dashes`)

RawData1 <- RawData[RawData$`Number of dashes` %in% c(1,2), ]
RawData2 <- RawData[!RawData$`Number of dashes` %in% c(1,2), ]

dealDash <- function(x, li_colnames, output){
  
  # get column numbers
  col_dash <- grep("Number of dashes", li_colnames)
  col_timeframe <- grep("time frame of cost data", li_colnames)
  col_start <- grep("start year", li_colnames)
  col_end <- grep("end year", li_colnames)
  col_duration <- grep("duration", li_colnames)
  
  num_dash <- x[col_dash]
  
  if(num_dash == 1){
    years = strsplit(x[col_timeframe], "-")[[1]]
    
    if((nchar(years[1])>=4)&(nchar(years[2])>=4)){ # to avoid NA from an exception row
      y1 = as.numeric(str_sub(years[1], -4, -1))
      y2 = as.numeric(str_sub(years[2], 1, 4))
      yrs = y2-y1
    } else{
      y1 = NA
      y2 = NA
      yrs = gsub("[^0-9,-]", "", x[col_timeframe]) # a regular expression to remove all characters that are not a number or . or -
    }
    
    x[col_start] = y1
    x[col_end] = y2
    x[col_duration] = yrs
  } else if(num_dash == 2){
    no_words = gsub("[^0-9,-]", "", x[col_timeframe]) # remove words
    durations = strsplit(no_words, ",")[[1]]
    duration1 = durations[1]
    d1_years = strsplit(duration1, "-")[[1]]
    
    duration2 = durations[2]
    d2_years = strsplit(duration2, "-")[[1]]
    
    y1 = c(as.numeric(d1_years[1]), as.numeric(d2_years[1]))
    y2 = c(as.numeric(d1_years[2]), as.numeric(d2_years[2]))
    yrs = y2-y1
    
    x[col_start] = paste(y1, collapse=',')
    x[col_end] = paste(y2, collapse=',')
    x[col_duration] = paste(yrs, collapse=',')
  } else{
    x[col_start] = 'unknown'
    x[col_end] = 'unknown'
    x[col_duration] = 'unknown'
  } 
  return(x)
}

RawData11 <- transpose(data.frame(apply(RawData1, 1, dealDash, colnames(RawData))))
colnames(RawData11) = colnames(RawData)
unique(RawData11$duration)

RawDataTime <- rbind(RawData11, RawData2)




# new column to specify whether the word "protected area" is used in either the title or purpose of study
RawData <- RawData %>% 
  mutate(protected_areas = ifelse(str_detect(Title, "protected area") | str_detect(`purpose of study`, "protected area"), 1, 0)) ## | can be read as "or" 


## practice working on the date field

## Gwen's attempt to remove only the numbers that are not years. I found this function called parse_number where i didn't even have to specify that i was looking for a number. It ends up with NAs everywhere there are no numbers but that should be ok because other lines of code will get them
## There are still some issues with the "3 month" field and the "7-14 yr estimate" field but hopefully we can get those with an additional line
## i also realized we will run into an issue if we all do ifelse where we overwrite each other's code when we put it together but we can deal with that next time

RawData <- RawData %>% 
  mutate(duration = ifelse(parse_number(`time frame of cost data`)<1000, parse_number(`time frame of cost data`),0))
                       
View(RawData[,c(13,31)]) # this is an easy way to look at just the columns i'm working on so that i can make sure my code is doing what i want

#write.csv(RawData,paste(DataSource,"/CleanData.csv", sep = ""))

