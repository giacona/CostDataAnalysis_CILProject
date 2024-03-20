FormatData <- function(RawData){ 
## this function takes the raw data and formats it for later analyses

#dim(RawData)
#head(RawData)
#View(RawData)

########################################## Clean up existing fields #####################################
# Standardize Currency ----------------------------------------------------

#  which(is.na(RawData$Currency))
#  unique(RawData$Currency)

  RawData[with(RawData, grepl("and", Currency)), ]$Currency <- 'two currencies'
  RawData[with(RawData, grepl("US", Currency)), ]$Currency <- 'USD'   # if US then USD
  RawData[with(RawData, grepl("AU", Currency)), ]$Currency <- 'AUD'    # if AU or A$ then AUD
  RawData[with(RawData, grepl("NZ", Currency)), ]$Currency <- 'NZD'   # if NZ then NZD
  RawData[with(RawData, grepl("uro", Currency)), ]$Currency <- 'Euro'
  RawData[with(RawData, grepl("€", Currency)), ]$Currency <- 'Euro'
  RawData[(RawData$Currency == "A$") & !is.na(RawData$Currency), ]$Currency <- "AUD"
  RawData[(RawData$Currency == "N$") & !is.na(RawData$Currency), ]$Currency <- "NGN" # not so sure
  RawData[(RawData$Currency == "two currencies") & !is.na(RawData$Currency), ]$Currency <- "two currencies (USD included)"
 # unique(RawData$Currency)

# deal with dashes in time frame (duration)########  
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
  RawData <- RawDataTime[,c(1:28,32)]
  
# Standardize rest of time frame (duration) --------------------------------------------------
  ## if there is a value in the duration field use that (from the dash removal above)
  ## if there is a written number in a field, use that as duration
  ## if the time is less than a year (3 months), or said in text to be a year, use 1 year
 
  RawData <- RawData %>% 
  mutate(duration1= case_when(
    parse_number(RawData$duration) >0 ~ parse_number(RawData$duration), 
    RawData$`time frame of cost data` %in% c("7-14 year estimate") ~ 10.5,
    RawData$`time frame of cost data` %in% c(">1969") ~ 46, # original article had data from 1970 to 2016
    RawData$`time frame of cost data` %in% c("single year per country, 
                                             but different countries were surveyed different years",
                                             "3 months",
                                             "2019",
                                             "2021",
                                             "1965") ~ 1,
    parse_number(`time frame of cost data`)<1000 ~ parse_number(`time frame of cost data`),
    
    )
  )  
  
  ## for some reason these two wouldn't behave in the case_when function so i'm forcing them down here
  ## if the time is 7-14 years, substitute mean value, if there are two values, use just one  
  RawData$duration1[which(RawData$duration =="7-14")] <- 10.5
  RawData$duration1[which(RawData$duration =="4,4")] <- 4
 #View(RawData[,c(14,30:31)])
  
  RawData$Duration <- RawData$duration1
  
  RawData <- RawData[,c(1:28,31)]
  

  
#  Clean up date of cost calculation --------------------------------------
  # new column to clean up NAs and ?s in "Date of Cost Calculation"
  RawData <- RawData %>% 
    mutate(`Date_cost_calculated` = case_when(`Date of cost calculation` %in% c("NA", "N/A", "n/a", "NA?", "n/a?", "N/A?") ~ NA_character_, TRUE ~ `Date of cost calculation`)) # reformats NAs
  RawData <- RawData %>% 
    mutate(Date_cost_calculated = gsub("\\?", "", Date_cost_calculated)) # removes ?s
  
##########################################  Add new fields ###############################################
# Aggregate Labor and Capital Cost Categories ---------------------------------------------------------

  RawData <- RawData %>% 
    mutate(cap_lab_costs = ifelse(`includes capital costs` == "Y" & `includes labor costs` == "Y", 1, 0)) ## ifelse inputs are "statement", "print 1 if true", and "print 0 if false"
  

# Is it in a US State -----------------------------------------------------
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
  
# Is it a Protected Area ---------------------------------------------------------------

# new column to specify whether the word "protected area" is used in either the title or purpose of study
RawData <- RawData %>% 
  mutate(protected_areas = ifelse(str_detect(Title, "protected area") | str_detect(`purpose of study`, "protected area"), 1, 0)) ## | can be read as "or" 


# Write Clean Dataset -----------------------------------------------------

write.csv(RawData,paste(DataSource,"/EditedData.csv", sep = "")) # save the edited file to the data repository 
EditedData <- RawData # can use this location to call up saved file if processing is long. Also can do in main MD document
return(EditedData) # return indicates what will get spit out of the function and what will be accessible in the MD doc
}




