
## these first 10 lines will be removed later when we turn this into a function that is called up by the markdown doc
## but for now they are here so that we can call up the correct data file to analyze

library(readr)
library(tidyverse) 
DataSource <- "./data" # input raw dataset goes here
# read in clean data file
EditedData <- read_csv(paste(DataSource,"/EditedData.csv", sep = "")) 
View(EditedData)

## Questions we can answer from this data set with basic summary statistics

## How does the number of papers that report on costs vary over time?
##
summary(EditedData$`Date Published`)
hist(EditedData$`Date Published`)

## how does the duration of costs (how many years the reported data represent) vary across the papers?
summary(EditedData$Duration)
hist(EditedData$Duration) # this is pretty skewed because lots are less than 20
hist(log(EditedData$Duration)) # log is not very intuitive, but can help for understanding what is going on in skewed datasets. Other ways of exploring this could include only looking at those with less than 20 years etc.

## What proportion of the studies are exploring protected area costs?
table(EditedData$protected_areas)
#calculate proportion
table(EditedData$protected_areas)[2]/sum(table(EditedData$protected_areas))

## what proportion of the studies are exploring costs in the US vs elsewhere? -Tyler

## what is the distribution of numbers of papers across the different currencies reported?


## what proportion of studies include capital costs? - Rachel
table(EditedData$`includes capital costs`)
table(EditedData$`includes capital costs`)[3]/sum(table(EditedData$`includes capital costs`))

## what proportion of studies include consumable costs? - Alice
table(EditedData$`includes consumables costs`)
table(EditedData$`includes consumables costs`)[3]/sum(table(EditedData$`includes consumables costs`))

## what proportion of studies include labor costs? -Taylor
table(EditedData$`includes labor costs`)
table(EditedData$`includes labor costs`)[3]/sum(table(EditedData$`includes labor costs`))

## what proportion of studies include overhead costs? - Caitlin
overheadcosts<-table(EditedData$`overhead included`)[2]/sum(table(EditedData$`overhead included`))
overheadcosts <- round((overheadcosts *100), digits=2)
print(paste("Proportion of Studies With Overhead Costs Is",overheadcosts, "%"))

## what proportion of studies include both capital and labor costs? - Rebecca

## what is the distribution of conservation action categories across the studies?
table(EditedData$`Conservation Action Category`)

barplot(table(EditedData$`Conservation Action Category`))

par(mar=c(2,15,2,2))
barplot(table(EditedData$`Conservation Action Category`),horiz = TRUE, las=1) 

## other questions require relating two columns of data
# does the duration of reported cost datasets increase over time?
plot(EditedData$`Date Published`,EditedData$Duration)


