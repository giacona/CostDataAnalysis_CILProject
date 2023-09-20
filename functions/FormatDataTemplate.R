FormatData <- function(RawData){ 
## this function takes the raw data and formats it for later analyses

#dim(RawData)
#head(RawData)
#View(RawData)

write.csv(RawData,paste(DataSource,"/EditedData.csv", sep = "")) # save the edited file to the data repository (could also go in output repository)

EditedData<- RawData # can use this location to call up saved file if processing is long. Also can do in main MD document


XXXData <- EditedData

return(XXXData) # return indicates what will get spit out of the function and what will be accessible in the MD doc

}