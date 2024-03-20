SummarizeData_RBL <- function(EditedData){ 


# ## what proportion of studies include capital costs? - Rachel
table(EditedData$`includes capital costs`)
table(EditedData$`includes capital costs`)[3]/sum(table(EditedData$`includes capital costs`))

summarytabledata_RBL <- tibble(c("Yes","N/a","No"),
                           c(table(EditedData$`includes capital costs`)[3],
                             table(EditedData$`includes capital costs`)[2],
                             table(EditedData$`includes capital costs`)[1]
                             ),
                           c(round(table(EditedData$`includes capital costs`)[3]/sum(table(EditedData$`includes capital costs`)), digits = 2), 
                           round(table(EditedData$`includes capital costs`)[2]/sum(table(EditedData$`includes capital costs`)), digits = 2), 
                          round(table(EditedData$`includes capital costs`)[1]/sum(table(EditedData$`includes capital costs`)), digits = 2)
                          ),.name_repair = "unique")
names(summarytabledata_RBL)<-c("","count","proportion")

return(summarytabledata_RBL)

}




