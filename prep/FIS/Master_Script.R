#### Example using code to generate b/bmsy values for fisheries catch data
#### Code is for a uniform distribution and no "res" values
## It depends on which program you sourec: Uniform or Relaxed
#### This was the original model used to calculate the OHI values

rm(list = ls())
library(dplyr)
library(ggplot2)
library(grid)
library(parallel)

############################################################
## Running CMSY script on handful of species to compare data (July 16 2014)----
## NOTE: if doing this again, the code for the next set of species is
## probably more organized.
############################################################

##Se realizaran los analisis por macrozona

#cdat <- read.csv("testData.csv", stringsAsFactors=FALSE)
cdatotal <- read.csv("prep/FIS/data_bmsy_na.csv", stringsAsFactors=FALSE)
cdatotal <- cdatotal[!is.na(cdatotal$stock_id),]

## Run bbmsy script
source('prep/FIS/cmsy_uniform.R')
#source('prep/FIS/cmsy_relaxed.R')

### run CMSY function:
#start the clock
Sys.time()
ptm <- proc.time()


cdat<-cdatotal %>% filter(MACROZONA == "AUSTRAL")%>%
  group_by(MACROZONA, id, stock_id, res, yr) %>%
  summarise(ct = sum(ct)) %>%
  as.data.frame() %>%
  select(-c("MACROZONA"))

b_bmsy <- data.frame() # create empty dataframe to add values

for(i in 1:length(unique(cdat$stock_id))){
  #for(i in 1:1){    ##troubleshooting
  test <- runCMSY(cdat, stockNumber=i)
  new <- data.frame(taxon_name=test[[1]],
                    b_bmsy=test[[2]],
                    year=test[[7]])
  b_bmsy <- rbind(b_bmsy, new)
}


b_bmsy<- cbind(b_bmsy, MACROZONA = "AUSTRAL")

# Stop the clock
proc.time() - ptm
Sys.time()

#write.csv(b_bmsy, "prep/FIS/my_bbmsy_uniform.csv", row.names=FALSE) # Does not consider resilience. Uniform priors
write.csv(b_bmsy, "my_bbmsy_relaxed_9.csv", row.names=FALSE) #Considers Resilience and relaxed priors
