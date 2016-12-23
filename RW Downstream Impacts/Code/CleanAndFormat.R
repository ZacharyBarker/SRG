################################################################################
#
# Cleans raw data from Riverguages.com and puts into a cleaned csv file
#
# Zac Barker, December 2016
#
################################################################################

CLEAN <- function(FlowFile, StageFile, HeaderSize){
     
     # Reads in the two files associate with the gauge
     flow <- read.csv(FlowFile, header = F, skip = HeaderSize)
     stage <- read.csv(StageFile, header = F, skip = HeaderSize)
     
     # Drop 3rd column
     flow$V3 <- NULL
     stage$V3 <- NULL
     
     # Drop last two rows which do not contain data
     flow <- head(flow, -2)
     stage <- head(stage, -2)
     
     # Formats the time stamps to POSIX
     flow$V1 <- strptime(flow$V1, format = "%m/%d/%Y %H:%M")
     stage$V1 <- strptime(stage$V1, format = "%m/%d/%Y %H:%M")
     
     # Merge flow and stage into one data frame
     df <- merge(flow, stage, by= "V1", all = T)
     
     # Rename the headers
     names(df) <- c("DateTime", "Flow", "Stage")
     
     # Convert the flow and stage to numerics
     df$Flow <- as.numeric(as.character(df$Flow))
     df$Stage <- as.numeric(as.character(df$Stage))
     
     return(df)
     
}