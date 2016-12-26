################################################################################
#
# Cleans raw data from Riverguages.com and puts into a cleaned csv file
#
# Zac Barker, December 2016
#
################################################################################

CLEAN <- function(FlowFile, StageFile, HeaderSize){
     
     # Reads in the two files associate with the gauge
     flow <- read.xls(FlowFile, sheet = 1, perl = "C:/Perl/bin/perl.exe")
     stage <- read.xls(StageFile, sheet = 1, perl = "C:/Perl/bin/perl.exe")
     
     # Drop 3rd column
     flow[,3] <- NULL
     stage[,3] <- NULL
     
     # Drop last row which do not contain data
     flow <- head(flow, -1)
     stage <- head(stage, -1)
     
     # Drop the header
     flow <- tail(flow, -HeaderSize)
     stage <- tail(stage, -HeaderSize)
     
     # Formats the time stamps to POSIX
     flow[,1] <- as.character(flow[,1])
     flow[,1] <- substr(flow[,1],1,nchar(flow[,1])-3)
     flow[,1] <- as.POSIXct(flow[,1], format = "%Y-%m-%d %H:%M:%S")
     stage[,1] <- as.character(stage[,1])
     stage[,1] <- substr(stage[,1],1,nchar(stage[,1])-3)
     stage[,1] <- as.POSIXct(stage[,1], format = "%Y-%m-%d %H:%M:%S")
     
     # Convert the flow and stage to numeric
     flow[,2] <- as.numeric(gsub(",", "", flow[,2]))
     stage[,2] <- as.numeric(gsub(",", "", stage[,2]))
     
     names(flow) <- c("DateTime", "Flow")
     names(stage) <- c("DateTime", "Stage")
     
     # Merge flow and stage into one data frame
     df <- merge(flow, stage, by = "DateTime", all = T)
     
     # Rename the headers
     names(df) <- c("DateTime", "Flow", "Stage")
     
     return(df)
     
}