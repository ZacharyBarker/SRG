################################################################################
#
# Work flow for RW downstream analysis
#
# Zac Barker, December 2016
#
################################################################################

# Load necessary libraries
library("gdata")                                                                # gdata must have perl library installed

     
# Set working directory to main RW Downstream impacts folder
# setwd("C:/Users/Zachary/Desktop/SRG/RW Downstream Impacts")


# Load user defined functions
source("Code/CleanAndFormat.R")
source("Code/FlowDurationCurve.R")
source("Code/RatingCurve.R")

# Clean & format data
setwd("Data/Raw Data/")
Dresden <- CLEAN("Dresden_Flow.xls", "Dresden_Stage.xls", 12)
Marseilles <- CLEAN("Marseilles_Flow.xls", "Marseilles_Stage.xls", 17)
StarvedRock <- CLEAN("StarvedRock_Flow.xls", "StarvedRock_Stage.xls", 21)
Peoria <- CLEAN("Peoria_Flow.xls", "Peoria_Stage.xls", 19)
LaGrange <- CLEAN("LaGrange_Flow.xls", "LaGrange_Stage.xls", 21)
LaGrange$Stage <- sapply(LaGrange$Stage, function(x){                           # Guage datum switched during the record
     if(x<=100 && !is.na(x)){
          x+413.5
     }else{x}
     })
setwd("../..")


# Flow duration curves
Consumption <- 1000
FDC("Dresden", Dresden, Consumption)
FDC("Marseilles", Marseilles, Consumption)
FDC("Starved Rock", StarvedRock, Consumption)
FDC("Peoria", Peoria, Consumption)
FDC("La Grange", LaGrange, Consumption)


# Rating curve
RATING_CURVE("Dresden", Dresden)
RATING_CURVE("Marseilles", Marseilles)
RATING_CURVE("Starved Rock", StarvedRock)
RATING_CURVE("Peoria", Peoria)
RATING_CURVE("La Grange", LaGrange)


# T test



