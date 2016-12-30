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
setwd("C:/Users/Zachary/Desktop/SRG/RW Downstream Impacts")


# Load user defined functions
source("Code/CleanAndFormat.R")
source("Code/FlowDurationCurve.R")
source("Code/RatingCurve.R")


# Load, clean & format data
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


# Load consumption patterns
Patterns <- read.csv("Data/ConsumptionPatterns.csv", header = T)
Scaler <- 1000


# Calculate consumption scenario
Dresden <- CONSUMPTION(Dresden, ConsumptionPattern, Scaler)


# Flow duration curves
Dresden <- FDC("Dresden", Dresden, Consumption)
Marseilles <- FDC("Marseilles", Marseilles, Consumption)
StarvedRock <- FDC("Starved Rock", StarvedRock, Consumption)
Peoria <- FDC("Peoria", Peoria, Consumption)
LaGrange <- FDC("La Grange", LaGrange, Consumption)


# Rating curve
si_Dresden <- RATING_CURVE("Dresden", Dresden)
si_Marseilles <- RATING_CURVE("Marseilles", Marseilles)
si_StarvedRock <- RATING_CURVE("Starved Rock", StarvedRock)
si_Peoria <- RATING_CURVE("Peoria", Peoria)
si_LaGrange <- RATING_CURVE("La Grange", LaGrange)


# T test



