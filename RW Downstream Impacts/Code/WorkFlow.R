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
source("Code/Consumption.R")
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
Dresden <- CONSUMPTION(Dresden, Patterns, "Summer", Scaler)
Marseilles <- CONSUMPTION(Marseilles, Patterns, "Winter", Scaler)
StarvedRock <- CONSUMPTION(StarvedRock, Patterns, "Winter", Scaler)
Peoria <- CONSUMPTION(Peoria, Patterns, "Winter", Scaler)
LaGrange <- CONSUMPTION(LaGrange, Patterns, "Winter", Scaler)


# Flow duration curves
Dresden <- FDC("Dresden", Dresden)
Marseilles <- FDC("Marseilles", Marseilles)
StarvedRock <- FDC("Starved Rock", StarvedRock)
Peoria <- FDC("Peoria", Peoria)
LaGrange <- FDC("La Grange", LaGrange)


# Rating curve
s_Dresden <- RATING_CURVE("Dresden", Dresden)
s_Marseilles <- RATING_CURVE("Marseilles", Marseilles)
s_StarvedRock <- RATING_CURVE("Starved Rock", StarvedRock)
s_Peoria <- RATING_CURVE("Peoria", Peoria)
s_LaGrange <- RATING_CURVE("La Grange", LaGrange)


# T test
t_Dresden <- T_TEST(Dresden)

# Probability of failure
pf_Dresden <- P_FAIL(Dresden, s_Dresden, 482.8)

