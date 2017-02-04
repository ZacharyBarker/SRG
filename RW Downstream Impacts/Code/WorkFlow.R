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
source("Code/Metrics.R")
source("Code/Patterns.R")


# Initialize metrics output dataframes
Gauge <- c("Dresden", "Marseilles", "StarvedRock", "Peoria", "LaGrange")
Station <- c(271, 245, 231, 158, 80)
tTest <- data.frame(Gauge, Station)
pFail <- data.frame(Gauge, Station)


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


# Rating curve
s_Dresden <- RATING_CURVE("Dresden", Dresden)
s_Marseilles <- RATING_CURVE("Marseilles", Marseilles)
s_StarvedRock <- RATING_CURVE("Starved Rock", StarvedRock)
s_Peoria <- RATING_CURVE("Peoria", Peoria)
s_LaGrange <- RATING_CURVE("La Grange", LaGrange)


# Load consumption patterns
Patterns <- read.csv("Data/ConsumptionPatterns.csv", header = T)
Scaler <- 200
PLOT_PATTERNS(Patterns)


# Loop through consumption scenarios
for(i in 2:ncol(Patterns)){

     # Isolate a scenario
     Scenario <- data.frame(Patterns$Month, Patterns[,i])
     Name <- colnames(Patterns)[i]
     colnames(Scenario) <- c("Month", Name)
     
     
     # Calculate consumption scenario
     Dresden <- CONSUMPTION(Dresden, Scenario, Scaler)
     Marseilles <- CONSUMPTION(Marseilles, Scenario, Scaler)
     StarvedRock <- CONSUMPTION(StarvedRock, Scenario, Scaler)
     Peoria <- CONSUMPTION(Peoria, Scenario, Scaler)
     LaGrange <- CONSUMPTION(LaGrange, Scenario, Scaler)
     
     
     # T test
     t_Dresden <- T_TEST(Dresden, Name)
     t_Marseilles <- T_TEST(Marseilles, Name)
     t_StarvedRock <- T_TEST(StarvedRock, Name)
     t_Peoria <- T_TEST(Peoria, Name)
     t_LaGrange <- T_TEST(LaGrange, Name)
     
     
     # Probability of failure
     pf_Dresden <- P_FAIL(Dresden, s_Dresden, 482.8, Name)
     pf_Marseilles <- P_FAIL(Marseilles, s_Marseilles, 458.5, Name)
     pf_StarvedRock <- P_FAIL(StarvedRock, s_StarvedRock, 440.3, Name)
     pf_Peoria <- P_FAIL(Peoria, s_Peoria, 430.0, Name)
     pf_LaGrange <- P_FAIL(LaGrange, s_LaGrange, 419.6, Name)
     
     
     # Metrics output dataframes
     tTest[,colnames(Patterns)[i]] <- c(t_Dresden,t_Marseilles,t_StarvedRock,t_Peoria,t_LaGrange)
     pFail[,colnames(Patterns)[i]] <- c(pf_Dresden,pf_Marseilles,pf_StarvedRock,pf_Peoria,pf_LaGrange)

}


# Plot flow duration curves
FDC("Dresden", Dresden)
FDC("Marseilles", Marseilles)
FDC("Starved Rock", StarvedRock)
FDC("Peoria", Peoria)
FDC("La Grange", LaGrange)