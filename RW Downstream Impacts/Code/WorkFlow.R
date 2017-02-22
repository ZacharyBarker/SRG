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
tTestTemp <- data.frame(Gauge, Station)
pFailTemp <- data.frame(Gauge, Station)
rLostTemp <- data.frame(Gauge, Station)


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
Scalers <- c(0, 200, 500, 1000, 1500, 2000)
PLOT_PATTERNS(Patterns)

## TEMPORARY VARIABLE INITALIZATION
I = 5
# price = c(1800, 2000, 2200)
price = 2000

# Loop through consumption scaler
for(Scaler in Scalers) {

     # Loop through consumption scenarios
     for(i in 2:ncol(Patterns)){
     
          # Isolate a scenario
          Scenario <- data.frame(Patterns$Month, Patterns[,i])
          if(colnames(Patterns)[i] == "Current"){
               Name <- "Current"
          } else {
               Name <- paste0(colnames(Patterns)[i],Scaler)
          }
          colnames(Scenario) <- c("Month", Name)
          
          
          # Calculate consumption scenario
          Dresden <- CONSUMPTION(Dresden, Scenario, Scaler, s_Dresden, 482.8, I, price)
          Marseilles <- CONSUMPTION(Marseilles, Scenario, Scaler, s_Marseilles, 458.5, I, price)
          StarvedRock <- CONSUMPTION(StarvedRock, Scenario, Scaler, s_StarvedRock, 440.3, I, price)
          Peoria <- CONSUMPTION(Peoria, Scenario, Scaler, s_Peoria, 430.0, I, price)
          LaGrange <- CONSUMPTION(LaGrange, Scenario, Scaler, s_LaGrange, 419.6, I, price)
          
          
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
          
          
          # Revenue lost
          c_Dresden <- COST(Dresden, Name)
          c_Marseilles <- COST(Marseilles, Name)
          c_StarvedRock <- COST(StarvedRock, Name)
          c_Peoria <- COST(Peoria, Name)
          c_LaGrange <- COST(LaGrange, Name)
          
          
          # Metrics output temporary dataframes
          tTestTemp$Scaler <- Scaler
          tTestTemp[,colnames(Patterns)[i]] <- c(t_Dresden,t_Marseilles,t_StarvedRock,t_Peoria,t_LaGrange)
          pFailTemp$Scaler <- Scaler
          pFailTemp[,colnames(Patterns)[i]] <- c(pf_Dresden,pf_Marseilles,pf_StarvedRock,pf_Peoria,pf_LaGrange)
          rLostTemp$Scaler <- Scaler
          rLostTemp[,colnames(Patterns)[i]] <- c(c_Dresden,c_Marseilles,c_StarvedRock,c_Peoria,c_LaGrange)
     }
     
     #Populates the master output dataframes with that scalers runs
     if(exists("tTest")) {
          tTest <- rbind(tTest, tTestTemp)
          pFail <- rbind(pFail, pFailTemp)
          rLost <- rbind(rLost, rLostTemp)
          
     } else {
          tTest <- tTestTemp
          pFail <- pFailTemp
          rLost <- rLostTemp
     }
     
}


# Plot flow duration curves
FDC("Dresden", Dresden)
FDC("Marseilles", Marseilles)
FDC("Starved Rock", StarvedRock)
FDC("Peoria", Peoria)
FDC("La Grange", LaGrange)

# Plot metrics plot
PLOT_TTEST(tTest)
PLOT_PFAIL(pFail)
PLOT_RLOST(rLost)