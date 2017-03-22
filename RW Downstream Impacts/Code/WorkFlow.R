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
# setwd("C:/Users/Zachary/Desktop/SRG/RW Downstream Impacts")                   # personal machine
setwd("C:/Users/Zachary.Barker/Desktop/SRG/RW Downstream Impacts")              # work machine


# Load user defined functions
source("Code/CleanAndFormat.R")
source("Code/Consumption.R")
source("Code/FlowDurationCurve.R")
source("Code/RatingCurve.R")
source("Code/Metrics.R")
source("Code/Patterns.R")


# Initialize metrics output dataframes
Gauge <- c("Dresden", "Marseilles", "Starved Rock", "Peoria", "La Grange")
Station <- c(271, 245, 231, 158, 80)
tTestTemp <- data.frame(Gauge, Station)
pFailTemp <- data.frame(Gauge, Station)
rLostTemp <- data.frame(Gauge, Station)
rMaxTemp <- data.frame(Gauge, Station)
rMinTemp <- data.frame(Gauge, Station)


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
Scalers <- c(0, 200, 1000, 2000)
PLOT_PATTERNS(Patterns)

# Load values at each gauge
Value <- read.csv("Data/ValuePerTon.csv")

## TEMPORARY VARIABLE INITALIZATION
I = 5
price = 620

# Get price for each gauge
v_Dresden <- c(Value[which(Value$Lock == "Dresden"),]$Average, 
               Value[which(Value$Lock == "Dresden"),]$Max, 
               Value[which(Value$Lock == "Dresden"),]$Min)
v_Marseilles <- c(Value[which(Value$Lock == "Marseilles"),]$Average, 
                  Value[which(Value$Lock == "Marseilles"),]$Max, 
                  Value[which(Value$Lock == "Marseilles"),]$Min)
v_StarvedRock <- c(Value[which(Value$Lock == "Starved Rock"),]$Average, 
                   Value[which(Value$Lock == "Starved Rock"),]$Max, 
                   Value[which(Value$Lock == "Starved Rock"),]$Min)
v_Peoria <- c(Value[which(Value$Lock == "Peoria"),]$Average, 
              Value[which(Value$Lock == "Peoria"),]$Max, 
              Value[which(Value$Lock == "Peoria"),]$Min)
v_LaGrange <- c(Value[which(Value$Lock == "La Grange"),]$Average, 
                Value[which(Value$Lock == "La Grange"),]$Max, 
                Value[which(Value$Lock == "La Grange"),]$Min)

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
          Dresden <- CONSUMPTION(Dresden, Scenario, Scaler, s_Dresden, 482.8, I, v_Dresden)
          Marseilles <- CONSUMPTION(Marseilles, Scenario, Scaler, s_Marseilles, 458.5, I, v_Marseilles)
          StarvedRock <- CONSUMPTION(StarvedRock, Scenario, Scaler, s_StarvedRock, 440.3, I, v_StarvedRock)
          Peoria <- CONSUMPTION(Peoria, Scenario, Scaler, s_Peoria, 430.0, I, v_Peoria)
          LaGrange <- CONSUMPTION(LaGrange, Scenario, Scaler, s_LaGrange, 419.6, I, v_LaGrange)
          
          
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
          
          
          # Avg value lost
          c_Dresden <- COST(Dresden, Name, "AvgValue")
          c_Marseilles <- COST(Marseilles, Name, "AvgValue")
          c_StarvedRock <- COST(StarvedRock, Name, "AvgValue")
          c_Peoria <- COST(Peoria, Name, "AvgValue")
          c_LaGrange <- COST(LaGrange, Name, "AvgValue")
          
          # Max value lost
          cMax_Dresden <- COST(Dresden, Name, "MaxValue")
          cMax_Marseilles <- COST(Marseilles, Name, "MaxValue")
          cMax_StarvedRock <- COST(StarvedRock, Name, "MaxValue")
          cMax_Peoria <- COST(Peoria, Name, "MaxValue")
          cMax_LaGrange <- COST(LaGrange, Name, "MaxValue")
          
          # Min value lost
          cMin_Dresden <- COST(Dresden, Name, "MinValue")
          cMin_Marseilles <- COST(Marseilles, Name, "MinValue")
          cMin_StarvedRock <- COST(StarvedRock, Name, "MinValue")
          cMin_Peoria <- COST(Peoria, Name, "MinValue")
          cMin_LaGrange <- COST(LaGrange, Name, "MinValue")
          
          # Metrics output temporary dataframes
          tTestTemp$Scaler <- Scaler
          tTestTemp[,colnames(Patterns)[i]] <- c(t_Dresden,t_Marseilles,t_StarvedRock,t_Peoria,t_LaGrange)
          pFailTemp$Scaler <- Scaler
          pFailTemp[,colnames(Patterns)[i]] <- c(pf_Dresden,pf_Marseilles,pf_StarvedRock,pf_Peoria,pf_LaGrange)
          rLostTemp$Scaler <- Scaler
          rLostTemp[,colnames(Patterns)[i]] <- c(c_Dresden,c_Marseilles,c_StarvedRock,c_Peoria,c_LaGrange)
          rMaxTemp$Scaler <- Scaler
          rMaxTemp[,colnames(Patterns)[i]] <- c(cMax_Dresden,cMax_Marseilles,cMax_StarvedRock,cMax_Peoria,cMax_LaGrange)
          rMinTemp$Scaler <- Scaler
          rMinTemp[,colnames(Patterns)[i]] <- c(cMin_Dresden,cMin_Marseilles,cMin_StarvedRock,cMin_Peoria,cMin_LaGrange)
     }
     
     #Populates the master output dataframes with that scalers runs
     if(exists("tTest")) {
          tTest <- rbind(tTest, tTestTemp)
          pFail <- rbind(pFail, pFailTemp)
          rLost <- rbind(rLost, rLostTemp)
          rMax <- rbind(rMax, rMaxTemp)
          rMin <- rbind(rMin, rMinTemp)
          
     } else {
          tTest <- tTestTemp
          pFail <- pFailTemp
          rLost <- rLostTemp
          rMax <- rMaxTemp
          rMin <- rMinTemp
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
PLOT_RLOST(rLost, rMin, rMax)
rNetLost <- PLOT_NETRLOST(rLost, rMin, rMax)