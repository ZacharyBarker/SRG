################################################################################
#
# Revenue reduction due to decreased stream flow
#
# Zac Barker, February 2017
#
################################################################################

COST <- function(df, immersion, scenario, price, criticalLevel) {
     
     # Heading titles
     heading1 <- paste0(colnames(scenario)[2],"Stage")
     heading2 <- paste0(colnames(scenario)[2],"Cost")
     
     # Calculation of cost depends on critical level
     if (df[,heading1] >= criticalLevel) {
          df[,heading2] <- 0
     } else if (df[,heading1] < criticalLevel) {
          deltaL <- criticalLevel - df[,heading1]
          df[,heading2] <- deltaL * immersion * price
     } else {
          df[,heading2] <- NULL
     }
  
     return(df)
}