################################################################################
#
# Calculates the hypothetical downstream flow given historical data and a 
#    consumption pattern.
#
# Zac Barker, December 2016
#
################################################################################

CONSUMPTION <- function(df, Consumption, Scaler){
     
     # Scale consumption patterns
     for(i in 2:ncol(Consumption)){
          Consumption[,i]<- Consumption[,i]*Scaler
     }
     
     # Calculate consumption based on patterns
     
     
     return(df)
}

# For testing
df <- CONSUMPTION(Dresden, Patterns, Scaler)