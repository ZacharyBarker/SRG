################################################################################
#
# Calculates the hypothetical downstream flow given historical data and a 
#    consumption pattern.
#
# df = data frame of guage
# Patterns = data frame dictionary of upstream consumption patterns
# Type = String of which pattern to use
# Scaler = Numeric on which to scale the pattern
#
# Zac Barker, December 2016
#
################################################################################

CONSUMPTION <- function(df, Patterns, Type, Scaler){
     
     Patterns$Month <- as.character(Patterns$Month)
     
     # Calculate consumption based on patterns
     df$Consumption <- Patterns[,Type][match(df$Month, Patterns$Month)]
     
     # Scale consumption
     df$Consumption <- df$Consumption*Scaler
     
     # Subtract consumption from flow
     df$ConsumptionFlow <- df$Flow - df$Consumption
     
     return(df)
}
