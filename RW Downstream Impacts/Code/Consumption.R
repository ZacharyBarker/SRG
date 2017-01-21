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

CONSUMPTION <- function(df, scenario, scaler){
     # CONSUMPTION <- function(df, Patterns, Type, Scaler){
     
     scenario$Month <- as.character(scenario$Month)
     
     # Calculate consumption based on patterns
     df$Consumption <- scenario[,2][match(df$Month, scenario$Month)]
     
     # Scale consumption
     df$Consumption <- df$Consumption*scaler
     
     # Subtract consumption from flow
     df$ConsumptionFlow <- df$Flow - df$Consumption
     
     return(df)
}