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

CONSUMPTION <- function(df, scenario, scaler, slope){
     
     # Format month as character
     scenario$Month <- as.character(scenario$Month)
     
     # Heading titles
     heading1 <- paste0(colnames(scenario)[2],"Consumption")
     heading2 <- paste0(colnames(scenario)[2],"Flow")
     heading3 <- paste0(colnames(scenario)[2],"Stage")
     
     # Calculate consumption based on patterns
     df[,heading1] <- scenario[,2][match(df$Month, scenario$Month)]
     
     # Scale consumption
     df[,heading1] <- df[,heading1]*scaler
     
     # Subtract consumption from flow
     df[,heading2] <- df$Flow - df[,heading1]
     
     # Calculate new stage
     df[,heading3] <- df$Stage-(df[,heading1]*slope)
     
     return(df)
}