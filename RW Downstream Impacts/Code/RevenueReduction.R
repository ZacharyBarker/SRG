################################################################################
#
# Revenue reduction due to decreased stream flow
#
# Zac Barker, February 2017
#
################################################################################

COST <- function(df, immersion, price) {
     
     # Creates a new column for revenue lost
          
          # For each time step (row):
          
          # Check if stage is lower than critical level
          
               # if not lower 
                    # revenue lost = 0
          
               # if is lower:
                    # (critical level - stage) * immersion factor * price
  
     return(df)
}