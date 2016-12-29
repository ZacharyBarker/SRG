################################################################################
#
# Derives the rating curve from flow and stage data
#
# Zac Barker, December 2016
#
################################################################################

library("ggplot2")

RATING_CURVE <- function(guage_name, df){
     
     # Fit linear model
     model = lm(Flow ~ Stage, data = df)
     slope = model$coefficents[2]
     intercept = model$coefficients[1]
     
     curve <- ggplot(df) + geom_point(aes(Stage,Flow))
     
     print(curve)

     return(model)
     
}

rc <- RATING_CURVE("Dresden", Dresden)