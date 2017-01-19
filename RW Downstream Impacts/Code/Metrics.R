################################################################################
#
# Statistical metrics used for assesing the upstream consumption
#
# Zac Barker, December 2016
#
################################################################################

# T test
T_TEST <- function(df){
     testResults = t.test(df$Flow,df$ConsumptionFlow,var.equal = TRUE)
     tStat = testResults[["statistic"]][[1]] 
     return(tStat)
}


# Probability of failure
P_FAIL <- function(df, slope, threshold){
     df$ConsumptionStage = df$Stage-(df$Consumption*slope)
     pFail = (length(which(df$ConsumptionStage<threshold))/length(df$Stage))*100
     return(pFail)
}