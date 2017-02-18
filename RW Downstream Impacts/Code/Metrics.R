################################################################################
#
# Statistical metrics used for assesing the upstream consumption
#
# Zac Barker, December 2016
#
################################################################################

# T test
T_TEST <- function(df, name){
     heading <- paste0(name,"Flow")
     testResults <- t.test(df$Flow,df[,heading],var.equal = TRUE)
     tStat <- testResults[["statistic"]][[1]] 
     return(tStat)
}


# Probability of failure
P_FAIL <- function(df, slope, threshold, name){
     heading1 <- paste0(name, "Consumption")
     heading2 <- paste0(name, "Stage")
     stageWOna <- df[,heading2][!is.na(df[,heading2])]
     countFail <- length(which(stageWOna<threshold))
     countLength <- length(stageWOna)
     pFailOut <- (countFail/countLength)*100
     return(pFailOut)
}


# Plot 
