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
     heading1 <- paste0(name, "Flow")
     heading2 <- paste0(name, "Stage")
     df[,heading2] <- df$Stage-(df[,heading1]*slope)
     pFailOut <- (length(which(df[,heading2]<threshold))/length(df$Stage))*100
     return(pFailOut)
}


# Plot 