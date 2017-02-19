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

# Total revenue lost for each scenario
COST <- function(df, name) {
     heading1 <- paste0(name, "Cost")
     total <- sum(df[,heading1], na.rm = T)
     return(total/(length(df[,heading1])/365.25))
}

# Plot t test
PLOT_TTEST <- function(df){
     
     # Remove current 
     dd <- df[ , !(names(df) %in% c("Current"))]
     
     # Reshape to plot
     dd <- melt(dd, id=c("Gauge", "Station", "Scaler"))
     
     # Plot
     p <- ggplot(dd) + geom_line(aes(x=Station, y=value, colour=variable), size=1) +
          facet_grid(.~Scaler)+
          theme_bw()+
          xlab("Station (River Miles from Mississippi River)")+
          ylab("t-statistic")+
          ggtitle("Additional consumption average per day (MGD)")+
          theme(legend.justification=c(0,1), 
                legend.position=c(0,1),
                legend.title=element_blank(), 
                legend.background = element_rect(fill="transparent"),
                plot.title = element_text(size = rel(1.5)),
                axis.text = element_text(size = rel(1.2)),
                axis.title = element_text(size = rel(1.5)),
                legend.text = element_text(size = rel(1.5)))
     
     print(p)
}

# Plot probability of failure
PLOT_PFAIL <- function(df){
     
     # Remove current 
     dd <- df[ , !(names(df) %in% c("Current"))]
     
     # Reshape to plot
     dd <- melt(dd, id=c("Gauge", "Station", "Scaler"))
     
     # Plot
     p <- ggplot(dd) + geom_line(aes(x=Station, y=value, colour=variable), size=1) +
          facet_grid(.~Scaler)+
          theme_bw()+
          xlab("Station (River Miles from Mississippi River)")+
          ylab("Probability of failure (%)")+
          ggtitle("Additional consumption average per day (MGD)")+
          theme(legend.justification=c(0,1), 
                legend.position=c(0,1),
                legend.title=element_blank(), 
                legend.background = element_rect(fill="transparent"),
                plot.title = element_text(size = rel(1.5)),
                axis.text = element_text(size = rel(1.2)),
                axis.title = element_text(size = rel(1.5)),
                legend.text = element_text(size = rel(1.5)))
     
     print(p)
}

# Plot revenue lost
PLOT_RLOST <- function(df){
     
     # Remove current 
     dd <- df[ , !(names(df) %in% c("Current"))]
     
     # Reshape to plot
     dd <- melt(dd, id=c("Gauge", "Station", "Scaler"))
     
     # Plot
     p <- ggplot(dd) + geom_line(aes(x=Station, y=value, colour=variable), size=1) +
          facet_grid(.~Scaler)+
          theme_bw()+
          xlab("Station (River Miles from Mississippi River)")+
          ylab("Revenue lost ($)")+
          ggtitle("Additional consumption average per day (MGD)")+
          theme(legend.justification=c(0,1), 
                legend.position=c(0,1),
                legend.title=element_blank(), 
                legend.background = element_rect(fill="transparent"),
                plot.title = element_text(size = rel(1.5)),
                axis.text = element_text(size = rel(1.2)),
                axis.title = element_text(size = rel(1.5)),
                legend.text = element_text(size = rel(1.5)))
     
     print(p)
}
