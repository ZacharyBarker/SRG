################################################################################
#
# Statistical metrics used for assesing the upstream consumption
#
# Zac Barker, December 2016
#
################################################################################

# T test
T_TEST <- function(df, name){
     
     # Create dataframe from columns needed
     consumption <- paste0(name,"Consumption")
     flow2 <- paste0(name,"Flow")
     df2 <- data.frame(df$Flow, df[,flow2], df[,consumption])
     names(df2) <- c("Flow", "Flow2", "Consumption")
     
     # Subset based on when there is actually consumption
     df2 <- df2[df2$Consumption > 0,]
     
     # Perform t test
     if(nrow(df2) > 0){
          testResults <- t.test(df2$Flow,df2$Flow2,var.equal = TRUE)
          tStat <- testResults[["statistic"]][[1]] 
     } else {
          tStat <- 0
     }
     return(tStat)
}


# Probability of failure
P_FAIL <- function(df, slope, threshold, name){
     
     # Create dataframe from columns needed
     heading1 <- paste0(name, "Consumption")
     heading2 <- paste0(name, "Stage")
     df2 <- data.frame(df[,heading1], df[,heading2])
     names(df2) <- c("Consumption", "Stage")
     
     # Subset based on when there is actually consumption
     df2 <- df2[df2$Consumption > 0,]
     
     # Remove NAs
     stageWOna <- df2$Stage[!is.na(df2$Stage)]
     
     # Calculate probability of failure
     if(nrow(df2) > 0){
          countFail <- length(which(stageWOna<threshold))
          countLength <- length(stageWOna)
          pFailOut <- (countFail/countLength)*100
     } else {
          pFailOut <- 0
     }
     return(pFailOut)
}

# Total revenue lost for each scenario
COST <- function(df, name, quant) {
     heading1 <- paste0(name, quant)
     total <- sum(df[,heading1], na.rm = T)
     return(total/(length(df[,heading1])/365.25))
}

# Plot t test
PLOT_TTEST <- function(df){
     
     # Remove current 
     dd <- df[ , !(names(df) %in% c("Current"))]
     
     # Add MGD to scaler
     dd$Scaler <- paste(dd$Scaler,"MGD")
     
     # Reshape to plot
     dd <- melt(dd, id=c("Gauge", "Station", "Scaler"))
     
     # Make the Scaler a factor to avoid sorting
     dd$Scaler <- factor(dd$Scaler, levels = dd$Scaler)
     
     # t Stat threshold
     x <- c(0, 0)
     y <- c(1.96, 1.96)
     threshold <- data.frame(x, y)
     
     # Plot
     p <- ggplot(dd) + geom_line(aes(x=Station, y=value, colour=variable), size=1) +
          facet_grid(.~Scaler)+
          geom_hline(data = threshold, aes(yintercept=y), linetype = "dashed", size=1)+
          theme_bw()+
          xlab("Station (River Miles from Mississippi River)")+
          ylab("t-statistic")+
          ggtitle("Maximum consumption per day")+
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
     
     # Add MGD to scaler
     dd$Scaler <- paste(dd$Scaler,"MGD")
     
     # Make the Scaler a factor to avoid sorting
     dd$Scaler <- factor(dd$Scaler, levels = dd$Scaler)
     
     # Reshape to plot
     dd <- melt(dd, id=c("Gauge", "Station", "Scaler"))
     
     # Plot
     p <- ggplot(dd, aes(x = factor(Gauge), y = value)) + geom_bar(stat = "identity") +
     # p <- ggplot(dd) + geom_line(aes(x=Station, y=value, colour=variable), size=1) +
          facet_grid(variable~Scaler)+
          theme_bw()+
          xlab("Gauge")+
          # xlab("Station (River Miles from Mississippi River)")+
          ylab("Increase in probability of failure (%)")+
          ggtitle("Maximum consumption per day")+
          theme(legend.justification=c(0,1), 
                legend.position=c(0,1),
                legend.title=element_blank(), 
                legend.background = element_rect(fill="transparent"),
                plot.title = element_text(size = rel(1.4)),
                axis.text.x = element_text(angle = 90, hjust = 1),
                axis.text = element_text(size = rel(1.2)),
                axis.title = element_text(size = rel(1.5)),
                legend.text = element_text(size = rel(1.5)))
     
     print(p)
}

# Plot revenue lost
PLOT_RLOST <- function(df, min, max){
     
     # Remove current 
     dd <- df[ , !(names(df) %in% c("Current"))]
     min <- min[ , !(names(min) %in% c("Current"))]
     max <- max[ , !(names(max) %in% c("Current"))]
     
     # Add MGD to scaler
     dd$Scaler <- paste(dd$Scaler,"MGD")
     
     # Make the Scaler a factor to avoid sorting
     dd$Scaler <- factor(dd$Scaler, levels = dd$Scaler)
     
     # Reshape to plot
     dd <- melt(dd, id=c("Gauge", "Station", "Scaler"))
     
     # Add min and max to dd
     min <- melt(min, id=c("Gauge", "Station", "Scaler"))
     max <- melt(max, id=c("Gauge", "Station", "Scaler"))
     dd$min <- min$value
     dd$max <- max$value
     
     # Convert to 1000 of $
     dd$value <- dd$value/1000
     dd$min <- dd$min/1000
     dd$max <- dd$max/1000
     
     # Plot
     p <- ggplot(dd, aes(x = factor(Gauge), y = value)) + geom_bar(stat = "identity") +
          geom_errorbar(aes(x = factor(Gauge), ymin = min, ymax = max), stat = "identity", size = .75, width=0.25) + 
          facet_grid(variable~Scaler)+
          theme_bw()+
          xlab("Gauge")+
          ylab("Value lost (thousand $/year)")+
          ggtitle("Maximum consumption per day")+
          scale_y_continuous(labels = scales::dollar)+
          theme(legend.justification=c(0,1), 
                legend.position=c(0,1),
                legend.title=element_blank(), 
                legend.background = element_rect(fill="transparent"),
                plot.title = element_text(size = rel(1.5)),
                axis.text = element_text(size = rel(1.2)),
                axis.text.x = element_text(angle = 90, hjust = 1),
                axis.title = element_text(size = rel(1.5)),
                legend.text = element_text(size = rel(1.5)))
     
     print(p)
}

# Plot revenue lost
PLOT_NETRLOST <- function(df, min, max){
     
     # Add MGD to scaler
     df$Scaler <- paste(df$Scaler,"MGD")
     
     # Make the Scaler a factor to avoid sorting
     df$Scaler <- factor(df$Scaler, levels = df$Scaler)
     
     # Calculate the net loss using the current scenario as baseline
     scenarios <- ncol(df)
     for(scenario in 5:scenarios){
          df[,scenario] <- df[,scenario] - df[,"Current"]
          min[,scenario] <- min[,scenario] - min[,"Current"]
          max[,scenario] <- max[,scenario] - max[,"Current"]
     }
     df$Current <- 0
     df2 <- df
     
     # Remove current 
     dd <- df[ , !(names(df) %in% c("Current"))]
     min <- min[ , !(names(min) %in% c("Current"))]
     max <- max[ , !(names(max) %in% c("Current"))]
     
     # Reshape to plot
     dd <- melt(dd, id=c("Gauge", "Station", "Scaler"))
     
     # Add min and max to dd
     min <- melt(min, id=c("Gauge", "Station", "Scaler"))
     max <- melt(max, id=c("Gauge", "Station", "Scaler"))
     dd$min <- min$value
     dd$max <- max$value
     
     # Convert to 1000 of $
     dd$value <- dd$value/1000
     dd$min <- dd$min/1000
     dd$max <- dd$max/1000
     
     # Plot
     p <- ggplot(dd, aes(x = factor(Gauge), y = value)) + geom_bar(stat = "identity") +
          geom_errorbar(aes(x = factor(Gauge), ymin = min, ymax = max), stat = "identity", size = .75, width=0.25) + 
          facet_grid(variable~Scaler)+
          theme_bw()+
          xlab("Gauge")+
          ylab("Increase of value lost (thousand $/year)")+
          ggtitle("Maximum consumption per day")+
          scale_y_continuous(labels = scales::dollar)+
          theme(legend.justification=c(0,1), 
                legend.position=c(0,1),
                legend.title=element_blank(), 
                legend.background = element_rect(fill="transparent"),
                plot.title = element_text(size = rel(1.5)),
                axis.text = element_text(size = rel(1.2)),
                axis.text.x = element_text(angle = 90, hjust = 1),
                axis.title = element_text(size = rel(1.5)),
                legend.text = element_text(size = rel(1.5)))
     
     print(p)
     return(df2)
}
