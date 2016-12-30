################################################################################
#
# Builds two flow duration curves from a data frame (date, stage, and flow) and 
#    consumption; signifying the shift.
#
# Zac Barker, December 2016
#
################################################################################

library("ggplot2")
library("reshape2")

FDC <- function(guage_name, df) {
     
     # Sort flows
     df <- df[order(df$Flow),]
     df$Rank <- order(df$Flow)
     df$Exceedence = 100-(100*(df$Rank/(length(df$Rank)+1)))
     df <- df[order(df$ConsumptionFlow),]
     df$ConsumptionRank <- order(df$ConsumptionFlow)
     df$ConsumptionExceedence = 100-(100*(df$ConsumptionRank/(length(df$ConsumptionRank)+1)))
     
     # Subset to only graphing parameters
     exceedence <- df[,c(8,10)]
     flow <- df[,c(2,6)]
     
     # Reshape to plot
     exceedence <- melt(exceedence)
     flow <- melt(flow)
     dd <- data.frame(flow, exceedence)
     
     # Plot
     # curve <- ggplot(df)+geom_line(aes(x = ConsumptionExceedence, y = ConsumptionFlow))+geom_line(aes(x = Exceedence, y = Flow))
     
     curve <- ggplot(dd) + geom_line(aes(x=value.1, y=value, colour=variable)) +
          scale_colour_manual(values=c("blue","red"), labels=c("Historical", "With Consumption"))+
          scale_y_log10()+
          theme_bw()+
          xlab("Exceedence Probability (%)")+
          ylab("Flow  (CFS) - Log Scale")+
          ggtitle(paste(guage_name, "Flow Duration Curve"))+
          theme(legend.justification=c(1,1), 
                legend.position=c(1,1), 
                legend.title=element_blank(), 
                legend.background = element_rect(fill="transparent"),
                plot.title = element_text(size = rel(2)),
                axis.text = element_text(size = rel(1.2)),
                axis.title = element_text(size = rel(1.5)),
                legend.text = element_text(size = rel(1.5)))
     
     print(curve)
     
     return(df)
}
