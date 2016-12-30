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

FDC <- function(guage_name, df, consumption) {
     
     # Sort flows
     df <- df[order(df$Flow),]
     df$Rank <- order(df$Flow)
     df$Exceedence = 100-(100*(df$Rank/(length(df$Rank)+1)))
     
     # Subset to only graphing parameters
     df2 <- df[,c(2,4,6)]
     
     # Reshape to plot
     dd = melt(df2, id=c("Exceedence"))
     
     # Plot
     curve <- ggplot(dd) + geom_line(aes(x=Exceedence, y=value, colour=variable)) +
          scale_colour_manual(values=c("blue","red"), labels=c("Historical", "With Consumption"))+
          scale_y_log10()+
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