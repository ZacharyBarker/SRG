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
     
     # Get list of scenarios to plot
     scenarios <- colnames(df)[grepl("Flow", names(df))]
     
     # Apply to each flow scenario
     for(i in scenarios){
          
          # Headings
          heading1 <- paste0(i,"Rank")
          heading2 <- paste0(i, "Exceedence")
     
          # Sort flows
          df <- df[order(df[,i]),]
          df[,heading1] <- order(df[,i])
          df[,heading2] = 100-(100*(df[,heading1]/(length(df[,heading1])+1)))
          
     }
     
     # Subset to only graphing parameters
     exceedence <- df[grepl("Exceedence", names(df))]
     flow <- df[scenarios]
     
     # Reshape to plot
     exceedence <- melt(exceedence)
     flow <- melt(flow)
     dd <- data.frame(flow, exceedence)
     
     # Plot
     curve <- ggplot(dd) + geom_line(aes(x=value.1, y=value, colour=variable)) +
          # facet_grid(.~variable) +
          scale_y_log10()+
          theme_bw()+
          xlab("Exceedence Probability (%)")+
          ylab("Flow  (CFS) - Log Scale")+
          ggtitle(paste(guage_name, "Flow Duration Curve"))+
          theme(legend.justification=c(1,1), 
                legend.position=c(1,0),
                legend.title=element_blank(), 
                legend.background = element_rect(fill="transparent"),
                plot.title = element_text(size = rel(2)),
                axis.text = element_text(size = rel(1.2)),
                axis.title = element_text(size = rel(1.5)),
                legend.text = element_text(size = rel(1.5)))
     
     print(curve)

}
