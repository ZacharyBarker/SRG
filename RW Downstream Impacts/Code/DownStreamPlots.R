#############################################################
# Plots graphs dealing with downstream impacts of 
# reclaimed water consumption
#
# By Zachary Barker
# October, 2015
#
#############################################################

## Set folder ##
setwd("D:/Illinois Research/RW for PP Cooling/Analysis/Downstream")

Consumption = 200

############################### Read in data ###############################

LaGrange = read.csv("LaGrange2.csv")                                     #Illinois River @ La Grange
Flow_LaGrange = LaGrange[["Flow_cfs"]]
Stage_LaGrange = LaGrange[["Stage_ft"]]
DeFacto_LaGrange = Flow_LaGrange*0.646316889697
RW_LaGrange = DeFacto_LaGrange-Consumption

Peoria = read.csv("Peoria2.csv")                                         #Illinois River @ Peoria
Flow_Peoria = Peoria[["Flow_cfs"]]
Stage_Peoria = Peoria[["Stage_ft"]]
DeFacto_Peoria = Flow_Peoria*0.646316889697
RW_Peoria = DeFacto_Peoria-Consumption

StarvedRock = read.csv("StarvedRock2.csv")                               #Illinois River @ Starved Rock
Flow_StarvedRock = StarvedRock[["Flow_cfs"]]
Stage_StarvedRock = StarvedRock[["Stage_ft"]]
DeFacto_StarvedRock = Flow_StarvedRock*0.646316889697
RW_StarvedRock = DeFacto_StarvedRock-Consumption

Marseilles = read.csv("Marseilles2.csv")                                #Illinois River @ Marseilles
Flow_marseilles = Marseilles[["Flow_cfs"]]
Stage_marseilles = Marseilles[["Stage_ft"]]
DeFacto_marseilles = Flow_marseilles*0.646316889697
RW_marseilles = DeFacto_marseilles-Consumption
RW_marseilles[RW_marseilles<0]=0

Dresden = read.csv("Dresden2.csv")                                       #Illinois River @ Dresden
Flow_Dresden = Dresden[["Flow_cfs"]]
Stage_Dresden = Dresden[["Stage_ft"]]
DeFacto_Dresden = Flow_Dresden*0.646316889697
RW_Dresden = DeFacto_Dresden-Consumption

Kankakee = read.csv("KankakeeRiver2.csv")                                #Kankakee River
DeFacto_kankakee = Kankakee[["De.Facto"]]
RW_kankakee = DeFacto_kankakee-37

CSSC = read.csv("CSSC2.csv")                                            #Chicago Sanitary & Shipping Cannal
DeFacto_cssc = CSSC[["De.Facto"]]
RW_cssc = DeFacto_cssc-Consumption

Stickney = read.csv("StickneyEffluent.csv")                            #Stickney WWTP
DeFacto_stickney = Stickney[["De.facto"]]
RW_stickney = DeFacto_stickney-Consumption

############################ Sorts data for flow duration curve ######################

rank_LaGrange1 = order(DeFacto_LaGrange)                                            #Illinois River @ La Grange
exceed_LaGrange1 = 100*(rank_LaGrange1/(length(DeFacto_LaGrange)+1))
rank_LaGrange2 = order(RW_LaGrange)
exceed_LaGrange2 = 100*(rank_LaGrange2/(length(RW_LaGrange)+1))

rank_Peoria1 = order(DeFacto_Peoria)                                                #Illinois River @ Peoria
exceed_Peoria1 = 100*(rank_Peoria1/(length(DeFacto_Peoria)+1))
rank_Peoria2 = order(RW_Peoria)
exceed_Peoria2 = 100*(rank_Peoria2/(length(RW_Peoria)+1))

rank_StarvedRock1 = order(DeFacto_StarvedRock)                                      #Illinois River @ Starved Rock
exceed_StarvedRock1 = 100*(rank_StarvedRock1/(length(DeFacto_StarvedRock)+1))
rank_StarvedRock2 = order(RW_StarvedRock)
exceed_StarvedRock2 = 100*(rank_StarvedRock2/(length(RW_StarvedRock)+1))

rank_marseilles1 = rank(DeFacto_marseilles,ties.method = "random")                    #Illinois River @ Marseilles
exceed_marseilles1 = 100*(rank_marseilles1/(length(DeFacto_marseilles)+1))
rank_marseilles2 = rank(RW_marseilles,ties.method = "random")
exceed_marseilles2 = 100*(rank_marseilles2/(length(RW_marseilles)+1))

rank_Dresden1 = rank(DeFacto_Dresden,ties.method = "random")                          #Illinois River @ Dresden
exceed_Dresden1 = 100*(rank_Dresden1/(length(DeFacto_Dresden)+1))
rank_Dresden2 = rank(RW_Dresden,ties.method = "random")
exceed_Dresden2 = 100*(rank_Dresden2/(length(RW_Dresden)+1))

rank_kankakee1 = rank(DeFacto_kankakee,ties.method = "random")                        #Kankakee River
exceed_kankakee1 = 100*(rank_kankakee1/(length(DeFacto_kankakee)+1))
rank_kankakee2 = rank(RW_kankakee,ties.method = "random")
exceed_kankakee2 = 100*(rank_kankakee2/(length(RW_kankakee)+1))

rank_cssc1 = rank(DeFacto_cssc,ties.method = "random")                                #Chicago Sanitary Shipping Canal
exceed_cssc1 = 100*(rank_cssc1/(length(DeFacto_cssc)+1))
rank_cssc2 = rank(RW_cssc,ties.method = "random")
exceed_cssc2 = 100*(rank_cssc2/(length(RW_cssc)+1))

rank_stickney1 = rank(DeFacto_stickney,ties.method = "random")                        #Stickney WWTP
exceed_stickney1 = 100*(rank_stickney1/(length(DeFacto_stickney)+1))
rank_stickney2 = rank(RW_stickney,ties.method = "random")
exceed_stickney2 = 100*(rank_stickney2/(length(RW_stickney)+1))

################################ Rating Curves #########################################

plot(DeFacto_LaGrange,Stage_LaGrange,col="blue",xlab="Streamflow (MGD)",                      #Illinois @ La Grange
     ylab="Stage (ft)")
par(new=T)
abline(a=419.8,b=0.000292,col="gray",lwd=2)
abline(a=418.4,b=0.000572,col="black",lwd=2)
abline(419.6,0,lty=3,lwd=2)
title(main="Rating Curve at La Grange")
legend('topleft',
       c("Daily Gauge Data","Linear Trend, All Data","Linear Trend, Lower Half","Minimum Stage"),pch = c(1,NA,NA,NA),
       lty=c(NA,1,1,3),lwd=c(1,2.5,2.5,2.5),col=c("blue","gray","black","black"),bty="n")

plot(DeFacto_Peoria,Stage_Peoria,col="blue",xlab="Streamflow (MGD)",                          #Illinois @ Peoria
     ylab="Stage (ft)")
par(new=T)
abline(a=428.9,b=0.000519,col="black",lwd=2)
abline(430.0,0,lty=3,lwd=2)
title(main="Rating Curve at Peoria")
legend(max(DeFacto_Peoria)*location,
       ((max(Stage_Peoria)-min(Stage_Peoria))*.25)+min(Stage_Peoria),
       c("Daily Gauge Data","Rating Curve","Minimum Stage"),pch = c(1,NA,NA),
       lty=c(NA,1,3),lwd=c(1,2.5,2.5),col=c("blue","black","black"),bty="n")

plot(DeFacto_StarvedRock,Stage_StarvedRock,col="blue",xlab="Streamflow (MGD)",                #Illinois @ Starved Rock
     ylab="Stage (ft)")
par(new=T)
abline(a=440.4,b=0.000487,col="black",lwd=2)
abline(440.3,0,lty=3,lwd=2)
title(main="Rating Curve at Starved Rock")
legend(max(DeFacto_StarvedRock)*location,
       ((max(Stage_StarvedRock)-min(Stage_StarvedRock))*.25)+min(Stage_StarvedRock),
       c("Daily Gauge Data","Rating Curve","Minimum Stage"),pch = c(1,NA,NA),
       lty=c(NA,1,3),lwd=c(1,2.5,2.5),col=c("blue","black","black"),bty="n")

plot(DeFacto_marseilles,Stage_marseilles,col="blue",xlab="Streamflow (MGD)",                  #Illinois @ Marseilles
     ylab="Stage (ft)")
par(new=T)
abline(a=459.1,b=0.000053,col="black",lwd=2)
abline(458.5,0,lty=3,lwd=2)
title(main="Rating Curve at Marseilles")
legend(max(DeFacto_marseilles)*location,
       ((max(Stage_marseilles)-min(Stage_marseilles))*.25)+min(Stage_marseilles),
       c("Daily Gauge Data","Rating Curve","Minimum Stage"),pch = c(1,NA,NA),
       lty=c(NA,1,3),lwd=c(1,2.5,2.5),col=c("blue","black","black"),bty="n")

plot(DeFacto_Dresden,Stage_Dresden,col="blue",xlab="Streamflow (MGD)",                        #Illinois @ Dresden
     ylab="Stage (ft)")
par(new=T)
abline(a=483.3,b=0.000243,col="black",lwd=2)
abline(482.3,0,lty=3,lwd=2)
title(main="Rating Curve at Dresden")
legend(max(DeFacto_Dresden)*location,
       ((max(Stage_Dresden)-min(Stage_Dresden))*.25)+min(Stage_Dresden),
       c("Daily Gauge Data","Rating Curve","Minimum Stage"),pch = c(1,NA,NA),
       lty=c(NA,1,3),lwd=c(1,2.5,2.5),col=c("blue","black","black"),bty="n")




