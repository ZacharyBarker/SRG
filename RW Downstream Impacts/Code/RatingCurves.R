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

################################ Rating Curves #########################################

plot(DeFacto_LaGrange,Stage_LaGrange,col="blue",xlab="Streamflow (MGD)",                      #Illinois @ La Grange
     ylab="Stage (ft)")
par(new=T)
abline(a=418.4,b=0.000572,col="black",lwd=2)
abline(419.6,0,lty=3,lwd=2)
title(main="Rating Curve at La Grange")
legend("topleft",c("Daily Gauge Data","Rating Curve","Minimum Stage"),pch = c(1,NA,NA),
       lty=c(NA,1,3),lwd=c(1,2.5,2.5),col=c("blue","black","black"),bty="n")

plot(DeFacto_Peoria,Stage_Peoria,col="blue",xlab="Streamflow (MGD)",                          #Illinois @ Peoria
     ylab="Stage (ft)")
par(new=T)
abline(a=428.8,b=0.000828,col="black",lwd=2)
abline(430.0,0,lty=3,lwd=2)
title(main="Rating Curve at Peoria")
legend("topleft",c("Daily Gauge Data","Rating Curve","Minimum Stage"),pch = c(1,NA,NA),
       lty=c(NA,1,3),lwd=c(1,2.5,2.5),col=c("blue","black","black"),bty="n")

plot(DeFacto_StarvedRock,Stage_StarvedRock,col="blue",xlab="Streamflow (MGD)",                #Illinois @ Starved Rock
     ylab="Stage (ft)")
par(new=T)
abline(a=440.5,b=0.000486,col="black",lwd=2)
abline(440.3,0,lty=3,lwd=2)
title(main="Rating Curve at Starved Rock")
legend("topleft",c("Daily Gauge Data","Rating Curve","Minimum Stage"),pch = c(1,NA,NA),
       lty=c(NA,1,3),lwd=c(1,2.5,2.5),col=c("blue","black","black"),bty="n")

plot(DeFacto_marseilles,Stage_marseilles,col="blue",xlab="Streamflow (MGD)",                  #Illinois @ Marseilles
     ylab="Stage (ft)")
par(new=T)
abline(a=458.9,b=0.000120,col="black",lwd=2)
abline(458.5,0,lty=3,lwd=2)
title(main="Rating Curve at Marseilles")
legend("topleft",c("Daily Gauge Data","Rating Curve","Minimum Stage"),pch = c(1,NA,NA),
       lty=c(NA,1,3),lwd=c(1,2.5,2.5),col=c("blue","black","black"),bty="n")

plot(DeFacto_Dresden,Stage_Dresden,col="blue",xlab="Streamflow (MGD)",                        #Illinois @ Dresden
     ylab="Stage (ft)")
par(new=T)
abline(a=483.3,b=0.000372,col="black",lwd=2)
abline(482.3,0,lty=3,lwd=2)
title(main="Rating Curve at Dresden")
legend("topleft",c("Daily Gauge Data","Rating Curve","Minimum Stage"),pch = c(1,NA,NA),
       lty=c(NA,1,3),lwd=c(1,2.5,2.5),col=c("blue","black","black"),bty="n")




