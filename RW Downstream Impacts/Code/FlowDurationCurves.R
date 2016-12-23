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

LaGrange = read.csv("LaGrange.csv")                                     #Illinois River @ La Grange
Flow_LaGrange = LaGrange[["Flow_cfs"]]
Stage_LaGrange = LaGrange[["Stage_ft"]]
DeFacto_LaGrange = Flow_LaGrange*0.646316889697
RW_LaGrange = DeFacto_LaGrange-Consumption

Peoria = read.csv("Peoria.csv")                                         #Illinois River @ Peoria
Flow_Peoria = Peoria[["Flow_cfs"]]
Stage_Peoria = Peoria[["Stage_ft"]]
DeFacto_Peoria = Flow_Peoria*0.646316889697
RW_Peoria = DeFacto_Peoria-Consumption

StarvedRock = read.csv("StarvedRock.csv")                               #Illinois River @ Starved Rock
Flow_StarvedRock = StarvedRock[["Flow_cfs"]]
Stage_StarvedRock = StarvedRock[["Stage_ft"]]
DeFacto_StarvedRock = Flow_StarvedRock*0.646316889697
RW_StarvedRock = DeFacto_StarvedRock-Consumption

Marseilles = read.csv("Marseilles.csv")                                #Illinois River @ Marseilles
Flow_marseilles = Marseilles[["Flow_cfs"]]
Stage_marseilles = Marseilles[["Stage_ft"]]
DeFacto_marseilles = Flow_marseilles*0.646316889697
RW_marseilles = DeFacto_marseilles-Consumption
RW_marseilles[RW_marseilles<0]=0

Dresden = read.csv("Dresden.csv")                                       #Illinois River @ Dresden
Flow_Dresden = Dresden[["Flow_cfs"]]
Stage_Dresden = Dresden[["Stage_ft"]]
DeFacto_Dresden = Flow_Dresden*0.646316889697
RW_Dresden = DeFacto_Dresden-Consumption

Kankakee = read.csv("KankakeeRiver.csv")                                #Kankakee River
DeFacto_kankakee = Kankakee[["De.Facto"]]
RW_kankakee = DeFacto_kankakee-37

CSSC = read.csv("CSSC.csv")                                            #Chicago Sanitary & Shipping Cannal
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

########################### Plots flow duration curves ##############################
location = .50

plot(sort(DeFacto_LaGrange),sort(exceed_LaGrange1,decreasing=T),                        #Illinois River @ La Grange
     type="l",lty=1,lwd=2,col="red",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
     xlim=c(0,max(DeFacto_LaGrange)),ylim=c(0,100))
par(new=T)
plot(sort(RW_LaGrange),sort(exceed_LaGrange2,decreasing=T),
     type="l",axes=F,lty=3,lwd=2,col="blue",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
     xlim=c(0,max(DeFacto_LaGrange)),ylim=c(0,100))
legend(max(DeFacto_LaGrange)*location,97.5,c("De facto reuse","Engineered reuse"),
       lty=c(1,3),lwd=c(2.5,2.5),col=c("red","blue"),bty="n")
title(main="Illinois @ La Grange Flow Duration")

plot(sort(DeFacto_Peoria),sort(exceed_Peoria1,decreasing=T),                              #Illinois River @ Peoria
     type="l",lty=1,lwd=2,col="red",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
     xlim=c(0,max(DeFacto_Peoria)),ylim=c(0,100))
par(new=T)
plot(sort(RW_Peoria),sort(exceed_Peoria2,decreasing=T),
     type="l",axes=F,lty=3,lwd=2,col="blue",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
     xlim=c(0,max(DeFacto_Peoria)),ylim=c(0,100))
legend(max(DeFacto_Peoria)*location,97.5,c("De facto reuse","Engineered reuse"),
       lty=c(1,3),lwd=c(2.5,2.5),col=c("red","blue"),bty="n")
title(main="Illinois @ Peoria Flow Duration")

plot(sort(DeFacto_StarvedRock),sort(exceed_StarvedRock1,decreasing=T),                    #Illinois River @ Starved Rock
     type="l",lty=1,lwd=2,col="red",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
     xlim=c(0,max(DeFacto_StarvedRock)),ylim=c(0,100))
par(new=T)
plot(sort(RW_StarvedRock),sort(exceed_StarvedRock2,decreasing=T),
     type="l",axes=F,lty=3,lwd=2,col="blue",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
     xlim=c(0,max(DeFacto_StarvedRock)),ylim=c(0,100))
legend(max(DeFacto_StarvedRock)*location,97.5,c("De facto reuse","Engineered reuse"),
       lty=c(1,3),lwd=c(2.5,2.5),col=c("red","blue"),bty="n")
title(main="Illinois @ Starved Rock Flow Duration")

plot(sort(DeFacto_marseilles),sort(exceed_marseilles1,decreasing=T),                      #Illinois River @ Marseilles
     type="l",lty=1,lwd=2,col="red",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
    xlim=c(0,max(DeFacto_marseilles)),ylim=c(0,100))
par(new=T)
plot(sort(RW_marseilles),sort(exceed_marseilles2,decreasing=T),
     type="l",axes=F,lty=3,lwd=2,col="blue",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
     xlim=c(0,max(DeFacto_marseilles)),ylim=c(0,100))
legend(max(DeFacto_marseilles)*location,97.5,c("De facto reuse","Engineered reuse"),
       lty=c(1,3),lwd=c(2.5,2.5),col=c("red","blue"),bty="n")
title(main="Illinois @ Marseilles Flow Duration")

plot(sort(DeFacto_Dresden),sort(exceed_Dresden1,decreasing=T),                              #Illinois River @ Dresden
     type="l",lty=1,lwd=2,col="red",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
     xlim=c(0,max(DeFacto_Dresden)),ylim=c(0,100))
par(new=T)
plot(sort(RW_Dresden),sort(exceed_Dresden2,decreasing=T),
     type="l",axes=F,lty=3,lwd=2,col="blue",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
     xlim=c(0,max(DeFacto_Dresden)),ylim=c(0,100),col.axis = "white",col.main = "white")
legend(max(DeFacto_Dresden)*location,97.5,c("De facto reuse","Engineered reuse"),
       lty=c(1,3),lwd=c(2.5,2.5),col=c("red","blue"),bty="n")
# axis(1,col = "white")
# axis(2,col = "white")
# title(main="Illinois @ Dresden Flow Duration", col = "white")

plot(sort(DeFacto_kankakee),sort(exceed_kankakee1,decreasing=T),                            #Kankakee River
     type="l",lty=1,lwd=2,col="red",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
     xlim=c(0,max(DeFacto_kankakee)),ylim=c(0,100))
par(new=T)
plot(sort(RW_kankakee),sort(exceed_kankakee2,decreasing=T),
     type="l",axes=F,lty=3,lwd=2,col="blue",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
     xlim=c(0,max(DeFacto_kankakee)),ylim=c(0,100))
legend(max(DeFacto_kankakee)*location,97.5,c("De facto reuse","Engineered reuse"),
       lty=c(1,3),lwd=c(2.5,2.5),col=c("red","blue"),bty="n")
title(main="Kankakee River Flow Duration")

plot(sort(DeFacto_cssc),sort(exceed_cssc1,decreasing=T),                                   #Chicago Sanitary & Shipping Canal
     type="l",lty=1,lwd=2,col="red",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
     xlim=c(0,max(DeFacto_cssc)),ylim=c(0,100))
par(new=T)
plot(sort(RW_cssc),sort(exceed_cssc2,decreasing=T),
     type="l",axes=F,lty=3,lwd=2,col="blue",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
     xlim=c(0,max(DeFacto_cssc)),ylim=c(0,100))
legend(max(DeFacto_cssc)*location,97.5,c("De facto reuse","Engineered reuse"),
       lty=c(1,3),lwd=c(2.5,2.5),col=c("red","blue"),bty="n")
title(main="Chicago S & S Canal Flow Duration")

plot(sort(DeFacto_stickney),sort(exceed_stickney1,decreasing=T),                           #Stickney WWTP
     type="l",lty=1,lwd=2,col="red",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
     xlim=c(0,max(DeFacto_stickney)),ylim=c(0,100))
par(new=T)
plot(sort(RW_stickney),sort(exceed_stickney2,decreasing=T),
     type="l",axes=F,lty=3,lwd=2,col="blue",xlab="Streamflow (MGD)",
     ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
     xlim=c(0,max(DeFacto_stickney)),ylim=c(0,100))
legend(max(DeFacto_stickney)*location,97.5,c("De facto reuse","Engineered reuse"),
       lty=c(1,3),lwd=c(2.5,2.5),col=c("red","blue"),bty="n")
title(main="Stickney WWTP Flow Duration")

# ################ Combine Plots #############################
# location = .4
# attach(mtcars)
# par(mfrow=c(2,2))
# 
# plot(sort(DeFacto_cssc),sort(exceed_cssc1,decreasing=T),                                   #Chicago Sanitary & Shipping Canal
#      type="l",lty=1,lwd=2,col="red",xlab="Streamflow (MGD)",
#      ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
#      xlim=c(0,max(DeFacto_cssc)),ylim=c(0,100))
# par(new=T)
# plot(sort(RW_cssc),sort(exceed_cssc2,decreasing=T),
#      type="l",axes=F,lty=3,lwd=2,col="blue",xlab="Streamflow (MGD)",
#      ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
#      xlim=c(0,max(DeFacto_cssc)),ylim=c(0,100))
# legend(max(DeFacto_cssc)*location,97.5,c("De facto reuse","Engineered reuse"),
#        lty=c(1,3),lwd=c(2.5,2.5),col=c("red","blue"),bty="n")
# title(main="Chicago S & S Canal Flow Duration")
# 
# plot(sort(DeFacto_kankakee),sort(exceed_kankakee1,decreasing=T),                            #Kankakee River
#      type="l",lty=1,lwd=2,col="red",xlab="Streamflow (MGD)",
#      ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
#      xlim=c(0,max(DeFacto_kankakee)),ylim=c(0,100))
# par(new=T)
# plot(sort(RW_kankakee),sort(exceed_kankakee2,decreasing=T),
#      type="l",axes=F,lty=3,lwd=2,col="blue",xlab="Streamflow (MGD)",
#      ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
#      xlim=c(0,max(DeFacto_kankakee)),ylim=c(0,100))
# legend(max(DeFacto_kankakee)*location,97.5,c("De facto reuse","Engineered reuse"),
#        lty=c(1,3),lwd=c(2.5,2.5),col=c("red","blue"),bty="n")
# title(main="Kankakee River Flow Duration")
# 
# plot(sort(DeFacto_marseilles),sort(exceed_marseilles1,decreasing=T),                      #Illinois River @ Marseilles
#      type="l",lty=1,lwd=2,col="red",xlab="Streamflow (MGD)",
#      ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
#      xlim=c(0,max(DeFacto_marseilles)),ylim=c(0,100))
# par(new=T)
# plot(sort(RW_marseilles),sort(exceed_marseilles2,decreasing=T),
#      type="l",axes=F,lty=3,lwd=2,col="blue",xlab="Streamflow (MGD)",
#      ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
#      xlim=c(0,max(DeFacto_marseilles)),ylim=c(0,100))
# legend(max(DeFacto_marseilles)*location,97.5,c("De facto reuse","Engineered reuse"),
#        lty=c(1,3),lwd=c(2.5,2.5),col=c("red","blue"),bty="n")
# title(main="Illinois @ Marseilles Flow Duration")
# 
# plot(sort(DeFacto_LaGrange),sort(exceed_LaGrange1,decreasing=T),                        #Illinois River @ La Grange
#      type="l",lty=1,lwd=2,col="red",xlab="Streamflow (MGD)",
#      ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
#      xlim=c(0,max(DeFacto_LaGrange)),ylim=c(0,100))
# par(new=T)
# plot(sort(RW_LaGrange),sort(exceed_LaGrange2,decreasing=T),
#      type="l",axes=F,lty=3,lwd=2,col="blue",xlab="Streamflow (MGD)",
#      ylab="Exceedence Probability (%)",xaxs="i",yaxs="i",
#      xlim=c(0,max(DeFacto_LaGrange)),ylim=c(0,100))
# legend(max(DeFacto_LaGrange)*location,97.5,c("De facto reuse","Engineered reuse"),
#        lty=c(1,3),lwd=c(2.5,2.5),col=c("red","blue"),bty="n")
# title(main="Illinois @ La Grange Flow Duration")
# 
