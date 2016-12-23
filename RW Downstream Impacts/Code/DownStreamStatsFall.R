#############################################################
# Computes statistics dealing with
# downstream impacts of reclaimed water consumption
#
# By Zachary Barker
# October, 2015
#
#############################################################

## Set folder ##
setwd("E:/Illinois Research/RW for PP Cooling/Analysis/Downstream/Seasonal")

######################### Read in data ######################

LaGrange = read.csv("LaGrangeFall.csv")                                     #Illinois River @ La Grange
Flow_LaGrange = LaGrange[["FlowFall"]]
Stage_LaGrange = LaGrange[["StageFall"]]
DeFacto_LaGrange = Flow_LaGrange*0.646316889697

Peoria = read.csv("PeoriaFall.csv")                                         #Illinois River @ Peoria
Flow_Peoria = Peoria[["FlowFall"]]
Stage_Peoria = Peoria[["StageFall"]]
DeFacto_Peoria = Flow_Peoria*0.646316889697

StarvedRock = read.csv("StarvedRockFall.csv")                               #Illinois River @ Starved Rock
Flow_StarvedRock = StarvedRock[["FlowFall"]]
Stage_StarvedRock = StarvedRock[["StageFall"]]
DeFacto_StarvedRock = Flow_StarvedRock*0.646316889697

Marseilles = read.csv("MarseillesFall.csv")                                 #Illinois River @ Marseilles
Flow_marseilles = Marseilles[["FlowFall"]]
Stage_marseilles = Marseilles[["StageFall"]]
DeFacto_marseilles = Flow_marseilles*0.646316889697

Dresden = read.csv("DresdenFall.csv")                                       #Illinois River @ Dresden
Flow_Dresden = Dresden[["FlowFall"]]
Stage_Dresden = Dresden[["StageFall"]]
DeFacto_Dresden = Flow_Dresden*0.646316889697

######## No Consumption Probability of Failure ############

LaGrange_Defacto_Pf = (sum(Stage_LaGrange<419.6)/length(Stage_LaGrange))*100                      #Illinois @ La Grange
Peoria_Defacto_Pf = (sum(Stage_Peoria<430.0)/length(Stage_Peoria))*100                            #Illinois @ Peoria
StarvedRock_Defacto_Pf = (sum(Stage_StarvedRock<440.3)/length(Stage_StarvedRock))*100             #Illinois @ Starved Rock
Marseilles_Defacto_Pf = (sum(Stage_marseilles<458.5)/length(Stage_marseilles))*100                #Illinois @ Marseilles
Dresden_Defacto_Pf = (sum(Stage_Dresden<482.8)/length(Stage_Dresden))*100                         #Illinois @ Dresden

PF_Output = file("PF_Output_Fall.csv","w+")                                                                 #Create a output file for probability of failure
cat("RW","Dresden","Marseilles","StarvedRock","Peoria","LaGrange\n",file = PF_Output, sep = ",")            #Wtites de facto scenario
cat("0",Dresden_Defacto_Pf,Marseilles_Defacto_Pf,StarvedRock_Defacto_Pf,Peoria_Defacto_Pf,
    LaGrange_Defacto_Pf,file = PF_Output, sep = ",")
cat(file = PF_Output, sep = "\n")

############## No Consumption t test ################

LaGrange_Test1 = t.test(DeFacto_LaGrange,DeFacto_LaGrange,var.equal = TRUE)                       #Computes t test for no consumption
Peoria_Test1 = t.test(DeFacto_Peoria,DeFacto_Peoria,var.equal = TRUE)
StarvedRock_Test1 = t.test(DeFacto_StarvedRock,DeFacto_StarvedRock,var.equal = TRUE)
Marseilles_Test1 = t.test(DeFacto_marseilles,DeFacto_marseilles,var.equal = TRUE)
Dresden_Test1 = t.test(DeFacto_Dresden,DeFacto_Dresden,var.equal = TRUE)

LaGrange_Test2 = LaGrange_Test1[["statistic"]]                                                    #Parses t test out of result
Peoria_Test2 = Peoria_Test1[["statistic"]]
StarvedRock_Test2 = StarvedRock_Test1[["statistic"]]
Marseilles_Test2 = Marseilles_Test1[["statistic"]]
Dresden_Test2 = Dresden_Test1[["statistic"]]

LaGrange_Test = LaGrange_Test2[[1]]                                                               #Parses number from t test
Peoria_Test = Peoria_Test2[[1]]
StarvedRock_Test = StarvedRock_Test2[[1]]
Marseilles_Test = Marseilles_Test2[[1]]
Dresden_Test = Dresden_Test2[[1]]

tTest_Output = file("tTest_Output_Fall.csv","w+")                                                               #Create a output file for t Tests
cat("RW","Dresden","Marseilles","StarvedRock","Peoria",
    "LaGrange\n",file = tTest_Output, sep = ",")                                                            #Wtites de facto scenario
cat("0",Dresden_Test,Marseilles_Test,StarvedRock_Test,Peoria_Test,LaGrange_Test,
    file = tTest_Output, sep = ",")
cat(file = tTest_Output, sep = "\n")

############## COUNSUMPTION #########################

for (i in seq(10,1800,10)){

  Consumption = i                                                 #Loops through consumption scenarios

############## Calculate RW #########################

  RW_LaGrange = DeFacto_LaGrange-Consumption
  RW_LaGrange[RW_LaGrange<0]=0                                    #Removes negatives and replaces with zero
  RW_Peoria = DeFacto_Peoria-Consumption
  RW_Peoria[RW_Peoria<0]=0
  RW_StarvedRock = DeFacto_StarvedRock-Consumption
  RW_StarvedRock[RW_StarvedRock<0]=0
  RW_marseilles = DeFacto_marseilles-Consumption
  RW_marseilles[RW_marseilles<0]=0
  RW_Dresden = DeFacto_Dresden-Consumption
  RW_Dresden[RW_Dresden<0]=0

########### With Reclaimed Water Consumption Probability of Failure #########

  LaGrange_RW_Stage = Stage_LaGrange-(Consumption*.000572)                                        #Illinois @ La Grange
  LaGrange_RW_Pf = (sum(LaGrange_RW_Stage<419.6)/length(LaGrange_RW_Stage))*100
  
  Peoria_RW_Stage = Stage_Peoria-(Consumption*.000828)                                            #Illinois @ Peoria
  Peoria_RW_Pf = (sum(Peoria_RW_Stage<430.0)/length(Peoria_RW_Stage))*100
  
  StarvedRock_RW_Stage = Stage_StarvedRock-(Consumption*.000486)                                  #Illinois @ Starved Rock
  StarvedRock_RW_Pf = (sum(StarvedRock_RW_Stage<440.3)/length(StarvedRock_RW_Stage))*100
  
  Marseilles_RW_Stage = Stage_marseilles-(Consumption*.000120)                                    #Illinois @ Marseilles
  Marseilles_RW_Pf = (sum(Marseilles_RW_Stage<458.5)/length(Marseilles_RW_Stage))*100
  
  Dresden_RW_Stage = Stage_Dresden-(Consumption*.000372)                                          #Illinois @ Dresden
  Dresden_RW_Pf = (sum(Dresden_RW_Stage<482.8)/length(Dresden_RW_Stage))*100
  
  cat(Consumption,Dresden_RW_Pf,Marseilles_RW_Pf,StarvedRock_RW_Pf,Peoria_RW_Pf,                           #Writes RW scenarios
      LaGrange_RW_Pf,file = PF_Output, sep = ",")
  cat(file = PF_Output, sep = "\n")

########################## t tests ########################################

  LaGrange_Test1 = t.test(DeFacto_LaGrange,RW_LaGrange,var.equal = TRUE)                           #Computes t test for each gauge
  Peoria_Test1 = t.test(DeFacto_Peoria,RW_Peoria,var.equal = TRUE)
  StarvedRock_Test1 = t.test(DeFacto_StarvedRock,RW_StarvedRock,var.equal = TRUE)
  Marseilles_Test1 = t.test(DeFacto_marseilles,RW_marseilles,var.equal = TRUE)
  Dresden_Test1 = t.test(DeFacto_Dresden,RW_Dresden,var.equal = TRUE)
  
  LaGrange_Test2 = LaGrange_Test1[["statistic"]]                                                    #Parses t test out of result
  Peoria_Test2 = Peoria_Test1[["statistic"]]
  StarvedRock_Test2 = StarvedRock_Test1[["statistic"]]
  Marseilles_Test2 = Marseilles_Test1[["statistic"]]
  Dresden_Test2 = Dresden_Test1[["statistic"]]
  
  LaGrange_Test = LaGrange_Test2[[1]]                                                               #Parses number from t test
  Peoria_Test = Peoria_Test2[[1]]
  StarvedRock_Test = StarvedRock_Test2[[1]]
  Marseilles_Test = Marseilles_Test2[[1]]
  Dresden_Test = Dresden_Test2[[1]]

  
  cat(Consumption,Dresden_Test,                                                      #Writes RW scenarios
      Marseilles_Test,StarvedRock_Test,Peoria_Test,LaGrange_Test,
      file = tTest_Output, sep = ",")
  cat(file = tTest_Output, sep = "\n")

##########################################################################
}                                                                                             #Ends loop

close(PF_Output)
close(tTest_Output)
