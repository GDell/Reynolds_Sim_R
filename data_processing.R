# Load GGPLOT2 for creating figures and graphs.
library(ggplot2)
#         VALIDATION DATA
# ALIGN
alignment_25.data <- read.csv(file="./data/individualBehaviors/alignment/align25Validation.csv")
alignment_5.data <- read.csv(file="./data/individualBehaviors/alignment/align50Validation.csv")
alignment_75.data <- read.csv(file="./data/individualBehaviors/alignment/align75Validation.csv")
alignment_100.data <- read.csv(file="./data/individualBehaviors/alignment/align100Validation.csv")
#AVOID
avoidance_25.data <- read.csv(file="./data/individualBehaviors/avoidance/avoidance25Validation.csv")
avoidance_5.data <- read.csv(file="./data/individualBehaviors/avoidance/avoidance50Validation.csv")
avoidance_75.data <- read.csv(file="./data/individualBehaviors/avoidance/avoidance75Validation.csv")
avoidance_100.data <- read.csv(file="./data/individualBehaviors/avoidance/avoidance100Validation.csv")
#ATTRACT
attraction_25.data <- read.csv(file="./data/individualBehaviors/attraction/attraction25Validation.csv")
attraction_5.data <- read.csv(file="./data/individualBehaviors/attraction/attraction50Validation.csv")
attraction_75.data <- read.csv(file="./data/individualBehaviors/attraction/attraction75Validation.csv")
attraction_100.data <- read.csv(file="./data/individualBehaviors/attraction/attraction100Validation.csv")
#LIGHT
light_25.data <- read.csv(file="./data/individualBehaviors/lightAttraction/light25Validation.csv")
light_5.data <- read.csv(file="./data/individualBehaviors/lightAttraction/light50Validation.csv")
light_75.data <- read.csv(file="./data/individualBehaviors/lightAttraction/light75Validation.csv")
light_100.data <- read.csv(file="./data/individualBehaviors/lightAttraction/light100Validation.csv")
#BOID
boid_25.data <- read.csv(file="./data/individualBehaviors/boidSim/boid50Validation.csv")
boid_5.data <- read.csv(file="./data/individualBehaviors/boidSim/boid25Validation.csv")
boid_75.data <- read.csv(file="./data/individualBehaviors/boidSim/boid75Validation.csv")
boid_100.data <- read.csv(file="./data/individualBehaviors/boidSim/boid100Validation.csv")

#         INTERACTION DATA

alignXlight_25.data <-read.csv("./data/pairedBehaviors/alignXlight/interaction25Validation.csv")
alignXlight_5.data <-read.csv("./data/pairedBehaviors/alignXlight/interaction50Validation.csv")
alignXlight_75.data <-read.csv("./data/pairedBehaviors/alignXlight/interaction75Validation.csv")
alignXlight_100.data <-read.csv("./data/pairedBehaviors/alignXlight/interaction100Validation.csv")

attractXlight_25.data <- read.csv("./data/pairedBehaviors/attractionXlight/interaction25Validation.csv")
attractXlight_5.data <- read.csv("./data/pairedBehaviors/attractionXlight/interaction50Validation.csv")
attractXlight_75.data <- read.csv("./data/pairedBehaviors/attractionXlight/interaction75Validation.csv")
attractXlight_100.data <- read.csv("./data/pairedBehaviors/attractionXlight/interaction100Validation.csv")

attractXalign_25.data <- read.csv("./data/pairedBehaviors/attractXalign/interaction25Validation.csv")
attractXalign_5.data <- read.csv("./data/pairedBehaviors/attractXalign/interaction50Validation.csv")
attractXalign_75.data <- read.csv("./data/pairedBehaviors/attractXalign/interaction75Validation.csv")
attractXalign_100.data <- read.csv("./data/pairedBehaviors/attractXalign/interaction100Validation.csv")

attractXavoid_25.data <- read.csv("./data/pairedBehaviors/attractXavoid/interaction25Validation.csv")
attractXavoid_5.data <- read.csv("./data/pairedBehaviors/attractXavoid/interaction50Validation.csv")
attractXavoid_75.data <- read.csv("./data/pairedBehaviors/attractXavoid/interaction75Validation.csv")
attractXavoid_100.data <- read.csv("./data/pairedBehaviors/attractXavoid/interaction100Validation.csv")

avoidXlight_25.data <- read.csv("./data/pairedBehaviors/avoidXlight/interaction25Validation.csv")
avoidXlight_5.data <- read.csv("./data/pairedBehaviors/avoidXlight/interaction50Validation.csv")
avoidXlight_75.data <- read.csv("./data/pairedBehaviors/avoidXlight/interaction75Validation.csv")
avoidXlight_100.data <- read.csv("./data/pairedBehaviors/avoidXlight/interaction100Validation.csv")

avoidXalign_25.data <- read.csv("./data/pairedBehaviors/avoidXalign/interaction25Validation.csv")
avoidXalign_5.data <- read.csv("./data/pairedBehaviors/avoidXalign/interaction50Validation.csv")
avoidXalign_75.data <-read.csv("./data/pairedBehaviors/avoidXalign/interaction75Validation.csv")
avoidXalign_100.data <-read.csv("./data/pairedBehaviors/avoidXalign/interaction100Validation.csv")



# Returns the average and STdev of the three trials in a behaviorSTRENGTHValidation.csv
returnTrialAvg <- function(dataEx, whichPhase) {
  overallAvg <- NA
  stdev <- NA

   if(whichPhase == "first") {
    combinedTrialData <- c(dataEx$Trial1[1:200], dataEx$Trial2[1:200], dataEx$Trial3[1:200])  
    # print(combinedTrialData)
    overallAvg <- mean(combinedTrialData)
    stdev <- sd(combinedTrialData)
  } else if(whichPhase == "second") {
    combinedTrialData <- c(dataEx$Trial1[200:800], dataEx$Trial2[200:800], dataEx$Trial3[200:800]) 
    # print(combinedTrialData)
    # print("WE MADE IT") 
    overallAvg <- mean(combinedTrialData)
    stdev <- sd(combinedTrialData)
  } else if(whichPhase == "full") {
    combinedTrialData <- c(dataEx$Trial1[1:800], dataEx$Trial2[1:800], dataEx$Trial3[1:800])  
    overallAvg <- mean(combinedTrialData)
    stdev <- sd(combinedTrialData)
  }
  return(c(overallAvg, stdev))
}



createBehaviorRangeGraph <- function(point25, std25, point50, std50, point75, std75, point100, std100, behaviorTitle) {

  # print(c(point25,point50, point75, point100))

  pointsLog <-  c(NA, point25,point50, point75, point100)
  stdLog <- c(NA , std25,std50, std75, std100)

   mean_sd <- data.frame(mean=pointsLog, sem=stdLog, group=c(0,25, 50, 75, 100 ))



    ggplot(mean_sd, aes(x=group, y=mean)) + 
    geom_line(stat='identity') +
    geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),
                  width=.2) +
    xlab('Strength') +
    ylab('GSI') + ylim(0.00:1.00) + ggtitle(behaviorTitle)

      # ggplot(mean_sd, aes(x=group, y=mean)) + 
      # geom_line(stat='identity') +
      # geom_errorbar(aes(ymin=mean-sem, ymax=mean+sem),
      #               width=.2) +
      # xlab('Strength') +
      # ylab('GSI') + ylim(0.00:1.00) + ggtitle(behaviorTitle)
}

  



graphGSIByStrength <- function(whichPhase) {

  phase <- whichPhase

  # print(paste("THIS IS PHASE", phase))

  light25result <- returnTrialAvg(light_25.data, phase)
  light50result <- returnTrialAvg(light_5.data, phase)
  light75result <- returnTrialAvg(light_75.data, phase)
  light100result <- returnTrialAvg(light_100.data, phase)

  # lightPlotData <- c(light25result, light50result, light75result, light100result)

  align25result <- returnTrialAvg(alignment_25.data, phase)
  align50result <- returnTrialAvg(alignment_5.data, phase)
  align75result <- returnTrialAvg(alignment_75.data, phase)
  align100result <- returnTrialAvg(alignment_100.data, phase)

  # alignPlotData <- c(align25result, align50result, align75result, align100result)

  attraction25result <- returnTrialAvg(attraction_25.data, phase)
  attraction50result <- returnTrialAvg(attraction_5.data, phase)
  attraction75result <- returnTrialAvg(attraction_75.data, phase)
  attraction100result <- returnTrialAvg(attraction_100.data, phase)

  # attractPlotData <- c(attraction25result, attraction50result, attraction75result, attraction100result)

  avoidance25result <- returnTrialAvg(avoidance_25.data, phase)
  avoidance50result <- returnTrialAvg(avoidance_5.data, phase)
  avoidance75result <- returnTrialAvg(avoidance_75.data, phase)
  avoidance100result <- returnTrialAvg(avoidance_100.data, phase)

  # avoidPlotData <- c(avoidance25result, avoidance50result, avoidance75result, avoidance100result)

  boid25result <- returnTrialAvg(boid_25.data, phase)
  boid50result <- returnTrialAvg(boid_5.data, phase)
  boid75result <- returnTrialAvg(boid_75.data, phase)
  boid100result <- returnTrialAvg(boid_100.data, phase)

  # boidPlotData <- c(boid25result, boid50result, boid75result, boid100result)

  # INTERACTION
  attractXlight25result <- returnTrialAvg(attractXlight_25.data, phase)
  attractXlight50result <- returnTrialAvg(attractXlight_5.data, phase)
  attractXlight75result <- returnTrialAvg(attractXlight_75.data, phase)
  attractXlight100result <- returnTrialAvg(attractXlight_100.data, phase)

  attractXlight25 <- attractXlight25result[1]
  attractXlight50 <- attractXlight50result[1]
  attractXlight75 <- attractXlight75result[1]
  attractXlight100 <- attractXlight100result[1]

  STDattractXlight25 <- attractXlight25result[2]
  STDattractXlight50 <- attractXlight50result[2]
  STDattractXlight75 <- attractXlight75result[2]
  STDattractXlight100 <- attractXlight100result[2]


  attractXlightlog <- c(NA, attractXlight25, attractXlight50, attractXlight75, attractXlight100)
  attractXlightSTDlog <- c(NA, STDattractXlight25, STDattractXlight50,STDattractXlight75,STDattractXlight100)

  alignXlighttemp25 <- returnTrialAvg(alignXlight_25.data, phase)
  alignXlighttemp50 <- returnTrialAvg(alignXlight_5.data, phase)
  alignXlightemp75 <- returnTrialAvg(alignXlight_75.data, phase)
  alignXlighttemp100 <- returnTrialAvg(alignXlight_100.data, phase)

  alignXlight25 <- alignXlighttemp25[1]
  alignXlight50 <- alignXlighttemp50[1]
  alignXlight75 <- alignXlightemp75[1]
  alignXlight100 <- alignXlighttemp100[1]

  STDalignXlight25 <- alignXlighttemp25[2]
  STDalignXlight50 <- alignXlighttemp50[2]
  STDalignXlight75 <- alignXlightemp75[2]
  STDalignXlight100 <- alignXlighttemp100[2]

  alignXlightlog <- c(NA, alignXlight25, alignXlight50, alignXlight75, alignXlight100)
  alignXlightSTDlog <- c(NA, STDalignXlight25, STDalignXlight50, STDalignXlight75, STDalignXlight100)

  attractXaligntemp25 <- returnTrialAvg(attractXalign_25.data, phase)
  attractXaligntemp50 <- returnTrialAvg(attractXalign_5.data, phase)
  attractXaligntemp75 <- returnTrialAvg(attractXalign_75.data, phase)
  attractXaligntemp100 <- returnTrialAvg(attractXalign_100.data, phase)

  attractXalign25 <- attractXaligntemp25[1]
  attractXalign50 <-attractXaligntemp50[1]
  attractXalign75 <- attractXaligntemp75[1]
  attractXalign100 <- attractXaligntemp100[1]

  STDattractXalign25 <- attractXalign25[2]
  STDattractXalign50 <-attractXaligntemp50[2]
  STDattractXalign75 <- attractXaligntemp75[2]
  STDattractXalign100 <- attractXaligntemp100[2]

  attractXalignlog <- c(NA, attractXalign25, attractXalign50, attractXalign75, attractXalign100)
  attractXalignSTDlog <- c(NA, STDattractXalign25, STDattractXalign50, STDattractXalign75, STDattractXalign100)

  attractXavoidtemp25 <- returnTrialAvg(attractXavoid_25.data, phase)
  attractXavoidtemp50 <- returnTrialAvg(attractXavoid_5.data, phase)
  attractXavoidtemp75 <- returnTrialAvg(attractXavoid_75.data, phase)
  attractXavoidtemp100 <- returnTrialAvg(attractXavoid_100.data, phase)

  attractXavoid25 <- attractXavoidtemp25[1]
  attractXavoid50 <- attractXavoidtemp50[1]
  attractXavoid75 <- attractXavoidtemp75[1]
  attractXavoid100 <- attractXavoidtemp100[1]

  STDattractXavoid25 <- attractXavoidtemp25[2]
  STDattractXavoid50 <- attractXavoidtemp50[2]
  STDattractXavoid75 <- attractXavoidtemp75[2]
  STDattractXavoid100 <- attractXavoidtemp100[2]


  attractXavoidlog <- c(NA, attractXavoid25, attractXavoid50, attractXavoid75, attractXavoid100)
  attractXavoidSTDlog <- c(NA, STDattractXavoid25, STDattractXavoid50, STDattractXavoid75, STDattractXavoid100)

  avoidXaligntemp25 <-returnTrialAvg(avoidXalign_25.data, phase)
  avoidXaligntemp50 <- returnTrialAvg(avoidXalign_5.data, phase)
  avoidXaligntemp75 <- returnTrialAvg(avoidXalign_75.data, phase)
  avoidXaligntemp100 <- returnTrialAvg(avoidXalign_100.data, phase)

  avoidXalign25 <- avoidXaligntemp25[1]
  avoidXalign50 <- avoidXaligntemp50[1]
  avoidXalign75 <- avoidXaligntemp75[1]
  avoidXalign100 <- avoidXaligntemp100[1]

  STDavoidXalign25 <- avoidXaligntemp25[2]
  STDavoidXalign50 <- avoidXaligntemp50[2]
  STDavoidXalign75 <- avoidXaligntemp75[2]
  STDavoidXalign100 <- avoidXaligntemp100[2]

  avoidXalignlog <- c(NA, avoidXalign25, avoidXalign50, avoidXalign75, avoidXalign100)
  avoidXalignSTDlog <- c(NA, STDavoidXalign25, STDavoidXalign50, STDavoidXalign75, STDavoidXalign100)

  avoidXlighttemp25 <- returnTrialAvg(avoidXlight_25.data, phase)
  avoidXlighttemp50 <- returnTrialAvg(avoidXlight_5.data, phase)
  avoidXlighttemp75 <- returnTrialAvg(avoidXlight_75.data, phase)
  avoidXlighttemp100 <- returnTrialAvg(avoidXlight_100.data, phase)

  avoidXlight25 <- avoidXlighttemp25[1]
  avoidXlight50 <-avoidXlighttemp50[1]
  avoidXlight75 <-avoidXlighttemp75[1]
  avoidXlight100 <- avoidXlighttemp100[1]

  STDavoidXlight25 <- avoidXlighttemp25[2]
  STDavoidXlight50 <-avoidXlighttemp50[2]
  STDavoidXlight75 <-avoidXlighttemp75[2]
  STDavoidXlight100 <- avoidXlighttemp100[2]

  # attractXlightfile 
  # alignXlightfile25, 
  # attractXalign25, 


  # attractXavoid25,

  # avoidXalign25, 
  # avoidXlight25, 

  avoidXlightlog <- c(NA, avoidXlight25, avoidXlight50,avoidXlight75,avoidXlight100)
  avoidXlightSTDlog <- c(NA, STDavoidXlight25, STDavoidXlight50, STDavoidXlight75, STDavoidXlight100)

  fullPairedBehaviorGraph <- data.frame(
                          meanAttractXlight=attractXlightlog, STDattractXlight=attractXlightSTDlog, 
                          meanAlignXlight=alignXlightlog, STDalignXlight =alignXlightSTDlog,  
                          meanAttractXalign=attractXalignlog, STDattractXalign =attractXalignSTDlog,
                          meanAttractXavoid=attractXavoidlog, STDattract = attractXavoidSTDlog,
                          meanAvoidXalign= avoidXalignlog, STDavoidXalign = avoidXalignSTDlog,
                          meanAvoidXlight= avoidXlightlog, STDavoidXlight = avoidXlightSTDlog ,
                          group=c(0,25, 50, 75, 100))

  allPairedBehaviorsGraph <<- ggplot(fullPairedBehaviorGraph, aes(group)) + xlab('Strength') + ylab('GSI') + ylim(0.00:1.00) +
                       geom_line(aes(y = meanAttractXlight, colour = "attract.light"), size=.8)+  geom_point(aes(y=meanAttractXlight), shape=15, size=2) +
                       geom_line(aes(y = meanAlignXlight, colour = "align.light"), size=.8)+  geom_point(aes(y=meanAlignXlight), shape=16, size=2) +
                       geom_line(aes(y = meanAttractXalign, colour = "attract.align"), size=.8)+  geom_point(aes(y=meanAttractXalign), shape=17, size=2) +
                       geom_line(aes(y = meanAttractXavoid, colour = "attract.avoid"), size=.8)+  geom_point(aes(y=meanAttractXavoid), shape=18, size=2) +
                       geom_line(aes(y = meanAvoidXalign, colour = "avoid.align"), size=.8)+  geom_point(aes(y=meanAvoidXalign), shape=19, size=2) +
                       geom_line(aes(y = meanAvoidXlight, colour = "avoid.light"), size=.8)+  geom_point(aes(y=meanAvoidXlight), shape=20, size=2)
  allPairedBehaviorsGraph  + ggtitle("Paired Behavior Interactions")
  allPairedBehaviorsGraph


  # SINGLE BEHAVIORS
  # AVOIDANCE
  avoid25 <- avoidance25result[1]
  avoid50 <- avoidance50result[1]
  avoid75 <- avoidance75result[1]
  avoid100 <- avoidance100result[1]

  stdAvoid25 <- avoidance25result[2]
  stdAvoid50 <- avoidance50result[2]
  stdAvoid75 <- avoidance75result[2]
  stdAvoid100 <- avoidance100result[2]

  # ATTRACTION
  attract25 <- attraction25result[1]
  attract50 <- attraction50result[1]
  attract75 <- attraction75result[1]
  attract100 <- attraction100result[1]

  stdAttract25 <- attraction25result[2]
  stdAttract50 <- attraction50result[2]
  stdAttract75 <- attraction75result[2]
  stdAttract100 <- attraction100result[2]

  #ALIGN
  align25 <- align25result[1]
  align50 <- align50result[1]
  align75 <- align75result[1]
  align100 <- align100result[1]

  stdAlign25 <- align25result[2]
  stdAlign50 <- align50result[2]
  stdAlign75 <- align75result[2]
  stdAlign100 <- align100result[2]

  #LIGHT
  light25 <- light25result[1]
  light50 <- light50result[1]
  light75 <- light75result[1]
  light100 <- light100result[1]

  stdLight25 <- light25result[2]
  stdLight50 <- light50result[2]
  stdLight75 <- light75result[2]
  stdLight100 <- light100result[2]

  avoidLog <-  c(NA, avoid25,avoid50, avoid75, avoid100)
  avoidSTDLog <- c(NA , stdAvoid25,stdAvoid50, stdAvoid75, stdAvoid100)

  attractLog <-  c(NA, attract25,attract50, attract75, attract100)
  attractSTDLog <- c(NA , stdAttract25,stdAttract50, stdAttract75, stdAttract100)

  alignLog <-  c(NA, align25,align50, align75, align100)
  alignSTDLog <- c(NA , stdAlign25,stdAlign50, stdAlign75, stdAlign100)

  lightLog <-  c(NA, light25,light50, light75, light100)
  lightSTDLog <- c(NA , stdLight25,stdLight50, stdLight75, stdLight100)


  fullSingleBehaviorGraph <- data.frame(meanAvoid=avoidLog, STDavoid=avoidSTDLog, 
                          meanLight=lightLog, STDlight =lightSTDLog,  
                          meanAlign=alignLog, STDalign =alignSTDLog,
                          meanAttract=attractLog, STDattract = attractSTDLog,
                          group=c(0,25, 50, 75, 100))

  allBehaviorsGraph <<- ggplot(fullSingleBehaviorGraph, aes(group)) + xlab('Strength') + ylab('GSI') + ylim(0.00:1.00) +
                       geom_line(aes(y = meanLight, colour = "Light"), size=.8)+  geom_point(aes(y=meanLight), shape=21, size=2) +
                       geom_line(aes(y = meanAlign, colour = "Align"), size=.8)+  geom_point(aes(y=meanAlign), shape=22, size=2) +
                       geom_line(aes(y = meanAvoid, colour = "Avoid"), size=.8)+  geom_point(aes(y=meanAvoid), shape=23, size=2) +
                       geom_line(aes(y = meanAttract, colour = "Attract"), size=.8)+  geom_point(aes(y=meanAttract), shape=24, size=2) 
  allBehaviorsGraph  + ggtitle("Behavior Interactions")
  allBehaviorsGraph



  lightPlot <<- createBehaviorRangeGraph(light25result[1], light25result[2], light50result[1], light50result[2], light75result[1], light75result[2], light100result[1], light100result[2], "Light Attraction")
  alignPlot <<- createBehaviorRangeGraph(align25result[1], align25result[2], align50result[1], align50result[2], align75result[1], align75result[2], align100result[1], align100result[2], "Social Alignment")
  attractionPlot <<- createBehaviorRangeGraph(attraction25result[1], attraction25result[2], attraction50result[1], attraction50result[2], attraction75result[1], attraction75result[2], attraction100result[1], attraction100result[2], "Social Attraction")
  avoidPlot <<- createBehaviorRangeGraph(avoidance25result[1], avoidance25result[2], avoidance50result[1], avoidance50result[2], avoidance75result[1], avoidance75result[2], avoidance100result[1], avoidance100result[2], "Social Avoidance")
  boidPlot <<- createBehaviorRangeGraph(boid25result[1], boid25result[2], boid50result[1], boid50result[2], boid75result[1], boid75result[2], boid100result[1], boid100result[2], "Boid Flock")

}


# graphGSIByStrength("first")
# graphGSIByStrength("first")
# graphGSIByStrength("first")

# graphGSIByStrength("second")
# graphGSIByStrength("second")
# graphGSIByStrength("second")
# graphGSIByStrength("second")




# Runs ANOVA for within behavior differences in GSI between behavior strengths. 
calculateWithinANOVAstats <- function(file25, file50, file75, file100, title, whichPhase) {


      if(whichPhase == "first") {
        temp25 <- c(file25$Trial1[1:200], file25$Trial2[1:200], file25$Trial3[1:200])
        temp50 <- c(file50$Trial1[1:200], file50$Trial2[1:200], file50$Trial3[1:200])
        temp75 <- c(file75$Trial1[1:200], file75$Trial2[1:200], file75$Trial3[1:200])
        temp100 <- c(file100$Trial1[1:200], file100$Trial3[1:200], file100$Trial3[1:200])
       
        tempLabel <- c(rep("str25",600),rep("str50",600),rep("str75", 600),rep("str100",600))
        tempTotal <- c(temp25[1:600],temp50[1:600],temp75[1:600],temp100[1:600])

      } else if (whichPhase =="second") {
        temp25 <- c(file25$Trial1[200:800], file25$Trial2[200:800], file25$Trial3[200:800])
        temp50 <- c(file50$Trial1[200:800], file50$Trial2[200:800], file50$Trial3[200:800])
        temp75 <- c(file75$Trial1[200:800], file75$Trial2[200:800], file75$Trial3[200:800])
        temp100 <- c(file100$Trial1[200:800], file100$Trial3[200:800], file100$Trial3[200:800])
        

        tempLabel <- c(rep("str25",1800),rep("str50",1800),rep("str75", 1800),rep("str100",1800))
        tempTotal <- c(temp25[1:1800],temp50[1:1800],temp75[1:1800],temp100[1:1800])

      } else if( whichPhase =="full") {

        temp25 <- c(file25$Trial1, file25$Trial2, file25$Trial3)
        temp50 <- c(file50$Trial1, file50$Trial2, file50$Trial3)
        temp75 <- c(file75$Trial1, file75$Trial2, file75$Trial3)
        temp100 <- c(file100$Trial1, file100$Trial2, file100$Trial3)


        tempLabel <- c(rep("str25",2400),rep("str50",2400),rep("str75", 2400),rep("str100",2400))
        tempTotal <- c(temp25[1:2400],temp50[1:2400],temp75[1:2400],temp100[1:2400])
      }
     
      # length(tempTotal)
      # if(betweenORwithin == "within") {
        tempStatDataframe <- data.frame(
          type = tempLabel,
          gsiList = tempTotal
        )


        fit <- aov(gsiList ~ type, data=tempStatDataframe)

        txtSumName <- paste("./",title,"_Anova_summary.txt")
        txtResultsName <- paste("./",title,"_Anova_results.txt")
        txtPostHocName <- paste("./",title,"_Anova_PostHoc.txt")

        capture_a <- summary(fit)
        capture.output(capture_a, file = txtSumName)
        capture_b <- fit
        capture.output(capture_b, file = txtResultsName)
        postHoc <- TukeyHSD(fit)
        capture_c <- postHoc
        capture.output(capture_c, file = txtPostHocName)

        # postHoc <- TukeyHSD(fit)

        fit
        postHoc
        graphed <- plot(postHoc)
        
        graphed 
}


calculateBetweenANOVAstats <- function(attractfile25, attractfile50, attractfile75, attractfile100, alignfile25, alignfile50, alignfile75, alignfile100, lightfile25, lightfile50, lightfile75, lightfile100, avoidancefile25, avoidancefile50, avoidancefile75, avoidancefile100, boidfile25, boidfile50, boidfile75, boidfile100, whichPhase,title1) {

    if(whichPhase == "first") {
        attracttemp25 <- c(attractfile25$Trial1[1:200], attractfile25$Trial2[1:200], attractfile25$Trial3[1:200])
        attracttemp50 <- c(attractfile50$Trial1[1:200], attractfile50$Trial2[1:200], attractfile50$Trial3[1:200])
        attracttemp75 <- c(attractfile75$Trial1[1:200], attractfile75$Trial2[1:200], attractfile75$Trial3[1:200])
        attracttemp100 <- c(attractfile100$Trial1[1:200], attractfile100$Trial3[1:200], attractfile100$Trial3[1:200])

        aligntemp25 <- c(alignfile25$Trial1[1:200], alignfile25$Trial2[1:200], alignfile25$Trial3[1:200])
        aligntemp50 <- c(alignfile50$Trial1[1:200], alignfile50$Trial2[1:200], alignfile50$Trial3[1:200])
        aligntemp75 <- c(alignfile75$Trial1[1:200], alignfile75$Trial2[1:200], alignfile75$Trial3[1:200])
        aligntemp100 <- c(alignfile100$Trial1[1:200], alignfile100$Trial3[1:200], alignfile100$Trial3[1:200])

        lighttemp25 <- c(lightfile25$Trial1[1:200], lightfile25$Trial2[1:200], lightfile25$Trial3[1:200])
        lighttemp50 <- c(lightfile50$Trial1[1:200], lightfile50$Trial2[1:200], lightfile50$Trial3[1:200])
        lighttemp75 <- c(lightfile75$Trial1[1:200], lightfile75$Trial2[1:200], lightfile75$Trial3[1:200])
        lighttemp100 <- c(lightfile100$Trial1[1:200], lightfile100$Trial3[1:200], lightfile100$Trial3[1:200])

        avoidtemp25 <- c(avoidancefile25$Trial1[1:200], avoidancefile25$Trial2[1:200], avoidancefile25$Trial3[1:200])
        avoidtemp50 <- c(avoidancefile50$Trial1[1:200], avoidancefile50$Trial2[1:200], avoidancefile50$Trial3[1:200])
        avoidtemp75 <- c(avoidancefile75$Trial1[1:200], avoidancefile75$Trial2[1:200], avoidancefile75$Trial3[1:200])
        avoidtemp100 <- c(avoidancefile100$Trial1[1:200], avoidancefile100$Trial3[1:200], avoidancefile100$Trial3[1:200])

        boidtemp25 <- c(boidfile25$Trial1[1:200], boidfile25$Trial2[1:200], boidfile25$Trial3[1:200])
        boidtemp50 <- c(boidfile50$Trial1[1:200], boidfile50$Trial2[1:200], boidfile50$Trial3[1:200])
        boidtemp75 <- c(boidfile75$Trial1[1:200], boidfile75$Trial2[1:200], boidfile75$Trial3[1:200])
        boidtemp100 <- c(boidfile100$Trial1[1:200], boidfile100$Trial3[1:200], boidfile100$Trial3[1:200])



        tempStrength <- c(rep("str25",1800),rep("str25",1800),rep("str25", 1800),rep("str25",1800) ,rep("str25",1800) ,
                       rep("str50",1800),rep("str50",1800),rep("str50", 1800),rep("str50",1800), rep("str50",1800),
                       rep("str75",1800),rep("str75",1800),rep("str75", 1800),rep("str75",1800), rep("str75",1800),
                       rep("str100",1800),rep("str100",1800),rep("str100", 1800),rep("str100",1800), rep("str100",1800))
        tempLabel <- c(rep("attract",600),rep("align",600),rep("light", 600),rep("avoid",600),rep("boid",600),
                       rep("attract",600),rep("align",600),rep("light", 600),rep("avoid",600),rep("boid",600),
                       rep("attract",600),rep("align",600),rep("light", 600),rep("avoid",600),rep("boid",600),
                       rep("attract",600),rep("align",600),rep("light", 600),rep("avoid",600),rep("boid",600) )
        tempTotal <- c(attracttemp25[1:600], aligntemp25[1:600],lighttemp25[1:600],avoidtemp25[1:600],boidtemp25[1:600],
                        attracttemp50[1:600], aligntemp50[1:600],lighttemp50[1:600],avoidtemp50[1:600],boidtemp50[1:600],
                        attracttemp75[1:600], aligntemp75[1:600],lighttemp75[1:600],avoidtemp75[1:600],boidtemp75[1:600],
                        attracttemp100[1:600], aligntemp100[1:600],lighttemp100[1:600],avoidtemp100[1:600],boidtemp100[1:600])

      } else if (whichPhase =="second") {

        attracttemp25 <- c(attractfile25$Trial1[200:800], attractfile25$Trial2[200:800], attractfile25$Trial3[200:800])
        attracttemp50 <- c(attractfile50$Trial1[200:800], attractfile50$Trial2[200:800], attractfile50$Trial3[200:800])
        attracttemp75 <- c(attractfile75$Trial1[200:800], attractfile75$Trial2[200:800], attractfile75$Trial3[200:800])
        attracttemp100 <- c(attractfile100$Trial1[200:800], attractfile100$Trial3[200:800], attractfile100$Trial3[200:800])
        
        aligntemp25 <- c(alignfile25$Trial1[200:800], alignfile25$Trial2[200:800], alignfile25$Trial3[200:800])
        aligntemp50 <- c(alignfile50$Trial1[200:800], alignfile50$Trial2[200:800], alignfile50$Trial3[200:800])
        aligntemp75 <- c(alignfile75$Trial1[200:800], alignfile75$Trial2[200:800], alignfile75$Trial3[200:800])
        aligntemp100 <- c(alignfile100$Trial1[200:800], alignfile100$Trial3[200:800], alignfile100$Trial3[200:800])

        lighttemp25 <- c(lightfile25$Trial1[200:800], lightfile25$Trial2[200:800], lightfile25$Trial3[200:800])
        lighttemp50 <- c(lightfile50$Trial1[200:800], lightfile50$Trial2[200:800], lightfile50$Trial3[200:800])
        lighttemp75 <- c(lightfile75$Trial1[200:800], lightfile75$Trial2[200:800], lightfile75$Trial3[200:800])
        lighttemp100 <- c(lightfile100$Trial1[200:800], lightfile100$Trial3[200:800], lightfile100$Trial3[200:800])

        avoidtemp25 <- c(avoidancefile25$Trial1[200:800], avoidancefile25$Trial2[200:800], avoidancefile25$Trial3[200:800])
        avoidtemp50 <- c(avoidancefile50$Trial1[200:800], avoidancefile50$Trial2[200:800], avoidancefile50$Trial3[200:800])
        avoidtemp75 <- c(avoidancefile75$Trial1[200:800], avoidancefile75$Trial2[200:800], avoidancefile75$Trial3[200:800])
        avoidtemp100 <- c(avoidancefile100$Trial1[200:800], avoidancefile100$Trial3[200:800], avoidancefile100$Trial3[200:800])

        boidtemp25 <- c(boidfile25$Trial1[200:800], boidfile25$Trial2[200:800], boidfile25$Trial3[200:800])
        boidtemp50 <- c(boidfile50$Trial1[200:800], boidfile50$Trial2[200:800], boidfile50$Trial3[200:800])
        boidtemp75 <- c(boidfile75$Trial1[200:800], boidfile75$Trial2[200:800], boidfile75$Trial3[200:800])
        boidtemp100 <- c(boidfile100$Trial1[200:800], boidfile100$Trial3[200:800], boidfile100$Trial3[200:800])

      
        tempLabel <- c(rep("attract",1800),rep("align",1800),rep("light", 1800),rep("avoid",1800) ,rep("boid",1800) ,
                       rep("attract",1800),rep("align",1800),rep("light", 1800),rep("avoid",1800), rep("boid",1800),
                       rep("attract",1800),rep("align",1800),rep("light", 1800),rep("avoid",1800), rep("boid",1800),
                       rep("attract",1800),rep("align",1800),rep("light", 1800),rep("avoid",1800), rep("boid",1800))

        tempStrength <- c(rep("str25",1800),rep("str25",1800),rep("str25", 1800),rep("str25",1800) ,rep("str25",1800) ,
                       rep("str50",1800),rep("str50",1800),rep("str50", 1800),rep("str50",1800), rep("str50",1800),
                       rep("str75",1800),rep("str75",1800),rep("str75", 1800),rep("str75",1800), rep("str75",1800),
                       rep("str100",1800),rep("str100",1800),rep("str100", 1800),rep("str100",1800), rep("str100",1800))


        # tempTotal <- c(temp25[1:1800],temp50[1:1800],temp75[1:1800],temp100[1:1800])

        tempTotal <- c(attracttemp25[1:1800], aligntemp25[1:1800],lighttemp25[1:1800],avoidtemp25[1:1800],boidtemp25[1:1800],
                        attracttemp50[1:1800], aligntemp50[1:1800],lighttemp50[1:1800],avoidtemp50[1:1800],boidtemp50[1:1800],
                        attracttemp75[1:1800], aligntemp75[1:1800],lighttemp75[1:1800],avoidtemp75[1:1800],boidtemp75[1:1800],
                        attracttemp100[1:1800], aligntemp100[1:1800],lighttemp100[1:1800],avoidtemp100[1:1800],boidtemp100[1:1800])

      }  


      

      # length(tempTotal)
      # if(betweenORwithin == "within") {
      tempStatDataframe <- data.frame(
        behavior = tempLabel,
        gsiList = tempTotal,
        strength = tempStrength
      )

        fit <- aov(gsiList ~ strength * behavior, data=tempStatDataframe)

        txtSumName <- paste("./",title1,"_Anova_summary.txt")
        txtResultsName <- paste("./",title1,"_Anova_results.txt")
        txtPostHocName <- paste("./",title1,"_Anova_PostHoc.txt")

        capture_a <- summary(fit)
        # str(capture_a)
        capture.output(capture_a, file = txtSumName)
        capture_b <- fit
        capture.output(capture_b, file = txtResultsName)
        postHoc <<- TukeyHSD(fit)
        capture_c <- postHoc
        capture.output(capture_c, file = txtPostHocName)
        viewFullFactorANOVA <- boxplot(gsiList ~ strength * behavior, data=tempStatDataframe)
        
        viewFullFactorANOVA

        printThis <<- summary(fit)[[1]][["Pr(>F)"]]
      
        fit
        postHoc
        graphed <- plot(postHoc)
        
        graphed 


}


calculatePairedBetweenANOVAstats <- function(attractXlightfile25, attractXlightfile50, attractXlightfile75, attractXlightfile100, alignXlightfile25, alignXlightfile50, alignXlightfile75, alignXlightfile100, attractXalign25, attractXalign50, attractXalign75, attractXalign100, attractXavoid25, attractXavoid50, attractXavoid75, attractXavoid100, avoidXalign25, avoidXalign50, avoidXalign75, avoidXalign100, avoidXlight25, avoidXlight50, avoidXlight75, avoidXlight100, whichPhase,title1) {

    if(whichPhase == "first") {
        attractXlighttemp25 <- c(attractXlightfile25$Trial1[1:200], attractXlightfile25$Trial2[1:200], attractXlightfile25$Trial3[1:200])
        attractXlighttemp50 <- c(attractXlightfile50$Trial1[1:200], attractXlightfile50$Trial2[1:200], attractXlightfile50$Trial3[1:200])
        attractXlighttemp75 <- c(attractXlightfile75$Trial1[1:200], attractXlightfile75$Trial2[1:200], attractXlightfile75$Trial3[1:200])
        attractXlighttemp100 <- c(attractXlightfile100$Trial1[1:200], attractXlightfile100$Trial3[1:200], attractXlightfile100$Trial3[1:200])

        alignXlighttemp25 <- c(alignXlightfile25$Trial1[1:200], alignXlightfile25$Trial2[1:200], alignXlightfile25$Trial3[1:200])
        alignXlighttemp50 <- c(alignXlightfile50$Trial1[1:200], alignXlightfile50$Trial2[1:200], alignXlightfile50$Trial3[1:200])
        alignXlightemp75 <- c(alignXlightfile75$Trial1[1:200], alignXlightfile75$Trial2[1:200], alignXlightfile75$Trial3[1:200])
        alignXlighttemp100 <- c(alignXlightfile100$Trial1[1:200], alignXlightfile100$Trial3[1:200], alignXlightfile100$Trial3[1:200])

        attractXaligntemp25 <- c(attractXalign25$Trial1[1:200], attractXalign25$Trial2[1:200], attractXalign25$Trial3[1:200])
        attractXaligntemp50 <- c(attractXalign50$Trial1[1:200],attractXalign50$Trial2[1:200],attractXalign50$Trial3[1:200])
        attractXaligntemp75 <- c(attractXalign75$Trial1[1:200], attractXalign75$Trial2[1:200], attractXalign75$Trial3[1:200])
        attractXaligntemp100 <- c(attractXalign100$Trial1[1:200], attractXalign100$Trial3[1:200], attractXalign100$Trial3[1:200])

        attractXavoidtemp25 <- c(attractXavoid25$Trial1[1:200], attractXavoid25$Trial2[1:200], attractXavoid25$Trial3[1:200])
        attractXavoidtemp50 <- c(attractXavoid50$Trial1[1:200], attractXavoid50$Trial2[1:200], attractXavoid50$Trial3[1:200])
        attractXavoidtemp75 <- c(attractXavoid75$Trial1[1:200], attractXavoid75$Trial2[1:200], attractXavoid75$Trial3[1:200])
        attractXavoidtemp100 <- c(attractXavoid100$Trial1[1:200], attractXavoid100$Trial3[1:200], attractXavoid100$Trial3[1:200])

        avoidXaligntemp25 <- c(avoidXalign25$Trial1[1:200], avoidXalign25$Trial2[1:200], avoidXalign25$Trial3[1:200])
        avoidXaligntemp50 <- c(avoidXalign50$Trial1[1:200], avoidXalign50$Trial2[1:200], avoidXalign50$Trial3[1:200])
        avoidXaligntemp75 <- c(avoidXalign75$Trial1[1:200], avoidXalign75$Trial2[1:200], avoidXalign75$Trial3[1:200])
        avoidXaligntemp100 <- c(avoidXalign100$Trial1[1:200], avoidXalign100$Trial3[1:200], avoidXalign100$Trial3[1:200])

        avoidXlighttemp25 <- c(avoidXlight25$Trial1[1:200], avoidXlight25$Trial2[1:200], avoidXlight25$Trial3[1:200])
        avoidXlighttemp50 <- c(avoidXlight50$Trial1[1:200], avoidXlight50$Trial2[1:200], avoidXlight50$Trial3[1:200])
        avoidXlighttemp75 <- c(avoidXlight75$Trial1[1:200], avoidXlight75$Trial2[1:200], avoidXlight75$Trial3[1:200])
        avoidXlighttemp100 <- c(avoidXlight100$Trial1[1:200], avoidXlight100$Trial3[1:200], avoidXlight100$Trial3[1:200])


        tempStrength <- c(rep("str25",1800),rep("str25",1800),rep("str25", 1800),rep("str25",1800) ,rep("str25",1800) ,rep("str25",1800) ,
                       rep("str50",1800),rep("str50",1800),rep("str50", 1800),rep("str50",1800), rep("str50",1800),rep("str50",1800),
                       rep("str75",1800),rep("str75",1800),rep("str75", 1800),rep("str75",1800), rep("str75",1800),rep("str75",1800),
                       rep("str100",1800),rep("str100",1800),rep("str100", 1800),rep("str100",1800), rep("str100",1800), rep("str100",1800))
        tempLabel <- c(rep("attractXlight",600),rep("alignXlight",600),rep("attractXalign", 600),rep("attractionXavoid",600), rep("avoidXalign",600),rep("avoidXlight",600),
                       rep("attractXlight",600),rep("alignXlight",600),rep("attractXalign", 600),rep("attractionXavoid",600), rep("avoidXalign",600),rep("avoidXlight",600),
                       rep("attractXlight",600),rep("alignXlight",600),rep("attractXalign", 600),rep("attractionXavoid",600), rep("avoidXalign",600),rep("avoidXlight",600),
                       rep("attractXlight",600),rep("alignXlight",600),rep("attractXalign", 600),rep("attractionXavoid",600), rep("avoidXalign",600),rep("avoidXlight",600) )
        tempTotal <- c(attractXlighttemp25[1:600], alignXlighttemp25[1:600], attractXaligntemp25[1:600],attractXavoidtemp25[1:600],avoidXaligntemp25[1:600],avoidXlighttemp25[1:600],
                        attractXlighttemp50[1:600], alignXlighttemp50[1:600], attractXaligntemp50[1:600],attractXavoidtemp50[1:600],avoidXaligntemp50[1:600],avoidXlighttemp50[1:600],
                        attractXlighttemp75[1:600], alignXlightemp75[1:600], attractXaligntemp75[1:600],attractXavoidtemp75[1:600],avoidXaligntemp75[1:600],avoidXlighttemp75[1:600],
                        attractXlighttemp100[1:600], alignXlighttemp100[1:600], attractXaligntemp100[1:600],attractXavoidtemp100[1:600],avoidXaligntemp100[1:600],avoidXlighttemp100[1:600])

      } else if (whichPhase =="second") {

        attractXlighttemp25 <- c(attractXlightfile25$Trial1[200:800], attractXlightfile25$Trial2[200:800], attractXlightfile25$Trial3[200:800])
        attractXlighttemp50 <- c(attractXlightfile50$Trial1[200:800], attractXlightfile50$Trial2[200:800], attractXlightfile50$Trial3[200:800])
        attractXlighttemp75 <- c(attractXlightfile75$Trial1[200:800], attractXlightfile75$Trial2[200:800], attractXlightfile75$Trial3[200:800])
        attractXlighttemp100 <- c(attractXlightfile75$Trial1[200:800], attractXlightfile75$Trial3[200:800], attractXlightfile75$Trial3[200:800])
        
        alignXlighttemp25 <- c(alignXlightfile25$Trial1[200:800], alignXlightfile25$Trial2[200:800], alignXlightfile25$Trial3[200:800])
        alignXlighttemp50 <- c(alignXlightfile50$Trial1[200:800], alignXlightfile50$Trial2[200:800], alignXlightfile50$Trial3[200:800])
        alignXlightemp75 <- c(alignXlightfile75$Trial1[200:800], alignXlightfile75$Trial2[200:800], alignXlightfile75$Trial3[200:800])
        alignXlighttemp100 <- c(alignXlightfile100$Trial1[200:800], alignXlightfile100$Trial3[200:800], alignXlightfile100$Trial3[200:800])

        attractXaligntemp25 <- c(attractXalign25$Trial1[200:800], attractXalign25$Trial2[200:800], attractXalign25$Trial3[200:800])
        attractXaligntemp50 <- c(attractXalign50$Trial1[200:800],attractXalign50$Trial2[200:800],attractXalign50$Trial3[200:800])
        attractXaligntemp75 <- c(attractXalign75$Trial1[200:800], attractXalign75$Trial2[200:800], attractXalign75$Trial3[200:800])
        attractXaligntemp100 <- c(attractXalign100$Trial1[200:800], attractXalign100$Trial3[200:800], attractXalign100$Trial3[200:800])

        attractXavoidtemp25 <- c(attractXavoid25$Trial1[200:800], attractXavoid25$Trial2[200:800], attractXavoid25$Trial3[200:800])
        attractXavoidtemp50 <- c(attractXavoid50$Trial1[200:800], attractXavoid50$Trial2[200:800], attractXavoid50$Trial3[200:800])
        attractXavoidtemp75 <- c(attractXavoid75$Trial1[200:800], attractXavoid75$Trial2[200:800], attractXavoid75$Trial3[200:800])
        attractXavoidtemp100 <- c(attractXavoid100$Trial1[200:800], attractXavoid100$Trial3[200:800], attractXavoid100$Trial3[200:800])

        avoidXaligntemp25 <- c(avoidXalign25$Trial1[200:800], avoidXalign25$Trial2[200:800], avoidXalign25$Trial3[200:800])
        avoidXaligntemp50 <- c(avoidXalign50$Trial1[200:800], avoidXalign50$Trial2[200:800], avoidXalign50$Trial3[200:800])
        avoidXaligntemp75 <- c(avoidXalign75$Trial1[200:800], avoidXalign75$Trial2[200:800], avoidXalign75$Trial3[200:800])
        avoidXaligntemp100 <- c(avoidXalign100$Trial1[200:800], avoidXalign100$Trial3[200:800], avoidXalign100$Trial3[200:800])

        avoidXlighttemp25 <- c(avoidXlight25$Trial1[200:800], avoidXlight25$Trial2[200:800], avoidXlight25$Trial3[200:800])
        avoidXlighttemp50 <- c(avoidXlight50$Trial1[200:800], avoidXlight50$Trial2[200:800], avoidXlight50$Trial3[200:800])
        avoidXlighttemp75 <- c(avoidXlight75$Trial1[200:800], avoidXlight75$Trial2[200:800], avoidXlight75$Trial3[200:800])
        avoidXlighttemp100 <- c(avoidXlight100$Trial1[200:800], avoidXlight100$Trial3[200:800], avoidXlight100$Trial3[200:800])



        tempLabel <- c(rep("attractXlight",600),rep("alignXlight",600),rep("attractXalign", 600),rep("attractionXavoid",600), rep("avoidXalign",600),rep("avoidXlight",600),
                       rep("attractXlight",600),rep("alignXlight",600),rep("attractXalign", 600),rep("attractionXavoid",600), rep("avoidXalign",600),rep("avoidXlight",600),
                       rep("attractXlight",600),rep("alignXlight",600),rep("attractXalign", 600),rep("attractionXavoid",600), rep("avoidXalign",600),rep("avoidXlight",600),
                       rep("attractXlight",600),rep("alignXlight",600),rep("attractXalign", 600),rep("attractionXavoid",600), rep("avoidXalign",600),rep("avoidXlight",600) )

        tempStrength <- c(rep("str25",1800),rep("str25",1800),rep("str25", 1800),rep("str25",1800) ,rep("str25",1800) ,rep("str25",1800) ,
                       rep("str50",1800),rep("str50",1800),rep("str50", 1800),rep("str50",1800), rep("str50",1800),rep("str50",1800),
                       rep("str75",1800),rep("str75",1800),rep("str75", 1800),rep("str75",1800), rep("str75",1800),rep("str75",1800),
                       rep("str100",1800),rep("str100",1800),rep("str100", 1800),rep("str100",1800), rep("str100",1800), rep("str100",1800))
        # tempTotal <- c(temp25[1:1800],temp50[1:1800],temp75[1:1800],temp100[1:1800])

        tempTotal <- c(attractXlighttemp25[1:1800], alignXlighttemp25[1:1800], attractXaligntemp25[1:1800],attractXavoidtemp25[1:1800],avoidXaligntemp25[1:1800],avoidXlighttemp25[1:1800],
                        attractXlighttemp50[1:1800], alignXlighttemp50[1:1800],attractXaligntemp50[1:1800],attractXavoidtemp50[1:1800],avoidXaligntemp50[1:1800],avoidXlighttemp50[1:1800],
                        attractXlighttemp75[1:1800], alignXlightemp75[1:1800], attractXaligntemp75[1:1800],attractXavoidtemp75[1:1800],avoidXaligntemp75[1:1800],avoidXlighttemp75[1:1800],
                        attractXlighttemp100[1:1800], alignXlighttemp100[1:1800], attractXaligntemp100[1:1800],attractXavoidtemp100[1:1800],avoidXaligntemp100[1:1800],avoidXlighttemp100[1:1800])


      }  


      
      # length(tempTotal)
      # if(betweenORwithin == "within") {
      tempStatDataframe <- data.frame(
        behavior = tempLabel,
        gsiList = tempTotal,
        strength = tempStrength
      )

        fit <- aov(gsiList ~ strength * behavior, data=tempStatDataframe)

        txtSumName <- paste("./",title1,"_Anova_summary.txt")
        txtResultsName <- paste("./",title1,"_Anova_results.txt")
        txtPostHocName <- paste("./",title1,"_Anova_PostHoc.txt")

        capture_a <- summary(fit)
        # str(capture_a)
        capture.output(capture_a, file = txtSumName)
        capture_b <- fit
        capture.output(capture_b, file = txtResultsName)
        postHoc <<- TukeyHSD(fit)
        capture_c <- postHoc
        capture.output(capture_c, file = txtPostHocName)
        viewFullFactorANOVA <- boxplot(gsiList ~ strength * behavior, data=tempStatDataframe)
        
        viewFullFactorANOVA

        printThis <<- summary(fit)[[1]][["Pr(>F)"]]
        printThis
        fit
        postHoc
        graphed <- plot(postHoc)
        
        graphed 

}



PerformWithinStats <- function(phase) {
  calculateWithinANOVAstats(alignment_25.data, alignment_5.data, alignment_75.data, alignment_100.data, "alignStatsResults", phase)
  calculateWithinANOVAstats(attraction_25.data, attraction_5.data, attraction_75.data, attraction_100.data, "attractionStatsResults", phase)
  calculateWithinANOVAstats(avoidance_25.data, avoidance_5.data, avoidance_75.data, avoidance_100.data, "avoidanceStatsResults", phase)
  calculateWithinANOVAstats(light_25.data, light_5.data, light_75.data, light_100.data, "lightStatsResults", phase)
  calculateWithinANOVAstats(boid_25.data, boid_5.data, boid_75.data, boid_100.data, "boidStatsResults", phase)

} 


PerformBetweenStats <- function(phase, pairedORsingle) {
  # attract align light avoidance boid 
  # calculateBetweenANOVAstats(
  #   attraction_25.data,attraction_5.data,attraction_75.data,attraction_100.data,
  #   alignment_25.data,alignment_5.data,alignment_75.data,alignment_100.data, 
  #   light_25.data,light_5.data, light_75.data,light_100.data, 
  #   avoidance_25.data,avoidance_5.data,avoidance_75.data,avoidance_100.data,
  #   boid_25.data,boid_5.data,boid_75.data,boid_100.data, phase, "WithinPhase_CrossBehavior"
  #   )
  if(pairedORsingle=="single") {
    calculateBetweenANOVAstats(
      attraction_25.data,attraction_5.data,attraction_75.data,attraction_100.data,
      alignment_25.data,alignment_5.data,alignment_75.data,alignment_100.data, 
      light_25.data,light_5.data, light_75.data,light_100.data, 
      avoidance_25.data,avoidance_5.data,avoidance_75.data,avoidance_100.data,
      boid_25.data,boid_5.data,boid_75.data,boid_100.data, phase, "WithinPhase_CrossBehavior"
    )
    # attractXlightfile25, attractXlightfile50, attractXlightfile75, attractXlightfile75, alignXlightfile25, alignXlightfile50, alignXlightfile75, alignXlightfile100, attractXalign25, attractXalign50, attractXalign75, attractXalign100, attractXavoid25, attractXavoid50, attractXavoid75, attractXavoid100, avoidXalign25, avoidXalign50, avoidXalign75, avoidXalign100, avoidXlight25, avoidXlight50, avoidXlight75, avoidXlight100, whichPhase,title1
  } else if(pairedORsingle=="paired") {
    # attractXlightfile25, , alignXlightfile25,  attractXalign25, attractXavoid25,  avoidXalign25,  avoidXlight25, , whichPhase,title1
    calculatePairedBetweenANOVAstats(
      attractXlight_25.data, attractXavoid_5.data, attractXavoid_75.data, attractXavoid_100.data,
      alignXlight_25.data, alignXlight_5.data, alignXlight_75.data, alignXlight_100.data,
      attractXalign_25.data, attractXalign_5.data, attractXalign_75.data, attractXalign_100.data,
      attractXavoid_25.data, attractXavoid_5.data, attractXavoid_75.data, attractXavoid_100.data,
      avoidXalign_25.data, avoidXalign_5.data, avoidXalign_75.data, avoidXalign_100.data,
      avoidXlight_25.data, avoidXlight_5.data, avoidXlight_75.data, avoidXlight_100.data, phase, "WithinPase_InteractionBehaviors"
      )
  }
    
}  



PerformBetweenStats("second", "paired")
# printThis
# PerformWithinStats("second")

graphGSIByStrength("second")
allPairedBehaviorsGraph

# alignPlot
# attractionPlot
# avoidPlot
# boidPlot
# lightPlot
# allBehaviorsGraph