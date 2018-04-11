library(ggplot2)

alignment_25.data <- read.csv(file="./data/alignment/align25Validation.csv")
alignment_5.data <- read.csv(file="./data/alignment/align50Validation.csv")
alignment_75.data <- read.csv(file="./data/alignment/align75Validation.csv")
alignment_100.data <- read.csv(file="./data/alignment/align100Validation.csv")


alignmentDataCollection <- c(alignment_25.data, alignment_5.data, alignment_75.data, alignment_100.data)


avoidance_25.data <- read.csv(file="./data/avoidance/avoidance25Validation.csv")
avoidance_5.data <- read.csv(file="./data/avoidance/avoidance50Validation.csv")
avoidance_75.data <- read.csv(file="./data/avoidance/avoidance75Validation.csv")
avoidance_100.data <- read.csv(file="./data/avoidance/avoidance100Validation.csv")

avoidanceDataCollection <- c(avoidance_25.data, avoidance_5.data, avoidance_75.data, avoidance_100.data)

attraction_25.data <- read.csv(file="./data/attraction/attraction25Validation.csv")
attraction_5.data <- read.csv(file="./data/attraction/attraction50Validation.csv")
attraction_75.data <- read.csv(file="./data/attraction/attraction75Validation.csv")
attraction_100.data <- read.csv(file="./data/attraction/attraction100Validation.csv")

attractionDataCollection <- c(attraction_25.data, attraction_5.data, attraction_75.data, attraction_100.data)

light_25.data <- read.csv(file="./data/lightAttraction/light25Validation.csv")
light_5.data <- read.csv(file="./data/lightAttraction/light50Validation.csv")
light_75.data <- read.csv(file="./data/lightAttraction/light75Validation.csv")
light_100.data <- read.csv(file="./data/lightAttraction/light100Validation.csv")

lightDataCollection < c(light_25.data, light_5.data, light_75.data, light_100.data)

boid_25.data <- read.csv(file="./data/boidSim/boid50Validation.csv")
boid_5.data <- read.csv(file="./data/boidSim/boid25Validation.csv")
boid_75.data <- read.csv(file="./data/boidSim/boid75Validation.csv")
boid_100.data <- read.csv(file="./data/boidSim/boid1Validation.csv")

boidDataCollection <- c(boid_25.data, boid_5.data, boid_75.data, boid_100.data)






determineAverage <- function(dataFileArray) {
  meansArray <- c()
  temp <- 1
  for(gsiLog in dataFileArray) {
    currentMean <- mean(gsiLog)
    meansArray[temp] <- currentMean
    temp <- temp + 1
  }
  return(meansArray)
}


returnTrialAvg <- function(dataEx, whichPhase) {
  overallAvg <- NA

   if(whichPhase == "first") {
    combinedTrialData <- cbind(dataEx$Trial1[1:200], dataEx$Trial2[1:200], dataEx$Trial3[1:200])  
    overallAvgerag <- overallAvg(combinedTrialData)
    stdev <- sd(combinedTrialData)
  } else if(whichPhase == "second") {
    combinedTrialData <- cbind(dataEx$Trial1[200:800], dataEx$Trial2[200:800], dataEx$Trial3[200:800])  
    overallAvgerag <- overallAvg(combinedTrialData)
    stdev <- sd(combinedTrialData)
  } else if(whichPhase == "full") {
    combinedTrialData <- cbind(dataEx$Trial1[1:800], dataEx$Trial2[1:800], dataEx$Trial3[1:800])  
    overallAvgerag <- overallAvg(combinedTrialData)
    stdev <- sd(combinedTrialData)
  }
  return(c(overallAvgerage, stdev))
}



# returnGSI <- function(dataFile, whichPhase) {
#   if(whichPhase == "first") {
#     tempGSI <- dataFile$GSI[1:200]
#   } else if(whichPhase == "second") {
#     tempGSI <- dataFile$GSI[200:800]
#   } else if(whichPhase == "full") {
#     tempGSI <- dataFile$dataFile$GSI[1:800]
#   }
#   return(tempGSI)
# }


createBehaviorRangeGraph <- function(point25, point50, point75, point100, behaviorTitle) {

  tempStrengthData <- data.frame(
    plotD <- c(point25,point50, point75, point100),
    # Strength_.25 <- point25,
    # Strengh_.50 <- point50,
    # Strength_.75 <- point75,
    # Strength_1.0 <- point100,
    Strength <- c(.25,.50,.75,1.00)
  )

  strengthMag <- ggplot(tempStrengthData, aes(Strength, y <- GSI)) + 
  geom_line(aes(y <- plotD, colour <- behaviorTitle), size =2)   
  # geom_line(aes(y <- GSI_align5, colour <- "ALIGNMENT"), size =2)+ 
  # geom_line(aes(y <- GSI_boid5, colour <- "BOID"), size =2) +
  # geom_line(aes(y <- GSI_attract5, colour <- "ATTRACTION"), size =2) +
  # geom_line(aes(y <- GSI_light5, colour <- "LIGHT"), size =2)

  GSIbehaviors5 + ggtitle(behaviorTitle)



}






graphAvgByMag <- function(alignData, avoidData, alignData, attractData, lightData, boidData, whichPhase) {

  light25result <- returnTrialAvg(lightData[1], "second")
  light50result <- returnTrialAvg(lightData[2], "second")
  light75result <- returnTrialAvg(lightData[3], "second")
  light100result <- returnTrialAvg(lightData[4], "second")

  lightPlotData <- c(light25result, light50result, light75result, light100result)

  align25result <- returnTrialAvg(alignData[1], "second")
  align50result <- returnTrialAvg(alignData[2], "second")
  align75result <- returnTrialAvg(alignData[3], "second")
  align100result <- returnTrialAvg(alignData[4], "second")

  alignPlotData <- c(align25result, align50result, align75result, align100result)

  attraction25result <- returnTrialAvg(attractData[1], "second")
  attraction50result <- returnTrialAvg(attractData[2], "second")
  attraction75result <- returnTrialAvg(attractData[3], "second")
  attraction100result <- returnTrialAvg(attractData[4], "second")

  attractPlotData <- c(attraction25result, attraction50result, attraction75result, attraction100result)

  avoidance25result <- returnTrialAvg(avoidData[1], "second")
  avoidance50result <- returnTrialAvg(avoidData[2], "second")
  avoidance75result <- returnTrialAvg(avoidData[3], "second")
  avoidance100result <- returnTrialAvg(avoidData[4], "second")

  avoidPlotData <- c(avoidance25result, avoidance50result, avoidance75result, avoidance100result)

  boid25result <- returnTrialAvg(boidData[1], "second")
  boid50result <- returnTrialAvg(boidData[2], "second")
  boid75result <- returnTrialAvg(boidData[3], "second")
  boid100result <- returnTrialAvg(boidData[4], "second")

  boidPlotData <- c(boid25result, boid50result, boid75result, boid100result)




}





# GSI_behaviorVal5 <-
#   data.frame(
#     AVOID <- GSI_avoid5,
#     ALIGN <- GSI_align5,
#     BOID <- GSI_boid5,
#     LIGHT <- GSI_attract5,
#     ATTRACTION <- GSI_attract5,
#     Time <- 1:800
#   )



# GSIbehaviors5 <- ggplot(GSI_behaviorVal5, aes(Time, y <- GSI)) + 
#   geom_line(aes(y <- GSI_avoid5, colour <- "AVOIDANCE"), size =2) +  
#   geom_line(aes(y <- GSI_align5, colour <- "ALIGNMENT"), size =2)+ 
#   geom_line(aes(y <- GSI_boid5, colour <- "BOID"), size =2) +
#   geom_line(aes(y <- GSI_attract5, colour <- "ATTRACTION"), size =2) +
#   geom_line(aes(y <- GSI_light5, colour <- "LIGHT"), size =2)

# GSIbehaviors5 + ggtitle("Behavior Stabilities")

