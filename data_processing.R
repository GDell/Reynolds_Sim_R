library(ggplot2)

alignment_25.data <- read.csv(file="./data/alignment/align25Validation.csv")
alignment_5.data <- read.csv(file="./data/alignment/align50Validation.csv")
alignment_75.data <- read.csv(file="./data/alignment/align75Validation.csv")
alignment_100.data <- read.csv(file="./data/alignment/align100Validation.csv")


# alignmentDataT <- c(alignment_100.data, alignment_75.data, alignment_5.data, alignment_25.data)


avoidance_25.data <- read.csv(file="./data/avoidance/avoidance25Validation.csv")
avoidance_5.data <- read.csv(file="./data/avoidance/avoidance50Validation.csv")
avoidance_75.data <- read.csv(file="./data/avoidance/avoidance75Validation.csv")
avoidance_100.data <- read.csv(file="./data/avoidance/avoidance100Validation.csv")

attraction_25.data <- read.csv(file="./data/attraction/attraction25Validation.csv")
attraction_5.data <- read.csv(file="./data/attraction/attraction50Validation.csv")
attraction_75.data <- read.csv(file="./data/attraction/attraction75Validation.csv")
attraction_100.data <- read.csv(file="./data/attraction/attraction100Validation.csv")

light_25.data <- read.csv(file="./data/lightAttraction/light25Validation.csv")
light_5.data <- read.csv(file="./data/lightAttraction/light50Validation.csv")
light_75.data <- read.csv(file="./data/lightAttraction/light75Validation.csv")
light_100.data <- read.csv(file="./data/lightAttraction/light100Validation.csv")

boid_25.data <- read.csv(file="./data/boidSim/boid50Validation.csv")
boid_5.data <- read.csv(file="./data/boidSim/boid25Validation.csv")
boid_75.data <- read.csv(file="./data/boidSim/boid75Validation.csv")
boid_1.data <- read.csv(file="./data/boidSim/boid1Validation.csv")


returnTrialAvg <- function(dataEx) {
  combinedTrialData <- cbind(dataEx$Trial1, dataEx$Trial2, dataEx$Trial3)  
  overallAvg <- cbind(dataEx$Trial1, dataEx$Trial2, dataEx$Trial3)
  stdev <- sd(combinedTrialData)
  return(c(overallAvg, stdev))
}

returnGSI <- function(dataFile, whichPhase) {
  if(whichPhase == "first") {
    tempGSI <- dataFile$GSI[1:200]
  } else if(whichPhase == "second") {
    tempGSI <- dataFile$GSI[200:800]
  } else if(whichPhase == "full") {
    tempGSI <- dataFile$dataFile$GSI[1:800]
  }
  return(tempGSI)
}



determineAverage <- function(dataFileArray) {
  meansArray <- c()
  temp <- 1
  for(gsiLog in dataFileArray) {
    currentMean <- mean(gsiLog)
    meansArray[temp] <- currentMean
    temp = temp + 1
  }
  return(meansArray)
}



GSI_avoid5 <- avoidance_5.dataT1$GSI[1:800]
GSI_align5 <- alignment_5.dataT1$GSI[1:800]
GSI_boid5 <- boid_5.dataT1$GSI[1:800]
GSI_attract5 <- attraction_5.dataT1$GSI[1:800]
GSI_light5 <- light_5.dataT1$GSI[1:800]

GSI_behaviorVal5 <-
  data.frame(
    AVOID = GSI_avoid5,
    ALIGN = GSI_align5,
    BOID = GSI_boid5,
    LIGHT = GSI_attract5,
    ATTRACTION = GSI_attract5,
    Time = 1:800
  )

GSIbehaviors5 <- ggplot(GSI_behaviorVal5, aes(Time, y = GSI)) + 
  geom_line(aes(y = GSI_avoid5, colour = "AVOIDANCE"), size =2) +  
  geom_line(aes(y = GSI_align5, colour = "ALIGNMENT"), size =2)+ 
  geom_line(aes(y = GSI_boid5, colour = "BOID"), size =2) +
  geom_line(aes(y = GSI_attract5, colour = "ATTRACTION"), size =2) +
  geom_line(aes(y = GSI_light5, colour = "LIGHT"), size =2)

GSIbehaviors5 + ggtitle("Behavior Stabilities")

