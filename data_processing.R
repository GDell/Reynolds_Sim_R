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

lightDataCollection <- c(light_25.data, light_5.data, light_75.data, light_100.data)

boid_25.data <- read.csv(file="./data/boidSim/boid50Validation.csv")
boid_5.data <- read.csv(file="./data/boidSim/boid25Validation.csv")
boid_75.data <- read.csv(file="./data/boidSim/boid75Validation.csv")
boid_100.data <- read.csv(file="./data/boidSim/boid100Validation.csv")

boidDataCollection <- c(boid_25.data, boid_5.data, boid_75.data, boid_100.data)






# determineAverage <- function(dataFileArray) {
#   meansArray <- c()
#   temp <- 1
#   for(gsiLog in dataFileArray) {
#     currentMean <- mean(gsiLog)
#     meansArray[temp] <- currentMean
#     temp <- temp + 1
#   }
#   return(meansArray)
# }


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
  
  error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
   if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
   stop("vectors must be same length")
   arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
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
   

 


  # tempStrengthData <- data.frame(
  #   plotD <- pointsLog,
  #   plotDstd <- stdLog,
  #   # Strength_.25 <- point25,
  #   # Strengh_.50 <- point50,
  #   # Strength_.75 <- point75,
  #   # Strength_1.0 <- point100,
  #   gsi <- c(1,2,3,4,5,6,7,8,10) ,
  #   Strength <- c(25,50,75,100)
  #   )


  # strengthMag <- ggplot(tempStrengthData, aes(Strength, y <- GSI)) + 
  # geom_line(aes(y <- c(point25,point50, point75, point100)), size =2)   
  

  # points <- c(point25,point50, point75, point100)
  # tempPlot <- plot(points, type="o", col="blue", xlab="Behavior Strength", ylab="GSI", axes=FALSE, main=behaviorTitle, ylim=c(0.:1.0))
  # axis(1, at=1:4, lab=c("25","50","75","100"))
  # axis(2, las=1, at=10.0*((1:100)/100))
  # error.bar(tempPlot, pointsLog, stdLog)
  # plotReturn <- ggplot(tempStrengthData, aes(x<- Strength,y<-gsi)) +
          # geom_line(aes(plotD), size=2)
        # geom_line(aes(y <- plotD), size = 2) 
    # geom_errorbar() + aes(ymin = plotD - plotDstd, ymax = plotD + ymax)

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


  # plotReturn
  # title(main=behaviorTitle)
  # axis(1, at=1:4, )
  # geom_line(aes(y <- GSI_align5, colour <- "ALIGNMENT"), size =2)+ 
  # geom_line(aes(y <- GSI_boid5, colour <- "BOID"), size =2) +
  # geom_line(aes(y <- GSI_attract5, colour <- "ATTRACTION"), size =2) +
  # geom_line(aes(y <- GSI_light5, colour <- "LIGHT"), size =2)

  # strengthMag + ylim(0.0,1.0)

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

  # createBehaviorRangeGraph(light25result[1], light25result[2], light50result[1], light50result[2], light75result[1], light75result[2], light100result[1], light100result[2], "Light Attraction")

  # png(filename="Std_PNG.png", 
  #     units="in", 
  #     width=5, 
  #     height=4, 
  #     pointsize=12, 
  #     res=72)
  # my_sc_plot(data)
  # dev.off()

  
  alignPlot <<- createBehaviorRangeGraph(align25result[1], align25result[2], align50result[1], align50result[2], align75result[1], align75result[2], align100result[1], align100result[2], "Social Alignment")
  attractionPlot <<- createBehaviorRangeGraph(attraction25result[1], attraction25result[2], attraction50result[1], attraction50result[2], attraction75result[1], attraction75result[2], attraction100result[1], attraction100result[2], "Social Attraction")
  avoidPlot <<- createBehaviorRangeGraph(avoidance25result[1], avoidance25result[2], avoidance50result[1], avoidance50result[2], avoidance75result[1], avoidance75result[2], avoidance100result[1], avoidance100result[2], "Social Avoidance")
  boidPlot <<- createBehaviorRangeGraph(boid25result[1], boid25result[2], boid50result[1], boid50result[2], boid75result[1], boid75result[2], boid100result[1], boid100result[2], "Boid Flock")

}

# graphGSIByStrength("first")
# graphGSIByStrength("first")
# graphGSIByStrength("first")
# graphGSIByStrength("first")

graphGSIByStrength("second")
# graphGSIByStrength("second")
# graphGSIByStrength("second")
# graphGSIByStrength("second")
alignPlot
attractionPlot
avoidPlot
boidPlot



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


