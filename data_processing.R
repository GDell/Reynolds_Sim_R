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



  # str25, str50,str75, str100)
  # print(y)




  lightPlot <<- createBehaviorRangeGraph(light25result[1], light25result[2], light50result[1], light50result[2], light75result[1], light75result[2], light100result[1], light100result[2], "Light Attraction")
  alignPlot <<- createBehaviorRangeGraph(align25result[1], align25result[2], align50result[1], align50result[2], align75result[1], align75result[2], align100result[1], align100result[2], "Social Alignment")
  attractionPlot <<- createBehaviorRangeGraph(attraction25result[1], attraction25result[2], attraction50result[1], attraction50result[2], attraction75result[1], attraction75result[2], attraction100result[1], attraction100result[2], "Social Attraction")
  avoidPlot <<- createBehaviorRangeGraph(avoidance25result[1], avoidance25result[2], avoidance50result[1], avoidance50result[2], avoidance75result[1], avoidance75result[2], avoidance100result[1], avoidance100result[2], "Social Avoidance")
  boidPlot <<- createBehaviorRangeGraph(boid25result[1], boid25result[2], boid50result[1], boid50result[2], boid75result[1], boid75result[2], boid100result[1], boid100result[2], "Boid Flock")

}

graphGSIByStrength("first")
# graphGSIByStrength("first")
# graphGSIByStrength("first")
# graphGSIByStrength("first")

# graphGSIByStrength("second")
# graphGSIByStrength("second")
# graphGSIByStrength("second")
# graphGSIByStrength("second")
# alignPlot
# attractionPlot
# avoidPlot
# boidPlot
# lightPlot


calculateWithinStats <- function(file25, file50, file75, file100, title, whichPhase) {



      

      # length(temp25)

      if(whichPhase == "first") {
        temp25 <- c(file25$Trial1[1:200], file25$Trial3[1:200], file25$Trial3[1:200])
        temp50 <- c(file50$Trial1[1:200], file50$Trial3[1:200], file50$Trial3[1:200])
        temp75 <- c(file75$Trial1[1:200], file75$Trial3[1:200], file75$Trial3[1:200])
        temp100 <- c(file100$Trial1[1:200], file100$Trial3[1:200], file100$Trial3[1:200])
       
        tempLabel <- c(rep("str25",600),rep("str50",600),rep("str75", 600),rep("str100",600))
        tempTotal <- c(temp25[1:600],temp50[1:600],temp75[1:600],temp100[1:600])

      } else if (whichPhase =="second") {
        temp25 <- c(file25$Trial1[200:800], file25$Trial3[200:800], file25$Trial3[200:800])
        temp50 <- c(file50$Trial1[200:800], file50$Trial3[200:800], file50$Trial3[200:800])
        temp75 <- c(file75$Trial1[200:800], file75$Trial3[200:800], file75$Trial3[200:800])
        temp100 <- c(file100$Trial1[200:800], file100$Trial3[200:800], file100$Trial3[200:800])
        

         tempLabel <- c(rep("str25",1800),rep("str50",1800),rep("str75", 1800),rep("str100",1800))
        tempTotal <- c(temp25[1:1800],temp50[1:1800],temp75[1:1800],temp100[1:1800])

      } else if( whichPhase =="full") {

        temp25 <- c(file25$Trial1, file25$Trial3, file25$Trial3)
        temp50 <- c(file50$Trial1, file50$Trial3, file50$Trial3)
        temp75 <- c(file75$Trial1, file75$Trial3, file75$Trial3)
        temp100 <- c(file100$Trial1, file100$Trial3, file100$Trial3)


        tempLabel <- c(rep("str25",2400),rep("str50",2400),rep("str75", 2400),rep("str100",2400))
        tempTotal <- c(temp25[1:2400],temp50[1:2400],temp75[1:2400],temp100[1:2400])
      }
     
      # length(tempTotal)

      lightStats <- data.frame(
        # str25 = temp25[1:2400],
        # str50 = temp50[1:2400],
        # str75 = temp75[1:2400],
        # str100 = temp100[1:2400],
        type = tempLabel,
        gsiList = tempTotal
      )


    fit <- aov(gsiList ~ type, data=lightStats)

    txtSumName <- paste("./",title,"_Anova_summary.txt")
    txtResultsName <- paste("./",title,"_Anova_results.txt")
    txtPostHocName <- paste("./",title,"_Anova_PostHoc.txt")

    capture_a <- summary(fit)
    capture.output(capture_a, file = txtSumName)
    capture_b <- fit
    capture.output(capture_b, file = txtResultsName)
    capture_c <- fit
    capture.output(capture_c, file = txtPostHocName)

    postHoc <- TukeyHSD(fit)

    fit
    postHoc
    graphed <- plot(postHoc)
    
    graphed 
    
    # fileLoc <- "./results.txt"
    # toWrite <- ""
    # #sumOfStats <- summary(fit)
    # toWrite <- paste(toWrite, fit , "\t")

    # toWrite <- paste(toWrite,model.tables(fit,"means"),"\t")
    #print(model.tables(fit,"effects"))
    # write.table(toWrite, fileLoc, sep="\t")
}


# calculateAcrossStats

      
      # print(temp25)


    
calculateWithinStats(alignment_25.data, alignment_5.data, alignment_75.data, alignment_100.data, "alignStatsResults", "first")

calculateWithinStats(attraction_25.data, attraction_5.data, attraction_75.data, attraction_100.data, "attractionStatsResults", "first")

calculateWithinStats(avoidance_25.data, avoidance_5.data, avoidance_75.data, avoidance_100.data, "avoidanceStatsResults", "first")

calculateWithinStats(light_25.data, light_5.data, light_75.data, light_100.data, "lightStatsResults", "first")

calculateWithinStats(boid_25.data, boid_5.data, boid_75.data, boid_100.data, "boidStatsResults", "first")
    # y <- A
    # One Way Anova (Completely Randomized Design)
    
# graphGSIByStrength("second")

# lightPlot



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


