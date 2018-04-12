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

       
        tempLabel <- c(rep("attract25",600),rep("align25",600),rep("light25", 600),rep("avoid25",600),rep("boid25",600),
                       rep("attract50",600),rep("align50",600),rep("light50", 600),rep("avoid50",600),rep("boid50",600),
                       rep("attract75",600),rep("align75",600),rep("light75", 600),rep("avoid75",600),rep("boid75",600),
                       rep("attract100",600),rep("align100",600),rep("light100", 600),rep("avoid100",600),rep("boid100",600) )
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

      
        tempLabel <- c(rep("attract25",1800),rep("align25",1800),rep("ligh25", 1800),rep("avoid25",1800) ,rep("boid25",1800) ,
                       rep("attract50",1800),rep("align50",1800),rep("light50", 1800),rep("avoid50",1800), rep("boid50",1800),
                       rep("attract75",1800),rep("align75",1800),rep("light75", 1800),rep("avoid75",1800), rep("boid75",1800),
                       rep("attract100",1800),rep("align100",1800),rep("light100", 1800),rep("avoid100",1800), rep("boid100",1800))

        # tempTotal <- c(temp25[1:1800],temp50[1:1800],temp75[1:1800],temp100[1:1800])

        tempTotal <- c(attracttemp25[1:1800], aligntemp25[1:1800],lighttemp25[1:1800],avoidtemp25[1:1800],boidtemp25[1:1800],
                        attracttemp50[1:1800], aligntemp50[1:1800],lighttemp50[1:1800],avoidtemp50[1:1800],boidtemp50[1:1800],
                        attracttemp75[1:1800], aligntemp75[1:1800],lighttemp75[1:1800],avoidtemp75[1:1800],boidtemp75[1:1800],
                        attracttemp100[1:1800], aligntemp100[1:1800],lighttemp100[1:1800],avoidtemp100[1:1800],boidtemp100[1:1800])

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

        txtSumName <- paste("./",title1,"_Anova_summary.txt")
        txtResultsName <- paste("./",title1,"_Anova_results.txt")
        txtPostHocName <- paste("./",title1,"_Anova_PostHoc.txt")

        capture_a <- summary(fit)
        capture.output(capture_a, file = txtSumName)
        capture_b <- fit
        capture.output(capture_b, file = txtResultsName)
        postHoc <- TukeyHSD(fit)
        capture_c <- postHoc
        capture.output(capture_c, file = txtPostHocName)

        

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
  

PerformBetweenStats <- function(phase) {
  # attract align light avoidance boid 
  calculateBetweenANOVAstats(
    attraction_25.data,attraction_5.data,attraction_75.data,attraction_100.data,
    alignment_25.data,alignment_5.data,alignment_75.data,alignment_100.data, 
    light_25.data,light_5.data, light_75.data,light_100.data, 
    avoidance_25.data,avoidance_5.data,avoidance_75.data,avoidance_100.data,
    boid_25.data,boid_5.data,boid_75.data,boid_100.data, phase, "WithinPhase_CrossBehavior"
    )
}  



PerformBetweenStats("first")
PerformWithinStats("second")




