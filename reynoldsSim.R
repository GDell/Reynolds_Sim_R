
# THE FOLLOWING IS A 2-D VARIANT OF CRAIG REYNOLDS' BOID MODEL.
# Written by Gabriel Dell'Accio
# Algorithm inspired by Craig Reynold's BOID algorithm and Conrad Parker's BOIDs pseudocode at: 
# http://www.kfish.org/boids/pseudocode.html

# Overview:
#   The time.stepper() function at the end of the script takes in the number of iterations you want to run the model for.    
#
#   Rules for swarming
#     1) social attraction: Boids are attracted to the center of the overall swarm.
#     2) Avoidance rule: Boids avoid collision with other boids by avoiding coming within
#        a certain distance of one another.
#     3) Match Velocity: Boids can sense the average velocity of the swarm and do their 
#        best to match it.
# 
#    GSI: Group Stability Index 
#     # This is an index developed by Baldessare and colleagues (2003) to measure the stability of a 
#     # swarm in their paper "Evolving mobile robots  able to display collective behaviors"


# LIBRARIES:
library(ggplot2)

# ARENA PARAMETERS:
  # Variable to hold the x,y center coordinate of the graph size.
  xLength <- 2000.0
  yLength <- 2000.0


  centerCor <- rep(xLength/2,2)
# SWARM PARAMETERS:
  # number of individuals
  nIndividuals <- 100
  # velocity that all Boid's will begin with. 
  baseVelocity <- 10
  # a value that increases the size of the distribution of initial locations for the swarm. 
  spatialDistribution <- 300
  # the number of iterations the model is run for.
  step.iterations <- 30

  # Vectors to hold the starting x,y velocities for the swarm.
  xVelocity <- rep(baseVelocity, nIndividuals)
  yVelocity <- rep(baseVelocity, nIndividuals)

  # This function creates the arena.
  create.arena <- function(xLength, yLength) {
    
    # length of x plane 
    xLimit <- xLength 
    # Length of y plane
    yLimit <- yLength
    
    # temporary data frame that holds the data 
    # that creates the borders of the 2D arena. 
    arena.Data <<- data.frame(
      yWall = rep(1,yLimit),
      xWall = c(1:xLimit),
      xNorthWall = c(1:xLimit),
      yNorthWall = rep(yLimit,yLimit)
    )

    # Plot Arena Borders
    arena <- ggplot(data = arena.Data, aes(x=xWall,y=yWall))
    arena <- arena + geom_point() +  ylim(1,yLimit) + xlim(1,xLimit) + geom_point(aes(xNorthWall,yNorthWall)) + geom_point(aes(rep(1,xLimit),c(1:yLimit))) + geom_point(aes(rep(xLimit,xLimit),c(1:yLimit)))

    return(arena)
  }
  arenaSim <- create.arena(xLength, yLength)

  # This function helps initialize the swarm and arena using a data frame, arena.data, 
  # that stores information about the swarm and the arena. 
  initialize.swarm <- function(spatial.distribution.factor,coordinateLocation) {
      arena.Data$index <<- c(1:nIndividuals)
      arena.Data$xPosition <<- rep(NA, xLength)
      arena.Data$yPosition <<- rep(NA, yLength)
    
    # Create the x,y position of each individual in the swarm by drawing from a 
    # random normal distribution around a given coordinate location in the arena. 
    for(var in 1:nIndividuals) {
      # velocityList[] <- c(baseVelocity,baseVelocity)
      arena.Data$xPosition[var] <<- rnorm(1, mean=coordinateLocation[1], sd=spatial.distribution.factor + 50)
      arena.Data$yPosition[var] <<- rnorm(1, mean=coordinateLocation[2], sd=spatial.distribution.factor + 50)
      # arena.Data$theta[var] <<- rnorm(1, mean = 180, sd= theta.distribution.factor + 5)
    }
  }

  # Call the initialize function that creates teh arena.data data frame 
  # with all previously declared swarm and arena parameters.
  initialize.swarm(spatialDistribution, centerCor)

  # Function that updates the current arena with the swarm's current location.
  display.swarm <- function() {
    return(arenaSim + geom_point(aes(arena.Data$xPosition, arena.Data$yPosition)))
  }

arenaSim <- display.swarm()
arenaSim

# GSI: functions for computing the group stability index
  # This function computes the distance between two x,y points in the 2D arena. 
  compute.cart.distance  <- function(x1,x2,y1,y2) {
    return(sqrt(((x1-x2)^2) + ((y1-y2)^2)) )
  }
  # This function calculates the total distance between all members of the flock. It returns the total distance between all individuals in one step of the arena.
  calculate.group.distance <- function() {
    groupedDistance <- 0
    for(var in 1:nIndividuals) {
      for(varTwo in 1:nIndividuals) {
         dist <- compute.cart.distance(arena.Data$xPosition[var], arena.Data$xPosition[varTwo], arena.Data$yPosition[var], arena.Data$yPosition[varTwo])
         groupedDistance <- groupedDistance + dist
      }
    }
    return(groupedDistance)
  }
  # This function takes in the previous time step's total group distance and the current time steps group distance to calculate the 
  # group stability over time.
  calculate.gsi <- function(groupedDistancePrev, groupedDistanceNext) {
    finalGsi <- 1 - (((abs(groupedDistancePrev - groupedDistanceNext)) / 4 ) / (nIndividuals*(nIndividuals-1)/2))
    return(finalGsi)
  }


# A function that returns the average x,y coordinate of the swarm.
find.center <- function() {

  totalX <- 0
  totalY <- 0


  for(var in 1:nIndividuals) {
    totalX <- totalX + arena.Data$xPosition[var] 
    totalY <- totalY + arena.Data$yPosition[var] 
  }

  averageX <- totalX / nIndividuals
  averageY <- totalY / nIndividuals

  centerAttraction <- c(averageX, averageY)

  return(centerAttraction)

}


# RULE 1: calculate the swarm's social attraction 
social.attraction <- function(boid, count) {

  totalX <- 0
  totalY <- 0

  # Find the average X,Y coordinate
  for(var in 1:nIndividuals) {
    totalX <- totalX + arena.Data$xPosition[var] 
    totalY <- totalY + arena.Data$yPosition[var] 
  }
  averageX <- totalX / nIndividuals
  averageY <- totalY / nIndividuals

  # calculate the the adjusted X,Y velocity paramter to move the Boid in the direction of the social attraction.
  centerAttraction <- c(((averageX - boid[1])/100 ), ((averageY - boid[2])/100))
  # print(centerAttraction)
  return(centerAttraction)
}


# RULE 2: calculate the avoidance correction for a boid   
avoidance.rule <- function(boid, count) {

  correctedCourse <- c(0,0)

  # for each individual ...
  for(var in 1:nIndividuals) {
    # If it it isnt the provided Boid...
    if(count != var) {
      # Check to see if any other individuals are within 100 spaces
      if((abs( arena.Data$xPosition[var] - boid[1]) < 100) && (abs(arena.Data$yPosition[var] - boid[2]) < 100)) {
        b.position <- c(arena.Data$xPosition[var], arena.Data$yPosition[var])
        # If there are individuals within this space, create a negative value to change the position of the Boid
        correctedCourse <- correctedCourse - (b.position - boid)
      } 
    }
  }
  # print(correctedCourse)

  return(correctedCourse)
}

# RULE 3: calculate the match velocity correction for a boid
match.velocity <- function(boid, count) {

  meanVelocityX <- 0 
  meanVelocityY <- 0

  # Calculate mean x,y velocities for the swarm 
  for(var in 1:nIndividuals) {
    meanVelocityX <- meanVelocityX + xVelocity[var]
    meanVelocityY <- meanVelocityY + yVelocity[var]  
  }
  meanVelocityX <- meanVelocityX / nIndividuals
  meanVelocityY <- meanVelocityY / nIndividuals

  # Calculate the individual boid's x,y correction 
  velocityCorrectionX <- (meanVelocityX - xVelocity[count]) /8
  velocityCorrectionY <- (meanVelocityY - xVelocity[count]) /8

  return(c(velocityCorrectionX, velocityCorrectionY))
}
 
# This function steps the swarm one step using the the 3 behavior rules 
# written above. 
compute.next.pos <- function() {

  # For each Boid...
  for(var in 1:nIndividuals) {

    # Store its current location
      currentIndividualLoc <- c(arena.Data$xPosition[var], arena.Data$yPosition[var])
      
      # Rule 1: find the average position of the swarm and calculate the individual's velocity 
      # parameter that determines how much it moves toward the center.
      social.attraction.rule <- social.attraction(currentIndividualLoc, var)
      # Rule 2: calculate the velocity parameter that ensures the individual avoids moving too close
      # to any one idividual within the swarm.
      avoidanceRule <- avoidance.rule(currentIndividualLoc, var)
      print(avoidanceRule)
      # Rule 3: calculate the average velocity of the swarm and find the velocity parameter that
      # that helps match the current individuals velocity to the swarm's average velocity.
      velocityRule <- match.velocity(currentIndividualLoc, var)
      
      # Apply the individulas current velocity to determine its next location.
      currentIndividualLoc <- currentIndividualLoc + c(xVelocity[var],yVelocity[var])

      # Change the individuals velocity as dedtermined by the 3 rules calculated above.
      xVelocity[var] <<- xVelocity[var] + social.attraction.rule[1] + avoidanceRule[1] + velocityRule[1]
      yVelocity[var] <<- yVelocity[var] + social.attraction.rule[2] + avoidanceRule[2] + velocityRule[2]

      # Change the individuals location
      arena.Data$xPosition[var] <<- currentIndividualLoc[1]
      arena.Data$yPosition[var] <<- currentIndividualLoc[2]
  }

}



# This function steps the swarm x iterations and tracks the group stability of the
# swarm in each time step. 
time.stepper <- function(iterations) {

  # Create a vector to hold a log of the change in group stability over time.
  gsiLog <- rep(0,iterations)

  # For i iterations ...
  for(i in 1:iterations) {
    
    # calculate total group distance at this time step 
    prevTotalDistance <- calculate.group.distance()

    # Compute and apply new positons for each individual in the swarm.
    compute.next.pos()

    # Calculate the total group distance after stepping the swarm.
    nextTotalDistance <- calculate.group.distance()

    # Calculate the change in group stability between the previous and current 
    # time step and log it in the gsiLog vector.
    gsiLog[i] <- calculate.gsi(prevTotalDistance, nextTotalDistance)

    # Display the swarm.
    plot(arena.Data$xPosition, arena.Data$yPosition)
    # Sleep in order to provide R studio the opportunity to load the plot. 
    Sys.sleep(.04)
  }
  return(gsiLog)
}

finalGsi <- time.stepper(step.iterations)

plot(finalGsi)


