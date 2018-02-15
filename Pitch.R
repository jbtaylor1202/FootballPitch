library(ggplot2)

#Circle drawing function
circleFun <- function(xPosition, yPosition, radius, npoints = 500){
  tt <- seq(0,2*pi,length.out = npoints)
  xCoordinate <- radius * cos(tt) + xPosition
  yCoordinate <- radius * sin(tt) + yPosition
  return(data.frame(x = xCoordinate, y = yCoordinate))
}

#Pitch drawing function
drawPitch <-function(pitchLength, pitchWidth){
  ##Add note that dimensions are to be input in m
  print("Input pitch length and pitch width are assumed to be in metres.")
  
  errorCount <- 0
  #Validate length as 90 to 120m
  if(pitchLength < 90 | pitchLength > 120){
    warning("Offical pitch lengths should be between 90m and 120m"); 
    errorCount <- errorCount + 1}
  #Validate widt as 45 to 90 m
  if(pitchWidth < 45 | pitchWidth > 90){
    warning("Offical pitch widths should be between 45m and 90m"); 
    errorCount <- errorCount + 1}
    
  if(errorCount > 0){
    stop("Pitch dimensions do not meet official rules.", call. = FALSE)
  }
  
  
  #All distances here have been converted from yards to meters
  #pitchBackground <- expand.grid(0:pitchLength, 0:pitchWidth)
  CentreCircle <- circleFun(xPosition = pitchLength/2, yPosition = pitchWidth/2, radius = 9.144)
  bottomLeftCornerCircle <- circleFun(xPosition = 0, yPosition = 0, radius = 0.9144)
  bottomRightCornerCircle <- circleFun(xPosition = max(pitchLength), yPosition = 0, radius = 0.9144)
  topLeftCornerCircle <- circleFun(xPosition = 0, yPosition = max(pitchWidth), radius = 0.9144)
  topRightCornerCircle <- circleFun(xPosition = max(pitchLength), yPosition = max(pitchWidth), radius = 0.9144)
  leftPenaltySpot <- circleFun(xPosition = 10.9728, yPosition = pitchWidth/2, radius = 0.1)
  rightPenaltySpot <- circleFun(xPosition = max(pitchLength-10.9728), yPosition = pitchWidth/2, radius = 0.1)
  sixYardBoxes <- data.frame(x1 = c(0,pitchLength - 5.4864),
                             x2 = c(5.4864, pitchLength),
                             y1 = c((pitchWidth/2)+9.144, (pitchWidth/2)+9.144),
                             y2 = c((pitchWidth/2)-9.144, (pitchWidth/2)-9.144))
  eighteenYardBoxes <- data.frame(x1 = c(0,pitchLength - 16.4592),
                             x2 = c(16.4592, pitchLength),
                             y1 = c((pitchWidth/2)+20.1168, (pitchWidth/2)+20.1168),
                             y2 = c((pitchWidth/2)-20.1168, (pitchWidth/2)-20.1168))
  leftEighteenYardBoxArcs <-circleFun(xPosition = 10.9728, yPosition = pitchWidth/2, radius = 9.144)
  rightEighteenYardBoxArcs <-circleFun(xPosition = max(pitchLength-10.9728), yPosition = pitchWidth/2, radius = 9.144)
  pitchBoundaries <- data.frame(x1 = 0,
                                x2 = pitchLength,
                                y1 = 0,
                                y2 = pitchWidth)
  goals<-data.frame(x1 = c(0,pitchLength + 2),
                    x2 = c(-2, pitchLength),
                    y1 = c((pitchWidth/2)+3.6576, (pitchWidth/2)+3.6576),
                    y2 = c((pitchWidth/2)-3.6576, (pitchWidth/2)-3.6576))
  
  #Plot built in this order so that layers don't cover others to ensure all pitch markings can be seen.
  ggplot() +
    geom_rect(data = goals, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), colour = "black", fill = 'black', size = 2) +
    geom_rect(data = pitchBoundaries, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), colour = "white", fill = 'dark green') +
    geom_vline(aes(xintercept = pitchLength/2),size=0.5,colour='white') +
    geom_path(data = CentreCircle,mapping = aes(x,y), colour = 'white') +
    geom_path(data = bottomLeftCornerCircle ,mapping = aes(x,y), colour = 'white') +
    geom_path(data = bottomRightCornerCircle,mapping = aes(x,y), colour = 'white') +
    geom_path(data = topLeftCornerCircle ,mapping = aes(x,y), colour = 'white') +
    geom_path(data = topRightCornerCircle,mapping = aes(x,y), colour = 'white') +
    geom_path(data = leftEighteenYardBoxArcs,mapping = aes(x,y), colour = 'white') +
    geom_path(data = rightEighteenYardBoxArcs,mapping = aes(x,y), colour = 'white') +
    geom_rect(data = eighteenYardBoxes, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), colour = "white", fill = 'dark green') +
    geom_rect(data = sixYardBoxes, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), colour = "white", fill = 'dark green') +
    geom_polygon(data = leftPenaltySpot, mapping =  aes(x,y), colour ='white', fill = 'white')+
    geom_polygon(data = rightPenaltySpot, mapping =  aes(x,y), colour ='white', fill = 'white') +
    xlab("") +
    ylab("") +
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank())
  }

#Add formations to pitch drawing
#Player Positions is a data.frame with name, x, y, Team
formationsFunction <- function(playerPosition, pitchLength, pitchWidth){
  
  #Validate playerPosition as a dataframe
  if(!is.data.frame(playerPosition)){
    stop("playerPosition needs to be a dataframe containing x, y, name, team",
         call. = FALSE)}
  
  pitchFigure <- drawPitch(pitchLength = pitchLength, pitchWidth = pitchWidth)
  pitchFigure <- pitchFigure + geom_point(data = playerPosition, mapping = aes(x = x, y = y, colour = team), size = 3)
  pitchFigure <- pitchFigure + geom_text(data = playerPosition, aes(x = x, y = y, label = name), nudge_y = -2)
  print(pitchFigure)
  }

#Add passes to pitch drawing

#Add heatmap/comtours to pitch drawing
 
###########
#TEST DATA#
###########
library(tidyverse)
drawPitch(pitchLength = 100,pitchWidth = 90)
playerPositionData <- read_csv("playerPositions.csv")
formationsFunction(playerPosition = playerPositionData, pitchLength = 100, pitchWidth = 90)
