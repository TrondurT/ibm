#     An Individual-Based Model of Copepods
#     By FjordProcess IBM Workshop October 2023

#############################################################################
### Prepping the workspace ----
rm(list=ls()) # clear workspace

#############################################################################
### Load libraries - for functions not in base packages ----
library(ggplot2)

#############################################################################
### Set up the initial population ----
initPop <-read.csv("InitialAbund.csv") # load initial population information

initPop$Stage<- factor(initPop$Stage,
                       levels=initPop$Stage,
                       labels=initPop$Stage) # code stage as a factor

# a look at the stage distribution
ggplot()+
  geom_bar(data = initPop, 
           mapping = aes(x = Stage, y = N),
           stat = "identity")

# Making our initial population
totPop <- sum(initPop$N) # find the total population size
forID <- paste("ID", c(1:totPop), sep = "_") # making a vector of IDs
forStage <- rep(initPop$Stage, times = initPop$N) # making a vector of stages
forStage <- factor(forStage,
                   levels=initPop$Stage,
                   labels=initPop$Stage) # code stage as a factor with levels ordered as we like
ZMat <- data.frame(ID = forID,
                   Stage = forStage) # make my initial population matrix

#############################################################################
### Model parameters ----
startTime <- 45 # starting day of year for model simulation, 14 Feb to align with sampling
endTime <- 181 # ending day of year for model simulation, 30 June
delta.t <- 1 # time-step in days

#############################################################################
### Time-stepping loop ----
ZMatOut <- data.frame() # make an empty data frame to store our progress
for (tNow in seq(from = startTime, to = endTime, by = delta.t)){
  
  ## Track our progress through the loop ----
  print(tNow)
  
  
  
  
  
  # Storing our progress
  ZMatOut <- rbind(ZMatOut, 
                   data.frame(Time = tNow, ZMat)) # add the population matrix and current time to the end of the output file
  
  
}

### Exploring our model results ----
#### Plotting an individual's trajectory over time
subDat <- subset(ZMatOut, ID == )  # subset out an individual

ggplot()+
  geom_line(data = subDat,
            mapping = aes(x = Time, y = Stage)) # plot an individual's stage over time

#### Plotting stage-specific abundances over time

