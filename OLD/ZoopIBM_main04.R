#     An Individual-Based Model of Copepods
#     By FjordProcess IBM Workshop October 2023

#############################################################################
### Prepping the workspace ----
rm(list=ls()) # clear workspace

#############################################################################
### Load libraries - for functions not in base packages ----
library(ggplot2) # for plotting functions
library(dplyr) # for manipulating data frames (e.g. group_by())

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
                   Stage = forStage,
                   Age = 1) # make my initial population matrix

#############################################################################
### Model parameters ----
startTime <- 45 # starting day of year for model simulation, 14 Feb to align with sampling
endTime <- 181 # ending day of year for model simulation, 30 June
delta.t <- 1 # time-step in days
# Stage durations
SDInfo<-data.frame(Stage = initPop$Stage,
                   SD = c(1.8,  2.9,  4.6,  8.7,  10.9,  13,  15.5,  18.4,  21.8,  26.1,  32.4, 49.9, 400)) # for time-invarying


#############################################################################
### Time-stepping loop ----
ZMatOut <- data.frame() # make an empty data frame to store our progress
for (tNow in seq(from = startTime, to = endTime, by = delta.t)){
  
  
  print(tNow) # Track our progress through the loop 
  
  # Development ----
  ## First, everyone ages:
  ZMat$Age <- ZMat$Age + delta.t # advance the age by delta.t
  
  ## Then see who moults to the next stage:
  ### Find the stage duration now
  ZMat$SDind <- SDInfo$SD[ZMat$Stage] # assign each individual their stage duration using levels of factor as a number
  
  ### Advance the stage of those meeting their stage duration
  inds <- which(ZMat$Age >= ZMat$SDind) # find the rows meeting the condition of age equal or greater than the individual's SD
  ZMat$Stage[inds] <- SDInfo$Stage[as.numeric(ZMat$Stage[inds])+1] # advance the stage of those individuals by using factor levels as a numeric
  
  
  # Storing our progress ----
  ZMatOut <- rbind(ZMatOut, 
                   data.frame(Time = tNow, ZMat)) # add the population matrix and current time to the end of the output file
  
  
} # end of time-stepping loop

### Exploring our model results ----
#### Plotting an individual's trajectory over time
subDat <- subset(ZMatOut, ID == "ID_63")  # subset out an individual

ggplot()+
  geom_line(data = subDat,
            mapping = aes(x = Time, y = Stage)) # plot an individual's stage over time

#### Plotting stage-specific abundances over time
summaryDat <- group_by(ZMatOut, Time, Stage) # identifies the grouping columns
summaryDat <- summarise(summaryDat, 
                        abund = n()) # to get counts by group

ggplot()+
  geom_line(data = summaryDat, 
            mapping = aes(x = Time, y = abund, col = Stage)) # plot abundance by stage over time

