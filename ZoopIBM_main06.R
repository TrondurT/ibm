#     An Individual-Based Model of Copepods
#     By FjordProcess IBM Workshop October 2023

#############################################################################
### Prepping the workspace ----
rm(list=ls()) # clear workspace
options(warn = 0) # If warn is two, all warnings are turned into errors. Default is warn = 0

#############################################################################
### Load libraries - for functions not in base packages ----
library(ggplot2) # for plotting functions
library(dplyr) # for manipulating data frames (e.g. group_by())
library(drcarlate) # for the norminv() function to choose rates from normal distributions

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
# make my initial population matrix
ZMat <- data.frame(ID = forID, # copepod ID
                   Fitness = runif(totPop, min = 0, max = 1), # an individual's fitness, used to choose process rates for the individual
                   Stage = forStage, # stage
                   Age = NA, # total age of copepod since it was "spawned"
                   TDF = runif(n = totPop, min = 0, max = 1)) # Thermal development fraction for tracking progress within a stage

#############################################################################
### Model parameters ----
startTime <- 45 # starting day of year for model simulation, 14 Feb to align with sampling
endTime <- 181 # ending day of year for model simulation, 30 June
delta.t <- 1 # time-step in days
# Stage durations

Stages <- factor(c("Egg", "N1",  "N2",  "N3",  "N4",  "N5",  "N6",  "C1",  "C2",  "C3",  "C4", "C5",  "C6"), 
                 levels=c("Egg", "N1",  "N2",  "N3",  "N4",  "N5",  "N6",  "C1",  "C2",  "C3",  "C4", "C5",  "C6"), 
                 labels=c("Egg", "N1",  "N2",  "N3",  "N4",  "N5",  "N6",  "C1",  "C2",  "C3",  "C4", "C5",  "C6"))


SDInfo<-data.frame(Stage = Stages,
                   a = c(595, 387, 583, 1387, 759, 715, 841, 966, 1137, 1429, 2166, 5916, NA), # Gentleman et al. 2008 ICES Journal of Marine Science
                   c = (-2.05),
                   d = 17,
                   foodEffect = c(0,0,0,1,1,1,1,1,1,1,1,1,1))

SDInfo$SDMeanConstant <- round((SDInfo$a*(8+9.11)^SDInfo$c)*(SDInfo$d/(58*50)*SDInfo$foodEffect+1),1)
SDInfo$SDVarConstant <- (0.18*(exp(4.4/8)))*((0.28*(exp((44/58)))))/0.32

#############################################################################
### Environmental forcing ----
ExForce<-read.csv("ExForcingData.csv") # load in environmental data
YearNow<-2000 # year to subset
ExForce<-subset(ExForce, Year == YearNow)  # subset out one year

# take a look at the forcing
ggplot()+
  geom_line(data = ExForce,
            mapping = aes(x=DoY, y=Temp))

ggplot()+
  geom_line(data = ExForce,
            mapping = aes(x=DoY, y=Chl))

#############################################################################
### Time-stepping loop ----
ZMatOut <- data.frame() # make an empty data frame to store our progress
for (tNow in seq(from = startTime, to = endTime, by = delta.t)){
  
  
  print(tNow) # Track our progress through the loop 
  
  # Development ----
  ## First, everyone ages:
  ZMat$Age <- ZMat$Age + delta.t # advance the age by delta.t
  
  ## And everyone's TDF is incremented:
  ### Find the stage duration now
  #ZMat$SDind <- SDInfo$SD[ZMat$Stage] # assign each individual their stage duration using levels of factor as a number
  
  ### Find the stage duration now
  for (st in unique(ZMat$Stage)){ # sort(unique()), can be used to keep the level order
    indsNow <- which(ZMat$Stage == st) # find the rows in ZMat with the current stage (st)
    SDinfoNow <- subset(SDInfo, Stage == st) # find the corresponding stage duration info in SDinfo
    
    ZMat$SDind[indsNow] <- norminv(p = ZMat$Fitness[indsNow], # number of individuals in the current stage
                                   mu = SDinfoNow$SDMeanConstant, # mean stage duration for current conditions and stage
                                   sigma = SDinfoNow$SDVarConstant)
    
    # ZMat$SDind[indsNow] <- rnorm(n = length(indsNow), # number of individuals in the current stage
    #                              mean = SDinfoNow$SDMeanConstant, # mean stage duration for current conditions and stage
    #                              sd = SDinfoNow$SDVarConstant) # standard deviation of stage duration for current conditions and stage # add the stage duration info to ZMat
  }
  
  ### Increment everyone's progress through stage (TDF)
  ZMat$TDF <- ZMat$TDF + (delta.t/ZMat$SDind)
  
  ## Then see who moults to the next stage:
  
  ### Advance the stage of those meeting their stage duration
  #### Advancing stage based on total age
  #inds <- which(ZMat$Age >= ZMat$SDind) # find the rows meeting the condition of age equal or greater than the individual's SD
  #ZMat$Stage[inds] <- SDInfo$Stage[as.numeric(ZMat$Stage[inds])+1] # advance the stage of those individuals by using factor levels as a numeric
  
  #### Advancing stage based on TDF
  inds <- which(ZMat$TDF >= 1) # find out who is ready to advance stage (row numbers)
  ZMat$Stage[inds] <- SDInfo$Stage[as.numeric(ZMat$Stage[inds])+1] # advance the stage of those individuals by using factor levels as a numeric
  ZMat$TDF[inds] <- ZMat$TDF[inds] - 1 # reset the TDF of those that moulted
  
  
  # Storing our progress ----
  ZMatOut <- rbind(ZMatOut, 
                   data.frame(Time = tNow, ZMat)) # add the population matrix and current time to the end of the output file
  
  # Remove some temporary objects to avoid reuse errors
  rm(inds, indsNow)
  
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
            mapping = aes(x = Time, y = abund, col = Stage)) #+ # plot abundance by stage over time
  #xlim(40,80)

# stage-specific abundances as a % of total:
ggplot()+
  geom_bar(data = ZMatOut, 
           mapping = aes(x = Time, fill = Stage)) # plot abundance by stage over time

