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

# 