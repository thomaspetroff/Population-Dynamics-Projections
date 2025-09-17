setwd("/Users/thomaspetroff/Desktop/Term 2/Population Dynamics & Projections/Week 3/Lecture 8/Exercise 2")

source("Functions_5.R")

library(ggplot2)
library(tidyr)
library(DemoDecomp)
library(tidyverse)


load('Decomp_Data_L4 (1).RData')

# vectors from Functions_4.R
age_names
cause_names

# first, we need vectors of mortality rates
mx1 <- age.specific.mx[age.specific.mx$year == 1996,]$mx

mx2 <- age.specific.mx[age.specific.mx$year == 2013,]$mx

# age specific contributions to the change in Lifespan variation
Results <- horiuchi(func = sd.frommx,
                    pars1 = mx1,
                    pars2 = mx2, N = 100)

Results

original <- sd.frommx(mx2) - sd.frommx(mx1)
original # lifespan variation went up by 0.95

# compare this output with original
sum(Results)

error_1 <- original - sum(Results)
error_1

# graph and interpret results

ggplot()+
  ggtitle(bquote(~"Change in SD 1996-2013"))+
  geom_bar(aes(x=age_names, y = Results), stat = "identity", position = "stack")

length(age_names)

# create a dataframe
Results_DF <- data.frame(cbind(age_names,Results))

Results_DF$Results <- as.numeric(Results_DF$Results)

Results_DF$age_cont <- c(0,1,seq(5,85,5))

# look at the change in life span variation below age 30
sum(Results_DF[Results_DF$age_cont < 30,]$Results)

# look at the change in life span variation above age 50
sum(Results_DF[Results_DF$age_cont > 50,]$Results)

################ Post-break ######################

COD1 <- as.matrix(cause.age.specific.mx[cause.age.specific.mx$year==1996,1:10])

vector_1 <- c(COD1)

COD2 <- as.matrix(cause.age.specific.mx[cause.age.specific.mx$year==2013,1:10])

vector_2 <- c(COD2)


sdfrom_vector <- function(vector,sex=1){
  dim(vector) <- c(19,10) # 19 rows, 10 columns
  mx          <- rowSums(vector) 
  sd.frommx(mx,sex)
}

# decomposition by age and cause
Results.sd <- horiuchi(func = sdfrom_vector, pars1 = vector_1, 
                       pars2 = vector_2, N = 100)
Results.sd

# go back to a matrix
dim(Results.sd) <- dim(COD1)

Results.sd

# check accuracy
original.sd <- sdfrom_vector(vector_2) - sdfrom_vector(vector_1)
original.sd

# difference from decomposition
with.decomp.sd <- sum(Results.sd)
with.decomp.sd 

#error
with.decomp.sd - original.sd # very small difference

# do some data handling

# convert to dataframe from matrix
Results.sd <- data.frame(Results.sd)

# add the names of the CODs
colnames(Results.sd) <- cause_names

# add the age groups
Results.sd$Age <- c(0,1,seq(5,85,5))

# add the age labels
rownames(Results.sd) <- age_names

# change dataframe from wide format to long format, really handy command
Results.sd <- gather(data = Results.sd,key = Cause,value = Contribution,-Age)

# challenge: how do you go from long format to wide format, essentially the opposite
# google antigather

# now graph results
ggplot(data=Results.sd, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle(bquote(~'Change in '~ sd[0] ~'1996-2013' ))+
  geom_bar(stat = "identity", position = "stack")







  





