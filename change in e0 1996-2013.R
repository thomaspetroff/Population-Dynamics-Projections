library(ggplot2)
library(tidyr)
library(DemoDecomp) # tim rife R package
# install.packages("DemoDecomp")

setwd("/Users/thomaspetroff/Desktop/Term 2/Population Dynamics & Projections/Week 3/Lecture 8/Exercise 1")

#load the data
load('Decomp_Data_L4.RData')

# load some functions
source("Functions_4.R")

# vectors from Functions_4.R
age_names
cause_names

# first, we need vectors of mortality rates
mx1 <- age.specific.mx[age.specific.mx$year == 1996,]$mx

mx2 <- age.specific.mx[age.specific.mx$year == 2013,]$mx

# age specific contributions to the change in LE
Results <- horiuchi(func = e0.frommx,
                    pars1 = mx1,
                    pars2 = mx2, N = 15)

Results

original <- e0.frommx(mx2) - e0.frommx(mx1)
original # life expectancy went up by 1.65

# compare this output with original
sum(Results)

error_1 <- original - sum(Results)
error_1

# graph and interpret results
ggplot()+
  ggtitle(bquote(~"Change in e0 1996-2013"))+
  geom_bar(aes(x=age_names, y = Results), stat = "identity", position = "stack")

################## post-break ########################

# create a dataframe
Results_DF <- data.frame(cbind(age_names,Results))

Results_DF$Results <- as.numeric(Results_DF$Results)

Results_DF$age_cont <- c(0,1,seq(5,85,5))

# look at the change in life span variation below age 30
sum(Results_DF[Results_DF$age_cont < 30,]$Results)

# look at the change in life span variation above age 50
sum(Results_DF[Results_DF$age_cont > 50,]$Results)

################ Post-break ######################

e0_1 <- as.matrix(cause.age.specific.mx[cause.age.specific.mx$year==1996,1:10])

vector_1 <- c(e0_1)

e0_2 <- as.matrix(cause.age.specific.mx[cause.age.specific.mx$year==2013,1:10])

vector_2 <- c(e0_2)


e0from_vector <- function(vector,sex=1){
  dim(vector) <- c(19,10) # 19 rows, 10 columns
  mx <- rowSums(vector) 
  e0.frommx(mx,sex) ## this is only difference, use the e0.frommx function
}

# decomposition by age and cause
Results.e0 <- horiuchi(func = e0from_vector, pars1 = vector_1, 
                       pars2 = vector_2, N = 100)
Results.e0

# go back to a matrix
dim(Results.e0) <- dim(e0_1)

Results.e0

# check accuracy
original.e0 <- e0from_vector(vector_2) - e0from_vector(vector_1)
original.e0

# difference from decomposition
with.decomp.e0 <- sum(Results.e0)
with.decomp.e0

#error
with.decomp.e0 - original.e0 # very small difference

# do some data handling

# convert to dataframe from matrix
Results.e0 <- data.frame(Results.e0)

# add the names of the CODs
colnames(Results.e0) <- cause_names

# add the age groups
Results.e0$Age <- c(0,1,seq(5,85,5))

# add the age labels
rownames(Results.e0) <- age_names

# change dataframe from wide format to long format, really handy command
Results.e0 <- gather(data = Results.e0,key = Cause,value = Contribution,-Age)

###### AT-HOME TASK ###########
# change dataframe from long format to wide format, using the spread function
Results.e0 <- spread(data = Results.e0, key = Cause, value = Contribution)


# challenge: how do you go from long format to wide format, essentially the opposite
# google antigather

# now graph results
ggplot(data=Results.e0, aes(x=as.factor(Age), y=Contribution, fill=Cause))+
  ggtitle(bquote(~'Change in e0 1996-2013' ))+
  geom_bar(stat = "identity", position = "stack")



#### AT-HOME TASK #######
# summarize by COD, create bar plot
ggplot(Results.e0, aes(x = reorder(Cause, Contribution), y = Contribution, fill = Cause)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "COD 1996-2013", x = "Cause", y = "Contribution") +
  scale_fill_manual(values = c("red", "blue", "green", "navy", "purple", "orange", 
                               "pink", "cyan", "yellow", "brown")) +
  theme_minimal() +
  theme(legend.position = "none")

# Work with a bar plot that considers the absolute value
# create a vector that takes the absolute value of contribution values
contribution_absolute <- Results.e0$Contribution
contribution_absolute <- abs(contribution_absolute)

# append this to the Results.e0 data frame
Results.e0$contribution_absolute <- contribution_absolute

# summarize by COD, create bar plot (absolute value)
ggplot(Results.e0, aes(x = reorder(Cause, contribution_absolute), y = contribution_absolute, fill = Cause)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "COD with most influence on e0 1996-2013", x = "Cause", y = "Contribution") +
  scale_fill_manual(values = c("red", "blue", "green", "navy", "purple", "orange", 
                               "pink", "cyan", "yellow", "brown")) +
  theme_minimal() +
  theme(legend.position = "none")

# put into quarto document
