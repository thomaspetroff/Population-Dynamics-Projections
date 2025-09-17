setwd("/Users/thomaspetroff/Desktop/Term 2/Population Dynamics & Projections/Week 2/Lecture 2/Years of life lost R Practical")
load('Exercise_YLL.RData')

source(file = 'Functions 2.R')

# Operations 

# create an object called YLL, which gives the proportion
# of a specific cause of death by age group
YLL <- LostYears(FLT = LT1, B = COD1)
list(YLL)
colSums(YLL)

# YLL up to age 85
sum(YLL[-nrow(YLL),]) # 11.56 years of life lost

list(YLL[-nrow(YLL),])

# which cause contributes the most to YLL
colSums(YLL[-nrow(YLL),]) # cardiovascular diesease contributes the most YLL

# plot the contribution of cause of death to YLL
Plot_YLL(FLT = LT1, B = COD1)

# plot the contribution of cause of death to YLL via a bar graph
library(ggplot2)
library(tidyverse)

# create an object that has the total YLL for each cause of death
causes_totalYLL <- colSums(YLL[-nrow(YLL),])
list(causes_totalYLL)

# create a dataframe to work with
causes_totalYLL_DF <- data.frame(
  cause = c("HIV/AIDS and tuberculosis", 
            "Diarrhea, lower respiratory, and other common infectious diseases", 
            "Neoplasms", 
            "Cardiovascular diseases", 
            "Diabetes, urogenital, blood, and endocrine diseases", 
            "Transport injuries", 
            "Unintentional injuries", 
            "Self-harm and interpersonal violence", 
            "Rest"),
  YLL = c(0.3858124, 0.7196955, 1.3730496, 3.2904562, 0.9479832, 
            1.0275571, 0.8439215, 0.1541113, 2.8134303)
)

# Create bar plot
ggplot(causes_totalYLL_DF, aes(x = reorder(cause, YLL), y = YLL, fill = cause)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "Total YLL by COD for females in Saudi Arabia 2005 - 2010", x = "Cause", y = "YLL") +
  scale_fill_manual(values = c("red", "blue", "green", "purple", "orange", 
                               "pink", "cyan", "yellow", "brown")) +
  theme_minimal() +
  theme(legend.position = "none")


################ Proportion of YLL by COD ############################
# Next step would be to calculate the proportion of YLL by COD
YLL_proportion <- causes_totalYLL_DF$YLL

causes_totalYLL_DF <- cbind(causes_totalYLL_DF, YLL_proportion)
causes_totalYLL_DF <- causes_totalYLL_DF %>% mutate(YLL_proportion = YLL_proportion / 11.55602)

# Create bar plot
ggplot(causes_totalYLL_DF, aes(x = reorder(cause, YLL_proportion), y = YLL_proportion, fill = cause)) +
  geom_bar(stat = "identity") +
  coord_flip() + 
  labs(title = "Proportion of YLL by COD for females in Saudi Arabia 2005 - 2010", x = "Cause", y = "Proportion of YLL (n = 11.56)") +
  scale_fill_manual(values = c("red", "blue", "green", "purple", "orange", 
                               "pink", "cyan", "yellow", "brown")) +
  theme_minimal() +
  theme(legend.position = "none")


