### Mortality Models PDP - Homework 1

# NOTES:

  # modelling total mortality by single-age group in the USA in the years 
  # 2010 and 2022, using HP model

# Task

# Create a user in HMD
# Up to you: find a way to download data directly from RStudio using MortalityLaws (or other package [Tim Riffe])
# USA, 2022-[2010, T, HP]
# Download data for your country by single-age for the years that you are interested and sex. 
# In some way take out 30 mortality rates for different ages. 
# Select a mortality law and fit model
# Compare mortality of the ages that you estimated with those observed; compare the two years that you have; interpretation of the model. 

# Clear the console and load in relevant packagaes
rm(list=ls(all=TRUE))
library(ggplot2)
library(tidyverse)

################## SET UP DATA FRAME ##################

# Import .csv file into R
library(readxl)

# Confirm you are in the correct directory
setwd("/Users/thomaspetroff/Desktop/Term 2/Population Dynamics & Projections/Week 1/Lecture 3")

# read the data
data2010 <- read_xlsx("USA Life table 2010 & 2022.xlsx",sheet="USA life table 2010")
data2022 <- read_xlsx("USA Life table 2010 & 2022.xlsx",sheet="USA life table 2022")
data2010_removed <- read_xlsx("USA Life table 2010 & 2022.xlsx",sheet="USA life table 2010 (removed)")
data2022_removed <- read_xlsx("USA Life table 2010 & 2022.xlsx",sheet="USA life table 2022 (removed)")

# Model mortality using the Heligman-Pollard method
install.packages('MortalityLaws')
library(MortalityLaws)

# List of available laws
availableLaws(law = NULL)

# Example data (replace with your actual age and mortality rate data)
ages <- 1:81  # Age range
mx_values <- data2010_removed$mx

# Fit the Heligman-Pollard model
HP_fitted <- MortalityLaw(x = data2010_removed$Age, mx = mx_values, law = "HP", opt.method = "poissonL")


# Now you can predict mortality rates for ages 1 to 100
DF <- data.frame(Age = 1:81,
                 Heligman.Pollard.mx = predict(HP_fitted, x = 1:81)) %>% 
  mutate(log.HP.mx = log(Heligman.Pollard.mx))



# Create a graph of the observed rates (mx) and the fitted rares (log.HP.mx) 
Heligman_Pollard <- ggplot(DF, aes(x = Age, y = log.HP.mx)) +
  geom_point(color='blue', lwd=0.5) +
  theme(legend.position = c(0.95, 0.95),
        legend.justification = c("right", "top")) + 
  geom_point(data = DF, aes(x=Age, y=Heligman.Pollard.mx), color="yellow", lwd=0.5)+
  labs(title = "Plot of observed and fitted log mortality rates",
       subtitle = 'Heligman Pollard (blue); Observed (yellow)',
       y = "Log mortality rate",
       x = 'Age (years)')

Heligman_Pollard



