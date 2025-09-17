# Stable Populations
# Date created: March 19th, 2025
# Date last uodated: March 19th, 2025

# setwd
setwd("/Users/thomaspetroff/Desktop/Term 2/Population Dynamics & Projections/Week 4/Lecture 10/Stable Populations")


# Load in the Egypt data
Egypt <- read.csv("Data_Practical5_R.csv", header=TRUE)[1:7, 1:3]

# Calculate the maternity rates adjusted for mortality
Egypt <- mutate(Egypt, 
                Mat.rates.adj = nLx*Maternity.rates) # adjusting for mortality

# Set a Generation length - rule of thumb=27 years
Gen.length <- 27

# calculate the NRR as the sum of the adjusted maternity rates
NRR <- Egypt %>% 
  summarize(sum=sum(Mat.rates.adj))

# Make the NRR value easy to work with by converting to a vector element
NRR <- NRR$sum

# Specify the number of iterations needed to obtain a stable population est
iterations <- 115

# ----- Find the instrinic growth rate
# Create empty matrix for the iterations of the stable pop age structure and vector for the iterations of the intrinsic growth rate
Stable.pop <- data.frame(matrix(NA, nrow(Egypt), iterations))

r <- rep(NA, iterations)

# Initial r and place into vector of iterative estimates of r (increasingly improving converging to 1 - balancing criteria)
r[1] <- log(NRR)/Gen.length

# Create a loop that calculates the stable population age stucture and stores the values of r
for (i in 1:ncol(Stable.pop)) {
  
  # Calculate the stable population age structure 
  Stable.pop[,i] <- exp(-r[i]*
                          (Egypt %>%
                             select(Age)+2.5))*
    Egypt %>%
    select(Mat.rates.adj)
  
  # Improve previous value of r using the difference of the sum of the age stucture from 1 (balancing characteristic)
  r[i+1] = r[i] + (sum(Stable.pop[,i])-1)/Gen.length
  
}

# Intrinsic growth rate
r <- last(r) # extracts last value of the vector r
r



################### Next tasks: USA data ########################
USA <- read.csv("Data_Practical5_R.csv", header=TRUE)[, 6:9]

# change column names
colnames(USA) <- c("Age", "Ca", "Lx", "Mx")

# specify radix
radix <- 100000

# Set a Generation length - rule of thumb=27 years
Gen.length <- 27

# Specify the number of iterations needed to obtain a stable population est
iterations_USA <- 12

# Calculate the maternity rates adjusted for mortality
USA <- mutate(USA,
              Mx = replace_na(Mx,0),
              Lx = Lx/radix,
              Mat.rates.adj = Lx * Mx) # adjusting for mortality

# calculate the NRR as the sum of the adjusted maternity rates
NRR_USA <- USA %>% 
  summarize(sum=sum(Mat.rates.adj))

# Make the NRR value easy to work with by converting to a vector element
NRR_USA <- NRR_USA$sum

# Create a stable population matrix and r vector
max_age <- 50
min_age <- 10
n <- 5
age_gps <- (max_age - min_age)/n

# ----- Find the instrinic growth rate
# Create a dataframe for Lotka's r calculations
# and an empty vector storing the values of the iterations of intrinsic growth rate
SP = data.frame(matrix(0, ncol = iterations_USA, nrow = age_gps))
r_vec <-rep(NA,iterations_USA)

# Initial r and place into vector of iterative estimates of r (increasingly improving converging to 1 - balancing criteria)
# This is calculated as the division of the log net reporduction rate and Generation length 
r_vec[1] <- log(NRR_USA)/Gen.length

# Here we run a loop with Lotka's balancing equation populating each column of the dataframe SP
# from which we can ascertain the stable population age structure
# The resulting growth rate is stored in subsequent elements of the vector r.vec
# within r.vec we can observe when further changes to r do not occur i.e. whether we have convergence
for (It in 1:ncol(SP)) {
  
  # Calculate the stable population age structure
  SP[,It] <- exp(-r_vec[It]*
                   (USA %>%
                      select(Age) %>%
                      filter(Age>5 & Age<50)+2.5))*
    (USA %>%
       filter(Age>5 & Age<50) %>%
       select(Mat.rates.adj))
  
  # Improve previous value of r using the difference of the sum of the age stucture from 1 (balancing characteristic)
  r_vec[It+1] = r_vec[It] + (sum(SP[,It])-1)/Gen.length
  
}

# Select the final iteration of the Intrinsic growth rate 
r <- last(r_vec)

# Calculate the stable pop equivalent person years
USA <- USA %>% 
  mutate(Lx = exp(-r*(Age+2.5)) * Lx)

USA$Lx[18] <- exp(-r*(USA$Age[18]+6.79))*USA$Lx[18]

# Calculate the life expectancy at birth (T0 / l0, where l0 is equal to 1 after having created X5La.r = X5La/radix previously)
e0 <- sum(USA$Lx)

# Calulate a column of the stable population age distribution XCa.s
USA <- USA %>% 
  mutate(Ca.s = Lx/e0)

# We can now calculate a Crude Birth rate using the life expectancy at birth 
CBR <- 1 / e0
CBR

# And crude death rate as the subtraction of the Crude Birth and intrinsic growth rate
CDR = CBR - r
CDR

# Plot the stable population proportion vs observed population proportion
p<-ggplot(data=USA) +
  geom_bar(aes(x=Ca, y=as.factor(Age)), stat="identity", col=1) +
  geom_bar(aes(x=Ca.s, y=as.factor(Age)), stat="identity", col = 2) 
p
