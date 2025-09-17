# Loading packages.

# If any of these are not installed on your workspace, use install.packages()

install.packages("tidyverse")
install.packages("HMDHFDplus")

library(tidyverse) # for data wrangling and nicer plotting
library(HMDHFDplus) # for downloading data from the Human Mortality Database
library(scales)   # for log transformations of axes
options(scipen=4) # I don't like scientific notation
library(ggplot2)


LT <- readHMDweb(CNTRY = "USA", item = "fltper_1x1", username = "tpetroff1212@gmail.com", password = "hzx-ufr!cad3tvg!URC")

# plot deaths
ggplot(data = LT) +
  geom_line(aes(x=Age,y=dx,colour=Year,group=Year)) + 
  xlab("Age") +
  ylab("Life table deaths") +
  ggtitle("USA female life table deaths (period)") +
  theme(axis.title.x = element_text(size=14)) + 
  theme(axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(axis.text.y  = element_text(size=12)) 

# plot survival curve
ggplot(data = LT) +
  geom_line(aes(x=Age,y=lx,colour=Year,group=Year)) + 
  xlab("Age") +
  ylab("Life table survivors") +
  ggtitle("USA female survival curves (period)") +
  theme(axis.title.x = element_text(size=14)) + 
  theme(axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(axis.text.y  = element_text(size=12)) 

# death density
ggplot(data = filter(LT, Year==1965 | Year==2015)) +
  geom_line(aes(x=Age,y=dx,colour=as.factor(Year),group=as.factor(Year)), linewidth=2) + 
  xlab("Age") +
  ylab("Life table deaths") +
  ggtitle("USA female life table deaths (period)") +
  theme(axis.title.x = element_text(size=14)) + 
  theme(axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(axis.text.y  = element_text(size=12)) +
  scale_colour_discrete(name  ="Year",
                        breaks=c(1965,2015),
                        labels=c(1965,2015))

# survival curve
ggplot(data = filter(LT, Year==1965 | Year==2015)) +
  geom_line(aes(x=Age,y=lx,colour=as.factor(Year),group=as.factor(Year)), linewidth=2) + 
  xlab("Age") +
  ylab("Life table survivors") +
  ggtitle("USA female survival curves (period)") +
  theme(axis.title.x = element_text(size=14)) + 
  theme(axis.text.x  = element_text(size=12)) +
  theme(axis.title.y = element_text(size=14)) +
  theme(axis.text.y  = element_text(size=12)) +
  scale_colour_discrete(name  ="Year",
                        breaks=c(1965,2015),
                        labels=c(1965,2015))

# Lifespan variation
Age <- 0:110
dx <- filter(LT, Year == 2000)$dx
ex <- filter(LT, Year == 2000)$ex[1]
ax <- filter(LT, Year == 2000)$ax
lx <- filter(LT, Year == 2000)$lx

alphax <- ax + Age
eta <- ex

# variance in age at death
inner <- dx * (alphax - eta)^2
inner

V <- sum(inner) / lx[1]
V

# standard deviation 
S <- sqrt(V)
S

# can make a function to replicate what we did above and make life simpler
sd_function <- function(age, dx, ex, ax, l0) {
  alphax <- age + ax
  eta <- ex[1] +age[1]
  inner <- dx * (alphax - eta)^2
  V <- sum(inner) / l0
  S <- sqrt(V)
  return(S)
}

# making a time series of SDs
Age <- 0:110
Years <- 1960:max(LT$Year)

# create an object S that is an empty vector of the same length Years
S <- c(NA, length(Years))

# populate S with data
for (i in 1:length(Years)){
  S[i] <- sd_function(age=Age,
                      dx=filter(LT, Year==Years[i])$dx,
                      ex=filter(LT, Year==Years[i])$ex,
                      ax=filter(LT, Year==Years[i])$ax,
                      l0=100000)
}


head(S)
S

e0_year <- filter(LT, Age == 0, Year >=1960)$ex

# make a dataframe for plotting
results <- tibble(Year=Years,
                  e0=e0_year,
                  S=S)
results

# Plot S
ggplot(results, aes(x=Year, y = S)) +
  geom_line(color = "blue", linewidth = 1.5)+
  ggtitle("USA females, standard deviation in age at death")

# Plot e0
ggplot(results, aes(x=Year, y = e0)) +
  geom_line(color = "red", linewidth = 1.5)+
  ggtitle("USA females, life expectancy 1960-2022")

####### MDLT ###########



###### Task: Simplify the S function #########


#Apply the function to each year using tidyverse functions:
#getting all the years first by filtering them, and make sure that they're grouped by year
#key is the 'summarize' function where the vectors in the funciton get stored as a list (e0 is used)
#S here is the sd_function applied to eac year !ONLY AFTER FILTERING THEM FOR THE CORRESPONDING YEAR


results1 <- LT %>%
  filter(Year >= 1960 & Year <= 2022) %>%  
  group_by(Year) %>%  
  summarize(
    ex = first(ex),  
    ax = list(ax),    
    dx = list(dx),   
    lx = list(lx),    
    S = sd_function(
      age = Age,
      dx = dx[[1]],  # Access the first (and only) element in the list
      ex = ex,
      ax = ax[[1]],  # Access the first (and only) element in the list
      l0 = 100000    # Initial population (l0)
    )
  ) %>%
  ungroup() %>%  # Ungroup after summarizing
  select(Year, e0 = ex, S)  # Select the columns you need for the result


# Plot results
ggplot(results1, aes(x = Year, y = S)) +
  geom_line(color= "green", linewidth=1.5) +
  labs(title = "Standard Deviation of Lifespan for Females in the USA between 1960 and 2022",
       x = "Year",
       y = "Standard Deviation (S)")







