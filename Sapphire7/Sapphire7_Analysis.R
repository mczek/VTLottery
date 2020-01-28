library(tidyverse)
library(data.table)
library(ggthemes)

simData <- fread("Sapphire7Test.csv") %>%
  mutate(percent = floor(100*ticketNum/nTickets)) %>%
  filter(!is.na(ExpVal))

nTickets <- 420000

# How many iterations enter a playable state?
simData %>%
  filter(!is.na(ExpVal)) %>%
  group_by(id) %>%
  summarize(maxExpVal = max(ExpVal))

simData %>%
  filter(id == 2) %>%
  ggplot(aes(x = ticketNum, y = ExpVal)) + 
  geom_point()

# create known information at each step
generateLBs <- function(simData, confidence = .9){
  lbValDF <- simData %>%
    group_by(percent) %>%
    slice(1) %>%
    summarize(lower_bound = quantile(ExpVal, confidence, na.rm = TRUE))
  
  return(lbValDF)
}


lbValDF <- generateLBs(simData)

# attach percentage

## NOTE: 
## there's an issue where there's no percent = 0 because there's no ticket = 0
## similarly, the last ExpVal is always NA
knowledgeData <- simData %>%
  left_join(lbValDF)


# visualizes the tickets we would buy
knowledgeData %>%
  mutate(indicator = lower_bound > 5) %>%
  ggplot(aes(x = ticketNum, y = ExpVal)) +
  geom_point(aes(color = indicator)) +
  scale_color_colorblind() +
  xlim(420000*.95, 420000)
  