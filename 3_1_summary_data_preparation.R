library(plyr)
library(dplyr)
library(ggplot2)

flights <- read.csv("data/flights_cleaned.csv")
set.seed(25)

year2011 <- filter(flights, date.year == 2011 & nbr.of.passengers > 0)
total <- ddply(year2011, .variables = "country", summarize, total_number_of_passangers_per_month = sum(nbr.of.passengers))

total$total_number_of_passangers_per_month <- log10(total$total_number_of_passangers_per_month)
total$category <- as.character(floor(total$total_number_of_passangers_per_month))

save(total, file = "number_of_total_passengers_in_2011.RData")