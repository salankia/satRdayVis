library(plyr)
library(dplyr)

flights <- read.csv("data/flights_cleaned.csv")

## how many passangers are moved in each period?
## period 1: from Ocotber until March
## period 2: from April to September

flights <- flights %>%
  arrange(date) %>%
  mutate(month = format(as.POSIXct(date), "%m")) %>%
  mutate(period = (!month %in% c("10", "11", "12", "01", "02", "03"))) %>%
  mutate(period = paste0("Period ", period + 1))

## we would like to classify based on year 2011 (it contains both periods)
flights_2011 <- filter(flights, date.year == 2011 & nbr.of.passengers > 0)

nbr_of_p <- ddply(flights_2011, .variables = c("country"),
      summarize, ratioOfPassangers = sum(nbr.of.passengers[period == "Period 2"] /
                                           sum(nbr.of.passengers[period == "Period 1"])))
nbr_of_p <- filter(nbr_of_p, ratioOfPassangers > 0 & ratioOfPassangers != Inf)
hist(nbr_of_p$ratioOfPassangers, breaks = 30)
nbr_of_p$category_of_ratio <- NA
# nbr_of_p$category_of_ratio[nbr_of_p$ratioOfPassangers < 1] <- "Winter_specific"
# nbr_of_p$category_of_ratio[nbr_of_p$ratioOfPassangers >= 1 &
#                              nbr_of_p$ratioOfPassangers < 1.5] <- "Medium"
# nbr_of_p$category_of_ratio[nbr_of_p$ratioOfPassangers >= 1.5] <- "Summer_specific"

nbr_of_p$category_of_ratio[nbr_of_p$ratioOfPassangers < 1.2] <- "Winter_specific"
nbr_of_p$category_of_ratio[nbr_of_p$ratioOfPassangers >= 1.2 &
                              nbr_of_p$ratioOfPassangers < 1.5] <- "Medium"
 nbr_of_p$category_of_ratio[nbr_of_p$ratioOfPassangers >= 1.5] <- "Summer_specific"

flights <- merge(nbr_of_p, flights, all.x = T)
flights <- flights[!is.na(flights$category_of_ratio), ]
flights_summary <- ddply(flights, .variables = c("country", "ratioOfPassangers",
                                                 "category_of_ratio", "date"),
                         summarize, sumOfMovedPassengers = sum(nbr.of.passengers))

save(flights_summary, file = "data/input_first_tab.RData")

