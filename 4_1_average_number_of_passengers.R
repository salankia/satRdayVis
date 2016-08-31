library(plyr)
library(dplyr)

flights <- read.csv("data/flights_cleaned.csv")
flights <- flights %>%
  arrange(date) %>%
  mutate(month = format(as.POSIXct(date), "%m")) %>%
  mutate(period = (!month %in% c("10", "11", "12", "01", "02", "03"))) %>%
  mutate(period = paste0("Period ", period + 1))

## we would like to classify based on year 2011 (it contains both periods)
flights_2011 <- filter(flights, date.year == 2011 & nbr.of.passengers > 0)
flights_2011$average_nbr_of_p_p_flight <- flights_2011$nbr.of.passengers / flights_2011$nbr.of.flights

nbr_of_p <- ddply(flights_2011, .variables = c("country", "period"),
                  summarize,
                  ratioOfPassangers = (sum(nbr.of.passengers) / 
                    sum(nbr.of.flights))
                    )

nbr_of_p <- nbr_of_p[nbr_of_p$country != "Japan", ]
nbr_of_p <- ddply(nbr_of_p, .variables = "country",
                  summarize, ratio_of_periods =
                    max(ratioOfPassangers[period == "Period 2"], 0) / 
                    max(ratioOfPassangers[period == "Period 1"] , 0))
nbr_of_p <- filter(nbr_of_p, ratio_of_periods > 0 & ratio_of_periods != Inf)
hist(nbr_of_p$ratio_of_periods, breaks = 30)
print(sort(nbr_of_p$ratio_of_periods))
nbr_of_p$category_of_ratio <- NA

nbr_of_p$category_of_ratio[nbr_of_p$ratio_of_periods < 1.0] <- "Bigger_in_summer"
nbr_of_p$category_of_ratio[nbr_of_p$ratio_of_periods >= 1.0 &
                             nbr_of_p$ratio_of_periods < 1.12] <- "Medium"
nbr_of_p$category_of_ratio[nbr_of_p$ratio_of_periods >= 1.12] <- "Bigger_in_winter"

flights <- merge(nbr_of_p, flights, all.x = T)
flights <- flights[!is.na(flights$category_of_ratio), ]
flights$avg_nbr_of_p_per_flight <- flights$nbr.of.passengers / flights$nbr.of.flights
avg_p_per_flight <- flights
save(avg_p_per_flight, file = "data/avg_p_per_flight.RData")
