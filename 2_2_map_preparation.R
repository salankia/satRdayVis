source("helper.R")

load("data/input_first_tab.RData")
flights_summary$date <- as.Date(flights_summary$date)
## calculatation of the average
avg_of_passangers <- ddply(flights_summary, .variables = "date",
                           summarise, avg_of_p = mean(sumOfMovedPassengers))

flights_summary_subset <- unique(flights_summary[, c("country", "category_of_ratio")])
colnames(flights_summary_subset)[1] <- "region"
world<-map_data('world')

new_world <- FiltersOutCountries(world)

world <- left_join(new_world, flights_summary_subset)
save(world, avg_of_passangers, flights_summary,  file = "2_summer_winter.RData")

## outliereket leellenorizni: peldaul vulkan kitores, vagy sportesemenyek?
