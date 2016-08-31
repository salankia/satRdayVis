source("helper.R")

load("data/avg_p_per_flight.RData")
avg_p_per_flight$date <- as.Date(avg_p_per_flight$date)
## calculatation of the average
avg_of_passangers_per_flight <- ddply(avg_p_per_flight, .variables = "date",
                           summarise, avg_of_avg = mean(avg_nbr_of_p_per_flight))

avg_p_per_flight_subset <- unique(avg_p_per_flight[, c("country", "category_of_ratio")])
colnames(avg_p_per_flight_subset)[1] <- "region"
world<-map_data('world')

new_world <- FiltersOutCountries(world)

world <- left_join(new_world, avg_p_per_flight_subset)
save(world, avg_of_passangers_per_flight, avg_p_per_flight,  file = "4_avg_p_per_flight.RData")

## outliereket leellenorizni: peldaul vulkan kitores, vagy sportesemenyek?
