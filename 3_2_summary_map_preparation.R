load("number_of_total_passengers_in_2011.RData")
colnames(total)[1] <- "region"

world_summary<-map_data('world')

world_summary <- FiltersOutCountries(world = world_summary)

world_summary <- left_join(world_summary, total)
save(world_summary, total, file = "3_overall.RData")

