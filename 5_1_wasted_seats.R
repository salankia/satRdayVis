library(plyr)
library(dplyr)
library(ggplot2)

flights <- read.csv("data/flights_cleaned.csv")
flights <- flights %>%
  arrange(date) %>%
  mutate(month = format(as.POSIXct(date), "%m")) %>%
  mutate(period = (!month %in% c("10", "11", "12", "01", "02", "03"))) %>%
  mutate(period = paste0("Period ", period + 1))

flights <- filter(flights, nbr.of.passengers > 0)

avg <- ddply(flights, .variables = "country", summarize,
             avg_p_p = mean(nbr.of.passengers / nbr.of.flights),
             avg_seat = mean(seat.capacity / nbr.of.flights),
             number_of_p = sum(nbr.of.passengers[date.year == 2011]))
avg$avg_utilization <- avg$avg_p_p / avg$avg_seat

ggplot(avg) + geom_point(aes(x = avg_seat, y = avg_p_p))

avg$coordx <- avg$avg_seat
avg$coordy <- avg$avg_utilization * 100

## awful manual modofications
avg$coordy[avg$country == "Moldova"] <- avg$coordy[avg$country == "Moldova"] + 0.5
avg$coordy[avg$country == "Slovenia"] <- avg$coordy[avg$country == "Slovenia"] - 1
avg$coordy[avg$country == "Finland"] <- avg$coordy[avg$country == "Finland"] - 1
avg$coordy[avg$country == "Egypt"] <- avg$coordy[avg$country == "Egypt"] - 0.5
avg$coordx[avg$country == "Romania"] <- avg$coordx[avg$country == "Romania"] - 5
avg$coordx[avg$country == "Denmark"] <- avg$coordx[avg$country == "Denmark"] - 15
avg$coordx[avg$country == "Belgium"] <- avg$coordx[avg$country == "Belgium"] - 15
avg$coordx[avg$country == "Israel"] <- avg$coordx[avg$country == "Israel"] - 10
avg$coordx[avg$country == "Ireland"] <- avg$coordx[avg$country == "Ireland"] - 15
avg$coordx[avg$country == "Sweden"] <- avg$coordx[avg$country == "Sweden"] - 15
avg$coordy[avg$country == "The Netherlands"] <- avg$coordy[avg$country == "The Netherlands"] - 0.5
avg$coordy[avg$country == "France"] <- avg$coordy[avg$country == "France"] - 1



avg %>%
  ggvis(x = ~ avg_seat, y = ~ avg_utilization * 100 )%>%
  layer_points() %>%
  layer_text(x = ~coordx + 2,
             y = ~coordy ,
             text := ~country,
             align := "left",
             baseline := "bottom") %>%
  add_axis("x", title = "Average seat capacity of a plane") %>%
  add_axis("y", title = "Average utilization of planes in %")
colnames(avg)[1] <- "region"


avg$color <- as.factor((avg$avg_seat - avg$avg_p_p) %/% 25)
save(avg,file = "wasted_seats.RData")
avg %>%
  ggvis(x = ~ avg_seat, y = ~ avg_p_p)%>%
  layer_points(fill = ~color) %>%
  # layer_text(x = ~avg_seat + 2,
  #            y = ~avg_p_p ,
  #            text := ~country,
  #            align := "left",
  #            baseline := "bottom") %>%
  add_axis("x", title = "Average seat capacity of a plane") %>%
  add_axis("y", title = "Average number of passengers on a plane") %>%
  layer_paths(data = data.frame(x = c(40, 40, 260, 260), y = c(15, 40, 260, 235)),
              x = ~x, y = ~y, fill := "#a6cee3", opacity := 0.1) %>%
  layer_paths(data = data.frame(x = c(40, 40, 260, 260), y = c(-10, 15, 235, 210)),
              x = ~x, y = ~y, fill := "#b2df8a", opacity := 0.1) %>%
  layer_paths(data = data.frame(x = c(64, 40, 260, 260), y = c(-10, -10, 210, 185)),
              x = ~x, y = ~y, fill := "#fdbf6f", opacity := 0.1) %>%
  layer_paths(data = data.frame(x = c(89, 64, 260, 260), y = c(-10, -10, 185, 160)),
              x = ~x, y = ~y, fill := "#fb9a99", opacity := 0.1) %>%
  layer_paths(data = data.frame(x = c(118, 89, 260, 260), y = c(-10, -10, 160, 135)),
              x = ~x, y = ~y, fill := "#cab2d6", opacity := 0.1) %>%
  scale_nominal("fill", range = c("#1f78b4", "#33a02c", "#ff7f00", "#e31a1c", "#6a3d9a")) %>%
  scale_numeric("x", domain = c(45, 270)) %>%
  scale_numeric("y", domain = c(0, 250)) %>%
  hide_legend("stroke") %>% hide_legend("fill")



avg %>% ggvis(x = ~avg_seat,
                y = ~rank_of_seatsize) %>%
  layer_bars(stroke := "grey") %>%
  scale_numeric("y", reverse = T)  %>% 
  layer_text(x = ~avg_seat - 0.01,
             y = ~rank_of_seatsize ,
             text := ~region,
             align := "right",
             baseline := "bottom")  %>%
  add_axis("x", title = "Average seat capacity of a plane", grid = F) %>%
  hide_axis("y") %>%
  set_options(width = "auto", height = "auto", resizable=FALSE)

avg %>% ggvis(x = ~avg_seat,
              y = ~rank_of_seatsize) %>%
  layer_bars(stroke := "grey") %>%
  scale_numeric("y", reverse = T)  %>% 
  layer_text(x = ~avg_seat - 0.01,
             y = ~rank_of_seatsize ,
             text := ~region,
             align := "right",
             baseline := "bottom")  %>%
  add_axis("x", title = "Average seat capacity of a plane", grid = F) %>%
  hide_axis("y") %>%
  add_tooltip(toltip_text_summary, on = "hover") %>%
  set_options(width = "auto", height = "auto", resizable=FALSE)

avg %>% ggvis() %>%
  layer_rects(x := 0,
              x2 = ~rank_of_capacity,
              y = ~100 * avg_capacity + 0.001, 
               y2 =  ~ 100 * avg_capacity - 0.001, stroke := "grey") %>%
  layer_text(x = ~rank_of_capacity + 1,
             y = ~100 * avg_capacity  ,
             text := ~region,
             align := "left",
             baseline := "bottom")  %>%
  add_axis("y", title = "Average utilization of a plane", grid = F) %>%
  hide_axis("x") %>%
  add_tooltip(toltip_text_summary, on = "hover") %>%
  set_options(width = "auto", height = "auto", resizable=FALSE)