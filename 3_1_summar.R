library(plyr)
library(dplyr)
library(ggplot2)

flights <- read.csv("data/flights_cleaned.csv")
set.seed(25)

year2011 <- filter(flights, date.year == 2011 & nbr.of.passengers > 0)
total <- ddply(year2011, .variables = "country", summarize, total_number_of_passangers_per_month = sum(nbr.of.passengers))

total$total_number_of_passangers_per_month <- log10(total$total_number_of_passangers_per_month)
total$category <- as.character(floor(total$total_number_of_passangers_per_month))

ggplot(total) + geom_point(aes(x = total_number_of_passangers_per_month,
                               y = total_number_of_passangers_per_month,
                               col = total_number_of_passangers_per_month)) +
  geom_text(aes(x = total_number_of_passangers_per_month,
                 y = total_number_of_passangers_per_month + 0.2, label = country,
                col = total_number_of_passangers_per_month)) +
  theme(legend.position = "none") +
  scale_color_gradient(low = "grey", high = "red") +
  geom_segment(aes(x = total_number_of_passangers_per_month, xend = total_number_of_passangers_per_month,
                   y = 1, yend = total_number_of_passangers_per_month, col = total_number_of_passangers_per_month))

total <- arrange(total, total_number_of_passangers_per_month)
total %>% ggvis(x = ~total_number_of_passangers_per_month,
                y = ~total_number_of_passangers_per_month) %>%
  layer_bars(stroke := "grey") %>%
  layer_text(x = ~total_number_of_passangers_per_month,
             y = ~total_number_of_passangers_per_month + rnorm(n = length(total$country),
                                                               mean = 0.2, sd = 0.2),
             text := ~country,
             align := "center",
             baseline := "bottom", 
             stroke = ~category) %>%
layer_points(fill = ~category)  %>%
  handle_click(on_click = function(data, location, session){print(data)
    print(location)}) 