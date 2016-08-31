library(dplyr)
library(plyr)

flights <- read.csv("data/BUD flights 2007-2012 v2.csv")
colnames(flights) <- tolower(colnames(flights))

flights <- flights%>%
  mutate(date = as.Date(date, "%m/%d/%Y"),
         date.half.year = NULL,
         date.year.quarter = NULL,
         date.year.month = NULL)
flights<- flights%>%
  mutate(nbr.of.passengers =
           as.numeric(gsub(",", "", as.character(nbr.of.passengers))),
         cargo.weight =
           as.numeric(gsub(",", "", as.character(cargo.weight))),
         nbr.of.flights =
           as.numeric(gsub(",", "", as.character(nbr.of.flights))),
         seat.capacity =
           as.numeric(gsub(",", "", as.character(seat.capacity))))

## filtering out those countries, which do not have enough data about passenger tranfer
summary <- ddply(flights, .variables = "country", summarize,
      number_of_presented_years = length(unique(date.year)),
      number_of_passengers_overall = sum(nbr.of.passengers)
      )
important.countries <- as.character(summary$country[summary$number_of_presented_years >= 4 &
                                         summary$number_of_passengers_overall > 10000])
flights<- flights[flights$country %in% important.countries, ]
write.csv(flights, file = "data/flights_cleaned.csv", row.names = F)

