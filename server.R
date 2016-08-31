library(shiny)
library(ggplot2)
library(reshape2)
require(maps)
require(maptools)
require(plyr)
library(dplyr)
library(ggvis)

load("2_summer_winter.RData")
load("3_overall.RData")
load("wasted_seats.RData")

chosenCountry <- NA

total <- arrange(total, total_number_of_passangers_per_month)
total$rank <- seq_along(total$region)

## selection binding
update_selection = function(data,location,session){
  print(unique(world$region[world$group == data$group]))
  if(is.null(data)) return(NULL)
  updateSelectInput(session
                    ,"medium"
                    ,selected=unique(world$region[world$group == data$group]))
}

update_selection2 = function(data,location,session){
  print(unique(world$region[world$group == data$group]))
  if(is.null(data)) return(NULL)
 chosenCountry <- data$region
}

toltip_text_summary <- function(x){
  if(is.null(x)) return(NULL)
  row <- total[total$region == x$region, ]
  s <- paste0("", row$region)
  s <- paste0(s, "<br />", "# passengers: ", row$total_number_of_passangers_per_month)
  s <- paste0(s, "<br />", "Rank: ", max(total$rank) - row$rank + 1)
  s
}

toltip_map_ratio <- function(x){
  print(x)
  if(is.null(x)) return(NULL)
  region <- unique(world$region[world$group == x$group])
  row <- flights_summary[flights_summary$country == region, ]
  s <- paste0("", unique(row$country))
  s <- paste0(s, "<br />", "Ratio: ", round(unique(row$ratioOfPassangers), digits = 2))
  s
}


toltip_ts_ratio <- function(x){
  #print(x)
  if(is.null(x)) return(NULL)
  region <- unique(flights_summary$country[as.numeric(as.POSIXct(flights_summary$date)) ==
                                             x$date])
  #s <- paste0("", unique(region))
  s <- paste0("# travellers: ", x$sumOfMovedPassengers)
  s
}


shinyServer(function(input, output) {
  #################### Overall ##########################
  overallVis = reactive({
    total %>% ggvis(x = ~total_number_of_passangers_per_month_log,
                    y = ~rank) %>%
      layer_bars(stroke := "grey") %>%
      layer_points(fill = ~category, size := 22)  %>% 
      layer_text(x = ~total_number_of_passangers_per_month_log - 0.01,
                 y = ~rank ,
                 text := ~region,
                 align := "right",
                 baseline := "bottom",
                 stroke = ~category) %>%
      layer_paths(data = data.frame(x = c(1.5, 1.9, 2.2, 2.9),
                                    y = c(rep(0, 4)),
                                    color = c(2, 2, 3, 3)),
                  x = ~x, y = ~y,  fill := ~color, strokeWidth := 5) %>%
      hide_legend("stroke") %>%
      hide_legend("fill") %>%
      add_axis("x", title = "", grid = F, values = c(2:6), subdivide = 9) %>%
      hide_axis("y") %>%
      scale_numeric("x", nice = T, expand = 0.03) %>%
      add_tooltip(toltip_text_summary, on = "hover") %>%
      set_options(width = "auto", height = "auto", resizable=FALSE)
  })
  overallVis %>% bind_shiny("summary")
  
  #################### Summer and Winter ################
  timseriesWinterData = reactive({
    flights_summary %>% filter(country  == input$winter)
  })
  timseriesMediumData = reactive({
    flights_summary %>% filter(country  == input$medium)
  })
  timseriesSummerData = reactive({
    flights_summary %>% filter(country  == input$summer)
  })
  
  mapWinterData = reactive({
    world %>% filter(region  == input$winter)
  })
  mapMediumData = reactive({
    world %>% filter(region  == input$medium)
  })
  mapSummerData = reactive({
    world %>% filter(region  == input$summer)
  })
  
  ## map operations
  mapVis = reactive({
    world %>% select(long, lat, group, order, region, category_of_ratio) %>%
      group_by(group) %>% 
      ggvis(x = ~long, y = ~lat) %>% 
      layer_paths(fill = ~category_of_ratio)   %>%
      add_tooltip(toltip_map_ratio, on = "hover") %>%
      scale_nominal("fill", range = c("#74add1", "#fee090", "#d73027", "grey")) %>%
      set_options(width = "auto", height = "auto", resizable=FALSE) %>%
      #add_axis("x", title = "", grid = F) %>%
      #add_axis("y", title = "", grid = F) %>%
      hide_axis("x") %>%
      hide_axis("y") %>%
      hide_legend("fill")
  })
 
  mapVis %>%
    bind_shiny("map")

  
  timerseriesVis = reactive({
    avg_of_passangers %>% ggvis(x = ~date, y = ~avg_of_p) %>%
      layer_points(fill := "grey", size := 12) %>% layer_lines(stroke := "grey") %>%
      add_tooltip(toltip_ts_ratio, on = "hover") %>%
      layer_paths(data = timseriesWinterData(),
                   x = ~date, y = ~sumOfMovedPassengers, stroke := "#313695", strokeWidth := 3) %>%
      layer_points(data = timseriesWinterData(),
                  x = ~date, y = ~sumOfMovedPassengers, fill := "#313695", size := 12) %>%
      layer_paths(data = timseriesMediumData(),
                  x = ~date, y = ~sumOfMovedPassengers, stroke := "#fdae61", strokeWidth := 3) %>%
      layer_points(data = timseriesMediumData(),
                   x = ~date, y = ~sumOfMovedPassengers, fill := "#fdae61", size := 12) %>%
      layer_paths(data = timseriesSummerData(),
                  x = ~date, y = ~sumOfMovedPassengers, stroke := "#a50026", strokeWidth := 3) %>%
      layer_points(data = timseriesSummerData(),
                   x = ~date, y = ~sumOfMovedPassengers, fill := "#a50026", size := 12)%>%
      set_options(width = "auto", height = "auto", resizable=FALSE) %>%
      add_axis("y", title = "Number of passangers", title_offset = 50)
  })
  
  timerseriesVis %>%
    bind_shiny("timeseries")
  
  ####################### Wasted
  scatterVis = reactive({
    
    avg %>%
      ggvis(x = ~ avg_seat, y = ~ avg_utilization * 100 )%>%
      layer_points() %>%
      layer_text(x = ~coordx + 2,
                 y = ~coordy ,
                 text := ~region,
                 align := "left",
                 baseline := "bottom") %>%
      add_axis("x", title = "Average seat capacity of a plane") %>%
      add_axis("y", title = "Average utilization of planes in %") %>%
      scale_numeric("y", zero = T)
  })

  
  scatterVis %>%
    bind_shiny("wasted")
})