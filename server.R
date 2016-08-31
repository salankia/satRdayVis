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
      group_by(total_number_of_passangers_per_month_log) %>%
      layer_bars(stroke = ~category) %>%
      layer_points(fill = ~category, size := 22)  %>% 
      layer_text(x = ~total_number_of_passangers_per_month_log - 0.01,
                 y = ~rank ,
                 text := ~region,
                 align := "right",
                 baseline := "bottom",
                 stroke = ~category) %>%
      layer_rects(data = data.frame(x = c(1, 2, 3, 4, 5),
                                    x2 = c(2, 3, 4, 5, 6),
                                    y = rep(0.25, times = 5),
                                    y2 = rep(-0.25, times = 5),
                                    color = c("1", "2", "3", "4", "5")),
                  x = ~x, y = ~y, x2 = ~x2, y2 = ~y2, fill = ~ color,
                  stroke = ~color) %>%
      hide_legend("stroke") %>%
      hide_legend("fill") %>%
      add_axis("x", title = "", grid = F, values = c(2:6)) %>%
      hide_axis("y") %>%
      scale_numeric("x", nice = T, expand = 0.03) %>%
      scale_nominal("stroke", range = c("#3288bd", "#66c2a5", "#fdae61", "#d53e4f", "#5e4fa2")) %>%
      scale_nominal("fill", range = c("#3288bd", "#66c2a5", "#fdae61", "#d53e4f", "#5e4fa2")) %>%
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
      layer_points(fill = ~color) %>%
      layer_text(x = ~coordx + 2,
                 y = ~coordy ,
                 text := ~region,
                 align := "left",
                 baseline := "bottom") %>%
      add_axis("x", title = "Average seat capacity of a plane") %>%
      add_axis("y", title = "Average utilization of planes in %") %>%
      scale_nominal("fill", range = c("#1f78b4", "#33a02c", "#ff7f00", "#e31a1c", "#6a3d9a")) %>%
#      scale_numeric("y", zero = T) %>%
      hide_legend("fill")%>%
      set_options(width = "auto", height = "auto", resizable=FALSE)
  })

  
  scatterVis %>%
    bind_shiny("wasted")
  
  
  ########## Wasted absolute value
  
  scatterAbsVis = reactive({
    avg %>%
      ggvis(x = ~ avg_seat, y = ~ avg_p_p)%>%
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
      hide_legend("stroke") %>% hide_legend("fill")  %>%
      layer_points(fill = ~color)%>%
      add_tooltip(function(x){
        print(x)
        if(!"color" %in% names(x)) return(NULL)
        region <- avg$region[round(avg$avg_p_p, digits = 3) == round(x$avg_p_p, digits = 3) &
                               round(x$avg_seat, digits = 3) == round(avg$avg_seat, digits = 3)]
        print(region)
        s <- paste0("", region)
        s
      }, on = "hover") %>%
      set_options(width = "auto", height = "auto", resizable=FALSE)
  })
  scatterAbsVis %>% bind_shiny("wasted_abs")
})
