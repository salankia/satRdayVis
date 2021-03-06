library(shiny)
library(ggvis)


load("data/input_first_tab.RData")

shinyUI(
  navbarPage(
  "Tips & tricks to avoid the crowd",
  
  #################### Overall ################
  tabPanel("Most popular countries",
           fluidPage(
             title = "What were the most popular destinations in 2011?",
             fluidRow(
               column(12, includeMarkdown("data/intro.Rmd")), 
               column(12, includeMarkdown("data/1.Rmd")), 
               column(12, ggvisOutput('summary'))
             )
           )),
  #### Wasted seats #######
  tabPanel("Seat capacity and utilization of planes", 
           fluidPage(
             fluidRow(
               column(12, includeMarkdown("data/intro.Rmd")) 
             ),
             
             fluidRow(
               column(6, includeMarkdown("data/2.Rmd")),
               column(4, ggvisOutput('wasted_abs'), style = "height:300px;")
             ), 
             fluidRow(
               column(10, ggvisOutput('wasted'))
             )
           )
  ),
  #################### Summer and Winter ################
  tabPanel("Seasonal changes", 
           fluidPage(
             fluidRow(
               column(12, includeMarkdown("data/intro.Rmd")) 
             ),
             
             fluidRow(
               column(6, ggvisOutput('map'), style = "height:350px;width:400px;"),
               column(6, includeMarkdown("data/3.Rmd"))
             ),
             
             hr(),
             
             fluidRow(
               column(1,
                      h6("Under 20%"),
                      radioButtons('winter', '',
                                   unique(as.character(flights_summary$country[flights_summary$category_of_ratio == "Winter_specific"])))
               ),
               column(1,
                      h6("Between 20% and 50%"),
                      radioButtons('medium', '',
                                   unique(as.character(flights_summary$country[flights_summary$category_of_ratio == "Medium"])))
               ),
               column(1,
                      h6("Above 50%"),
                      radioButtons('summer', '',
                                   unique(as.character(flights_summary$country[flights_summary$category_of_ratio == "Summer_specific"])))
               ),
               column(9, ggvisOutput('timeseries'), style = "height:500px;")
             )
           )
      )
  )
  )