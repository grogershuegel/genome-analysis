#
# Make a Shiny App with a graph of the time series data of confirmed cases 
# for with the US and four other countries. Add a slide bar that allows the 
# user to adjust the date.
#

####DATA
library(shiny)
library(tidyverse)
library(lubridate)
time_series_confirmed_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region")  %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Confirmed") 
# Let's get the times series data for deaths
time_series_deaths_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region")  %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Deaths")
time_series_recovered_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")) %>%
    rename(Province_State = "Province/State", Country_Region = "Country/Region") %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long),
                 names_to = "Date", values_to = "Recovered")
# Create Keys 
time_series_confirmed_long <- time_series_confirmed_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep = ".", remove = FALSE)
time_series_deaths_long <- time_series_deaths_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep = ".") %>% 
    select(Key, Deaths)
time_series_recovered_long <- time_series_recovered_long %>% 
    unite(Key, Province_State, Country_Region, Date, sep = ".") %>% 
    select(Key, Recovered)
# Join tables
time_series_long_joined <- full_join(time_series_confirmed_long,
                                     time_series_deaths_long, by = c("Key"))
time_series_long_joined <- full_join(time_series_long_joined,
                                     time_series_recovered_long, by = c("Key")) %>% 
    select(-Key)
# Reformat the data
time_series_long_joined$Date <- mdy(time_series_long_joined$Date)
# Create Report table with counts
time_series_long_joined_counts <- time_series_long_joined %>% 
    pivot_longer(-c(Province_State, Country_Region, Lat, Long, Date),
                 names_to = "Report_Type", values_to = "Counts")
# rename the data
global_time_series <- time_series_long_joined
# Get first and last date for graph ***There are NA in the date field to consider
first_date = min(global_time_series$Date, na.rm = TRUE)
last_date = max(global_time_series$Date, na.rm = TRUE)
# Define reporting types
Report_Type = c("Confirmed", "Deaths", "Recovered")
# Create list of countries
Countries = global_time_series$Country_Region

###THE SHINY APP#####

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Reporting Data"),
    
    #Vertical
    verticalLayout(

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("dates", 
                           label = "Date range", 
                           start = first_date)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("datePlot")
        )
    )
),
tags$hr(),

    #Sidebar 2
    sidebarLayout(
        sidebarPanel(
          selectInput("select_type", 
                      "Report Type", 
                      Report_Type, 
                      selected = "Confirmed"),
      ),
      
      mainPanel(
        plotOutput("reportPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   #Plot 1
   output$datePlot <- renderPlot({
        pick_date<- global_time_series%>% 
            group_by(Country_Region,Date) %>% 
            summarise_at(c("Confirmed"), sum) %>% 
            filter (Country_Region %in% c("US","Italy","China", 
                                          "Australia", "France"))
        
        ggplot(pick_date, aes_string(x = "Date",  
                                     y = "Confirmed", 
                                     color = "Country_Region")) + 
            geom_point() +
            geom_line()+
            labs(color= "Country")+
            xlim(input$dates)+
            ggtitle("Confirmed COVID-19 Cases")
    })
    
    #Plot 2
    output$reportPlot <- renderPlot({
      pick_date<- global_time_series%>% 
        group_by(Country_Region, Date) %>% 
        summarise_at(c("Confirmed", "Recovered", "Deaths"), sum) %>% 
        filter (Country_Region %in% c("US","Italy","China", 
                                      "Australia", "France"))
      
      ggplot(pick_date, aes_string(x = "Date",  
                                   y = input$select_type, 
                                   color = "Country_Region")) + 
        geom_point() +
        geom_line()+
        labs(color= "Country")+
        ggtitle("COVID-19 Reported Cases, Deaths, and Recoveries")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
