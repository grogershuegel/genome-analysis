#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(maps)
library(mapdata)
library(RColorBrewer)

###DATA###
US_time_series_confirmed_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")) %>%
  select(-c(UID, iso2, iso3, code3, FIPS)) %>% 
  pivot_longer(-c(Admin2, Province_State, Country_Region, Lat, Long_, Combined_Key),
               names_to = "Date", values_to = "Confirmed") 
# Let's get the times series data for deaths
US_time_series_deaths_long <- read_csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")) %>%
  select(-c(UID, iso2, iso3, code3, FIPS)) %>% 
  pivot_longer(-c(Admin2, Province_State, Country_Region, Lat, Long_, Combined_Key),
               names_to = "Date", values_to = "Deaths")
# Create Keys 
US_time_series_confirmed_long <- US_time_series_confirmed_long %>% 
  unite(Key, Combined_Key, Date, sep = ".", remove = FALSE)
US_time_series_deaths_long <- US_time_series_deaths_long %>% 
  unite(Key, Combined_Key, Date, sep = ".") %>% 
  select(Key, Deaths)

# Join tables
US_time_series_long_joined <- full_join(US_time_series_confirmed_long,
                                        US_time_series_deaths_long, by = c("Key")) %>% 
  select(-Key)
# Reformat the data
US_time_series_long_joined$Date <- mdy(US_time_series_long_joined$Date)
# Rename
US_time_series <- US_time_series_long_joined

names(US_time_series)

first_date = min(US_time_series$Date, na.rm = TRUE)
last_date = max(US_time_series$Date, na.rm = TRUE)
Report_Type = c("Confirmed", "Deaths")

###SHINY APP####
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19 Reporting Data in the US"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("select_type",
                        "Report Type",
                        Report_Type,
                        selected = "Confirmed"),
            sliderInput("slider_date",
                        "Date",
                        min = first_date,
                        max = last_date,
                        value = first_date,
                        step = 7)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      US_data_time <- US_time_series %>% 
        filter(Country_Region == "US") %>% 
        group_by(Province_State) %>% 
        filter(Date == input$slider_date) %>% 
        summarise_at(c("Confirmed", "Deaths"), sum) %>% 
        mutate(Province_State = tolower(Province_State))
      
      us<- map_data("state")
      state_join<- left_join(us, US_data_time, by=c("region"="Province_State"))
        
      ggplot(data = us, mapping = aes(x=long, 
                                             y=lat, 
                                             group=group))+
        coord_fixed(1.5)+
        geom_polygon(data = state_join, aes_string(fill = input$select_type), color= "white")+
        scale_fill_gradientn(colours=
                               brewer.pal(n=5, "OrRd"),
                             trans= "log10")+
        labs(title = "COVID-19 Reports in US")+
        theme_void()+
        theme(
          plot.background = element_rect(fill = "#ffffff", color = NA), 
          panel.background = element_rect(fill = "#ffffff", color = NA), 
          legend.background = element_rect(fill = "#ffffff", color = NA)
        )
      
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
