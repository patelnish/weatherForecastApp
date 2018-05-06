# Nisha Patel
# Code File for Dark Sky API weather forecasting


#----------- Load helper functions & load packages ---------------------#
source("helpers.R")
packagesList <- c("tidyverse", "lubridate", "stringr","jsonlite", 
                  "zipcode", "modelr", "leaflet", "shiny" )
lapply(packagesList, installPackages)
#---------------------------------------------------------#

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    ",
                    
                    "#sidebar {
                      background-color: #3877D6;
                      font-family: 'Arial Black';
                     

                    }"
                    
                    ))
    ),
  
  
  theme = "bootstrapDarkTheme.css",
  titlePanel( h1(" Hourly Weather Forecast App", 
                 style = "font-family: 'lobster', cursive; font-weight: 600; 
                         color: #3877D6;")),
  
  sidebarLayout(
    
    sidebarPanel(id = "sidebar",
      fluidRow(
        column(6, numericInput(inputId = 'zip', label = "Enter Your Zipcode", value = 0
                               ),
               actionButton(inputId = 'forecast', label= 'Forecast')),
        
        column(6, textInput(inputId = "plot_title", label = "Enter plot name to download"),
               
               downloadButton(outputId = "download_plot", label = "Download Plot"))
        
      ) ,# end fluidRow,
      
      fluidRow(tags$a(href = "https://darksky.net/poweredby/", "Powered by Dark Sky"))
      
    ), # end Sidebar Panel
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Forecast Plot", plotOutput(outputId = "location")),
    
                  tabPanel("Map View",  leafletOutput(outputId = "map"))
       )
    
      
    ) # end main panel
    
    
    
  ) # end Sidebar layout
  
  
 
 
  
) # end UI


server <- function(input, output){
  
  
  
  out <- eventReactive(input$forecast, { 
    print("clicked")
    
    
    location_output(input$zip)
     
  })
  
  
  out2 <- eventReactive(input$forecast, {
    
    data("zipcode")
    
    # Step 1 - Obtain zipcode information
    location =  zipcode %>% filter(zip == input$zip)
    
    user_latitude = location$latitude
    user_longitude = location$longitude
    user_city = str_c(location$city, location$state, sep = ",")
    
    
    m <- leaflet() %>% addTiles() %>% addMarkers(lng = user_longitude,
                                                 lat = user_latitude)
    m
    
  })
  
  
  output$map <- renderLeaflet(out2())
  
  
  output$location <- renderPlot(out())
 
  output$download_plot <- downloadHandler(
    
    filename = function() {
      
      str_c(input$plot_title, ".jpg", sep = "")
      
      },
    
    content = function(file) {
      
      ggsave(file, out(), width = 10, height = 6, units = "in")
      
      }
    
    
  )
    

  
 
} # end server


shinyApp(ui = ui, server = server)


