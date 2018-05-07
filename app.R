# Nisha Patel
# Code File to Create R Shiny App
# App: Hourly Temperature Forecast (Powered by Dark Sky: https://darksky.net/poweredby/)


#----------- Load helper functions & load packages ---------------------#
packagesList <- c("tidyverse", "lubridate", "stringr","jsonlite", 
                  "zipcode", "modelr", "leaflet", "shiny" )
lapply(packagesList, installPackages)
source("helpers.R")
data("zipcode")
#---------------------------------------------------------#

ui <- fluidPage(
  
  # CSS related code to style app page
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    ",
                    
                    "#sidebar {
                      background-color: #3877D6;
                      font-family: 'Arial Black';
                      font-weight:200;
                     
                    }"
                    
                    ))
    ),
  
  
  theme = "bootstrapDarkTheme.css",
  titlePanel( h1(" Hourly Weather Forecast App", 
                 style = "font-family: 'lobster', cursive; font-weight: 400; 
                         color: #3877D6;")),
  
  sidebarLayout(
    
    sidebarPanel(id = "sidebar",
                 
      fluidRow(
        
        # Zip input, forecast, plot title input, and download plot buttons
        column(6, numericInput(inputId = 'zip', label = "Enter Your Zipcode", value = 0
                               ),
               actionButton(inputId = 'forecast', label= 'Forecast')),
        
        column(6, textInput(inputId = "plot_title", label = "Enter plot name to download"),
               
               downloadButton(outputId = "download_plot", label = "Download Plot"))
        
      ) , # end fluidRow
      
      # Note: Attribution to Dark Sky
      fluidRow(tags$a(href = "https://darksky.net/poweredby/", "Powered by Dark Sky"))
      
    ), # end sidebar panel
    
    mainPanel(
      
      # First tab is forecast plot, second tab has leaflet map of location 
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Forecast Plot", plotOutput(outputId = "location")),
    
                  tabPanel("Map View",  leafletOutput(outputId = "map"))
       )
    
      
    ) # end main panel
    
    
    
  ) # end sidebar layout
  

  
) # end UI


server <- function(input, output){
  
  # After zip input, if user clicks forecast, then hourly temp plot will be generated
  out_plot <- eventReactive(input$forecast, { 
    
    location_plot_output(input$zip)
     
  })
  
  # After zip input, if user clicks forecast, then leaflet map of input location will be generated
  out_leaflet <- eventReactive(input$forecast, {
    
    location_leaflet_output(input$zip)
    
  })
  
  # Show plot 
  output$map <- renderLeaflet(out_leaflet())
  
  # Show leaflet
  output$location <- renderPlot(out_plot())
 
  
  # Enable downloading and saving of plot
  output$download_plot <- downloadHandler(
    
    filename = function() {
      
      # Let user choose title for saving plot based on input
      str_c(input$plot_title, ".jpg", sep = "")
      
      },
    
    content = function(file) {
      
      ggsave(file, out(), width = 10, height = 6, units = "in")
      
      }
    
  )
    
  
 
} # end server


shinyApp(ui = ui, server = server)


