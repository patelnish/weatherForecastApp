# Nisha Patel
# This file contains helper functions for app.R - creating an Hourly Weather Forecasting App

#----------Helper function to install necessary packages----------#
installPackages <- function(pack) { 
  
  # Install package if it is not already
  if (!(pack %in% installed.packages()[, "Package"])){ 
    
    install.packages(pack, repos='http://cran.us.r-project.org')
  }
  
  library(pack, character.only = TRUE)
  
  
} # end installPackages()


#----------Helper function to use zipcode data set to get latitude and longitude,
# and generate plot of hourly temperature----------#
location_plot_output <- function(zip_input){
  

  # Obtain zipcode from user and get associated entry in zipcode dataset
  location =  zipcodes %>% filter(zip == zip_input)
  
  
  # No zipcode provided yet by user, then return null
  if(nrow(location) > 1){
    
    return(NULL)
  }
  
  
  user_latitude = location$lat
  user_longitude = location$long
  user_city = str_c(location$city, location$state, sep = ",")
 
  
  # Generate plot 
  plot_out = generate_plot(user_latitude, user_longitude, user_city )
  
  
  plot_out
  

} # end location_plot_output()


#----------Helper function to use latitude and longitude to get data from Dark Sky API,
# generate plot of hourly temperature using weather forecast data----------#
generate_plot <- function(user_lat, user_long, user_city){
  
  
  #API request
  secret_key = "f25ffdcabb20bff27edea5afb5750d96"
  
  request = str_c(
    # BASE URL
    "https://api.darksky.net/forecast/",
    secret_key, '/',
    toString(user_lat), ',', 
    toString(user_long), 
    sep = "" )
  
  # Fetch Data!
  response = fromJSON(request)
  
  # Extract time, temperature, current time information
  temperature_list = response$hourly$data$temperature
  time_list = response$hourly$data$time
  timeStamp = response$currently$time
  
  # Ensure time is in EST
  current_timeStamp = as_datetime(timeStamp, tz = "EST")
  
  # Generate titles for labeling plot
  x = as.POSIXlt(current_timeStamp)
  time_title = paste(x$hour,x$min, sep = ":")
  date_title = paste(month(current_timeStamp), day(current_timeStamp), year(current_timeStamp), sep = "/")
  
  
  #-- Analysis ---#
  
  # Create Data Table
  df = tibble(time = time_list, temperature = temperature_list)
  
  # Convert epoch time to date time + add hour, weekday columns too
  df = df %>% mutate(date_time = as_datetime(time, tz = "EST"), 
                     time_hours = hour(date_time),
                     time_label = paste(as.POSIXlt(date_time)$hour,
                                        as.POSIXlt(date_time)$min, sep = ":"),
                     # make hours sequential - range from 0 upto 36 hours
                     time_full_hours = seq(0,nrow(df) - 1),
                     week_day = as.factor(wday(date_time, label = T, abbr = F)))
  
  # Generate plot with added linear trend line
  plot_title = str_c("Hourly Temperature Forecast in", user_city, "(as of", time_title, "on", date_title,")" ,sep = " ")
  forecast_plot1 = df %>% ggplot(mapping = aes(x = time_full_hours , y = temperature), color = "blue") + 
    geom_point(data = df, mapping = aes(x = time_full_hours, y = temperature, color = week_day)) + 
    geom_smooth(method = "lm",color = "blue") +
    
    labs(x = "Time (in Hours)", 
         y = "Temperature (in Degrees Fahrenheit)",
         title = plot_title,
         color = "Day of Week"
    ) 
  
  forecast_plot1
  
} # end generate_plot()

#----------Helper function to generate leaflet plot of location----------#
location_leaflet_output <- function(zip_input){

  # Obtain zipcode from user and get associated entry in zipcode dataset
  location =  zipcodes %>% filter(zip == zip_input)
  
  # No zipcode provided yet by user, then return null
  if(nrow(location) > 1){
    return(NULL)
  }

  user_latitude = location$lat
  user_longitude = location$long
  user_city = str_c(location$city, location$state, sep = ",")
  
  m <- leaflet() %>% addTiles() %>% addMarkers(lng = user_longitude,
                                               lat = user_latitude)
  m
} # end location_leaflet_output()
