
installPackages <- function(pack) { 
  
  if (!(pack %in% installed.packages()[, "Package"])){ 
    print("installing now")
    install.packages(pack, repos='http://cran.us.r-project.org', lib = "C:/Users/nishap/Desktop/app" )
  }
  
  library(pack, character.only = TRUE)
  
  
} # end installPackages



location_output <- function(zip_input){
  
  data("zipcode")
  
  # Step 1 - Obtain zipcode information
  location =  zipcode %>% filter(zip == zip_input)
  print(nrow(location))
  if(nrow(location) > 1){
    print("Null returned")
    return(NULL)
  }
  user_latitude = location$latitude
  user_longitude = location$longitude
  user_city = str_c(location$city, location$state, sep = ",")
  plot_city <<- user_city
  
  # generate plot
  plot_out = generate_plot(user_latitude, user_longitude, user_city )
  
  
  plot_out
  

}

generate_plot <- function(user_lat, user_long, user_city){
  
  
  #Step 2. Get data using Dark Sky API
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
  
  # Step 3. Extract time, temperature, current time
  temperature_list = response$hourly$data$temperature
  time_list = response$hourly$data$time
  
  
  # Save current time details for plot titles
  timeStamp = response$currently$time
  current_timeStamp = as_datetime(timeStamp, tz = "EST")
  x = as.POSIXlt(current_timeStamp)
  time_title = paste(x$hour,x$min, sep = ":")
  date_title = paste(month(current_timeStamp), day(current_timeStamp), year(current_timeStamp), sep = "/")
  
  
  #----------- Analysis ---------------------#
  
  # Step 4. Create Data Table
  df = tibble(time = time_list, temperature = temperature_list)
  
  # Step 5. Convert epoch time to date time + add hour, weekday columns too
  df = df %>% mutate(date_time = as_datetime(time, tz = "EST"), 
                     time_hours = hour(date_time),
                     time_label = paste(as.POSIXlt(date_time)$hour,
                                        as.POSIXlt(date_time)$min, sep = ":"),
                     time_full_hours = seq(0,nrow(df) - 1),
                     week_day = as.factor(wday(date_time, label = T, abbr = F)))
  
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
  
  
}
