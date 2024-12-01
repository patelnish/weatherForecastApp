# Nisha Patel
# This file contains helper functions for app.R - creating an Hourly Weather Forecasting App

#----------Helper function to install necessary packages----------#
installPackages <- function(pack) { 
  
  # Install package if it is not already
  if (!(pack %in% installed.packages()[, "Package"])){ 
    
    install.packages(pack, repos='http://cloud.r-project.org')
  }
  
  library(pack, character.only = TRUE)
  
  
} # end installPackages()


#----------Helper function to take in user zipcode input and get latitude and longitude
location_plot_output <- function(zipcode_input){
  
  # now geocode itself error checks on zipcode input
  location =  geocode_zip(zip_code = zipcode_input)
  
  
  user_latitude = location$lat
  user_longitude = location$lng
  
  reverse_zipcode_data = reverse_zipcode(location$zipcode)
  
  user_city_state = str_c(reverse_zipcode_data$major_city[[1]], reverse_zipcode_data$state[[1]], sep = ",")

  
  # # Generate plot 
  # plot_out = generate_plot(user_latitude, user_longitude, user_city )
  # 
  # 
  # plot_out
  
} 

############################################################
# use latitue and longitude to run API request
# general API format of requests
# https://api.meteomatics.com/<validdatetime>/<parameters>/<location>/<format>?<optionals>
run_api_request <- function(user_latitude, user_longitude) {
  
  base_url = "api.meteomatics.com"
  
  date_today = lubridate::today()
  date_in_one_week = date_today + 7
  
  date_today_formatted = str_c(format_ISO8601(as.POSIXct(date_today, tz = "UTC")), 'Z')
  date_in_one_week_formatted = str_c(format_ISO8601(as.POSIXct(date_in_one_week, tz = "UTC")), 'Z')
  interval_formatted = str_c(date_today_formatted, date_in_one_week_formatted, sep='--')
  
  interval_step = 'PT1H'
  
  time_interval_syntax = str_c(interval_formatted, interval_step, sep=":")
  
  # Using httr2 library
  
  # Instantaneous temperature at 2m above ground in degrees Fahrenheit
  temp_option = "t_2m:F"
  
  lat_long_formatted = str_c(user_latitude, user_longitude, sep=",")
  
  output_format = "json?"
  
  formatted_request_url = str_c(base_url, time_interval_syntax, temp_option, lat_long_formatted, output_format, sep='/')
  
  req <- request(formatted_request_url) |> req_auth_basic(username = meteomatics_username, password = meteomatics_password)
  
  # can dry run a request and set headers to False/True to debug
  req |> req_dry_run()
  req |> req_dry_run(redact_headers = FALSE)
  
  output = req_perform(req)
}


api_request_output = run_api_request(user_latitude, user_longitude)

json_response = resp_body_json(api_request_output)

hourly_temp_data = json_response$data[[1]]$coordinates[[1]]$dates
hourly_temp_df = do.call(rbind.data.frame, hourly_temp_data)
hourly_temp_df$date = as_datetime(hourly_temp_df$date)

# Extract time, temperature, current time information
temperature_list = as.numeric(hourly_temp_df$value)
time_list = hourly_temp_df$date
timeStamp = json_response$dateGenerated

# Ensure time is in EST
current_timeStamp = as_datetime(timeStamp, tz = "EST")

# Generate titles for labeling plot
x = as.POSIXlt(current_timeStamp)
time_title = paste(x$hour,x$min, sep = ":")
date_title = paste(lubridate::month(current_timeStamp), lubridate::day(current_timeStamp), lubridate::year(current_timeStamp), sep = "/")


#-- Analysis ---#

# Create Data Table
df = tibble(time = time_list, temperature = temperature_list)

# Convert epoch time to date time + add hour, weekday columns too
df_full = df %>%  
  dplyr::mutate(
      date_time = as_datetime(time, tz = "EST"), 
      time_hours = lubridate::hour(date_time),
      time_label = paste(as.POSIXlt(date_time)$hour,
                        as.POSIXlt(date_time)$min, sep = ":"),

            # make hours sequential - range from 0 upto 36 hours
      time_full_hours = seq(0,nrow(df) - 1),
      week_day = as.factor(lubridate::wday(time, label = TRUE, abbr = FALSE))
  )

# Generate plot with added linear trend line
plot_title = str_c("Hourly Temperature Forecast in", user_city_state, "(as of", time_title, "on", date_title,")" ,sep = " ")
forecast_plot1 = df_full %>% ggplot(mapping = aes(x = time_full_hours , y = temperature), color = "blue") + 
  geom_point(data = df_full, mapping = aes(x = time_full_hours, y = temperature, color = week_day)) + 
  geom_smooth(method = "lm",color = "blue") +
  
  labs(x = "Time (in Hours)", 
       y = "Temperature (in Degrees Fahrenheit)",
       title = plot_title,
       color = "Day of Week"
  ) 

forecast_plot1








