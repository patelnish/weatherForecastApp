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

  
} 

location_plot_output(48307)

# scratchwork

############################################################
# run get query for meteomatics temperature - 7 day difference

meteomatics_username=''
meteomatics_password=''

# general API format of requests
# https://api.meteomatics.com/<validdatetime>/<parameters>/<location>/<format>?<optionals>

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

# verbosity can be adjusted for output
output_json = resp_body_json(output)




