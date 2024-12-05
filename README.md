# weatherForecastApp
This repository contains code to run an Hourly Temperature Forecasting App using Shiny in R

## Getting Started
1. Download files together.
2. Go to https://www.meteomatics.com/en/weather-api/weather-api-free/ and sign up for the free version of the
	Weather API.
3. Upon signing up, you should receive an email with your login details (username and password). You can watch a tutorial video here: https://www.youtube.com/watch?v=IzmST1Lht3c&ab_channel=Meteomatics or consult the documentation here: https://www.meteomatics.com/en/api/getting-started/
4. Follow instructions here: https://www.meteomatics.com/en/api/request/api-requests-oauth-authentification/ in order to obtain a token
5. Fill in the corresponding parameters in the credentials file: meteomatics_creds.json


## Running the App
1. Run app.R on commandline using the following line: Rscript app.R (alternatively source the file on RStudio!). If you are not sure that you have installed all the required packages, then you can run lines 8-14 in the app.R file first before proceeding to run app.R as a whole.
2. Enter a zipcode into the app (must be a 5-digit zipcode) and press the 'Forecast' button.
3. Use the 'Forecast Plot' tab view to see the temperature for the next 7 days.
4. Use the 'Map View' tab to see the latitude and longitude of the input zipcode on a leaflet map.


