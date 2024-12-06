# myAPI.R
# Import all the libraries

library(plumber)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(yardstick)
library(readr)

library(plumber)

# Define an example endpoint
#* @get /hello
#* @response 200
function() {
  return("Hello, World!")
}

# This starts the API
pr <- plumb('testapi')
pr$run(host = '0.0.0.0', port = 8000, swagger = TRUE)
