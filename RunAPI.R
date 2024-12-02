# Run my API
library(plumber)
#library(randomForest)

r <- plumb("myAPI.R")

# run it on the port in the DockerFile 
r$run(port = 8000)




