# myAPI.R
# Import all the libraries

library(plumber)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(yardstick)
library(readr)


# Load the dataset
health_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")
head(health_data)

# Convert variables to correct types
health_data <- health_data |>
  mutate(
    Diabetes_binary = factor(Diabetes_binary, levels = c(0,1), labels = c("No", "Yes")),
    HighBP = factor(HighBP, levels = c(0,1), labels = c("No", "Yes")),
    HighChol = factor(HighChol, levels = c(0,1), labels = c("No", "Yes")),
    CholCheck = factor(CholCheck, levels = c(0,1), labels = c("No", "Yes")),
    Smoker = factor(Smoker, levels = c(0,1), labels = c("No", "Yes")),
    Stroke = factor(Stroke, levels = c(0,1), labels = c("No", "Yes")),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c(0,1), labels = c("No","Yes")),
    PhysActivity = factor(PhysActivity, levels = c(0,1), labels = c("No", "Yes")),
    Fruits = factor(Fruits, levels = c(0,1), labels = c("No", "Yes")),
    Veggies = factor(Veggies, levels = c(0,1), labels = c("No", "Yes")),
    HvyAlcoholConsump = factor(HvyAlcoholConsump, levels = c(0, 1), labels = c("No", "Yes")),
    AnyHealthcare = factor(AnyHealthcare, levels = c(0,1), labels = c("No", "Yes")),
    NoDocbcCost = factor(NoDocbcCost, levels = c(0,1), labels = c("No", "Yes")),
    GenHlth = factor(GenHlth, levels = c(1:5), labels = c("Excellent", "Very Good", "Good", "Fair", "Poor")),
    DiffWalk = factor(DiffWalk, levels = c(0,1), labels = c("No", "Yes")),
    Sex = factor(Sex, levels = c(0,1), labels = c("Female", "Male")),
    Age = factor(Age, levels = c(1:13), labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80+")),
    Education = factor(Education, levels = c(1:6), labels = c("KG or No School", "Elementary", "Middle school", "High school", "College","Professional Degree")),
    Income = factor(Income, levels = c(1:8), labels = c("<10K", "$10k-$15k", "$15k-$20k", "$20k-$25k", "$25k-$35k", "$35k-$50k", "$50k-$75k", "$75k+"))
  )

# Convert BMI to numeric 
health_data$BMI <- as.numeric(health_data$BMI)
sum(is.na(health_data$BMI))

# Define the recipe 
LR2_recipe <- recipe(Diabetes_binary ~ BMI + Smoker + HighBP + HeartDiseaseorAttack + PhysActivity + Sex, 
                     data = health_data) |>
  step_normalize(BMI)

# Define the random forest model specification
rf_spec <- rand_forest(trees = 1000) |>
  set_engine("ranger") |>
  set_mode("classification")

# fit the workflow
rf_wkf <- workflow() |>
  add_recipe(LR2_recipe) |>
  add_model(rf_spec)

# Fit the model to the entire data
final_rf_model <- fit(rf_wkf, data = health_data)

# Define the Helper function to find the most prevalent class
get_most_prevalent_class <- function(column) {
  if(all(is.na(column))) {
    return("None")
  }
  column |>
    na.omit() |>
    table() |>
    which.max() |>
    names()
}

# Default values for predictors
default_values <- list(
  HighBP = get_most_prevalent_class(health_data$HighBP),
  BMI = mean(health_data$BMI, na.rm = TRUE),
  Smoker = get_most_prevalent_class(health_data$Smoker),
  HeartDiseaseorAttack = get_most_prevalent_class(health_data$HeartDiseaseorAttack),
  PhysActivity = get_most_prevalent_class(health_data$PhysActivity),
  Sex = get_most_prevalent_class(health_data$Sex)
)


#* Prediction endpoint
#* @param HighBP Blood pressure status (default: most prevalent)
#* @param BMI Body Mass Index (default: mean value)
#* @param Smoker Smoking status (default: most prevalent)
#* @param HeartDiseaseorAttack Heart disease status (default: most prevalent)
#* @param PhysActivity Physical activity status (default: most prevalent)
#* @param Sex Gender (default: most prevalent)
#* @get /pred
pred_endpoint <- function(HighBP = default_values$HighBP,
                          BMI = default_values$BMI,
                          Smoker = default_values$Smoker,
                          HeartDiseaseorAttack = default_values$HeartDiseaseorAttack,
                          PhysActivity = default_values$PhysActivity,
                          Sex = default_values$Sex) {
  
  
  # Create a new data frame with input values
  input_data <- tibble(
    HighBP = factor(HighBP, levels = c("No", "Yes")),
    BMI = as.numeric(BMI),
    Smoker = factor(Smoker, levels = c("No", "Yes")),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c("No", "Yes")),
    PhysActivity = factor(PhysActivity, levels = c("No", "Yes")),
    Sex = factor(Sex, levels = c("Female", "Male"))
  )
  
  # Make prediction with the fitted model
  prediction <- predict(final_rf_model, input_data) |>
    pull(.pred_class)
  
  return(list(prediction = prediction))
}


#* Info endpoint
#* @get /info
info_endpoint <- function() {
  return(list(
    name = "Arti",
    url = "https://github.com/Arti2190/Final-Project.git"
  ))
}





# Generate the predictions for the entire dataset
predictions <- predict(final_rf_model, health_data) |>
  bind_cols(health_data |> select(Diabetes_binary)) |>
  mutate(
    .pred_class = factor(.pred_class, levels = c("No", "Yes")),  
    Diabetes_binary = factor(Diabetes_binary, levels = c("No", "Yes"))  
  )

str(predictions)

#* Confusion matrix endpoint
#* @get /confusion
#* @serializer png
confusion_endpoint <- function() {
  # create confusion matrix 
  cm <- predictions |>
    conf_mat(Diabetes_binary, .pred_class)  
  
  # Create a confusion matrix plot (heatmap)
  cm_plot <- autoplot(cm, type = "heatmap") +
    ggtitle("Confusion Matrix Heatmap")
  
  #print(cm_plot)
  # Save the plot to a static file
  output_file <- "www/confusion_matrix.png"  
  ggsave(output_file, plot = cm_plot, device = "png", width = 7, height = 5, units = "in")
  
  # Return the file URL
  #list(url = paste0("http://localhost:8000/", output_file))
  # Return the confusion matrix as JSON
  as.list(cm$table)
}



# Explicitly create and run the Plumber object
  #pr <- Plumber$new()
#pr$print()
  #pr$handle("GET", "/pred", pred_endpoint)
  #pr$handle("GET", "/info", info_endpoint)
  #pr$handle("GET", "/confusion", confusion_endpoint)

# Run the API server
  #pr$run(port = 8000, swagger = TRUE)

#For cleaning the cache
#pr$removeHandle("GET", "/pred")
#plumb(file='new_api.R')$run()