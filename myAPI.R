# Load required libraries
library(plumber)
library(tidymodels)
library(dplyr)
library(ggplot2)
library(yardstick)

# Load the dataset
health_data <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")
head(health_data)

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

health_data


str(health_data)
# Define the recipe

LR2_recipe <- recipe(Diabetes_binary ~ BMI + Smoker + HighBP + HeartDiseaseorAttack + PhysActivity + Sex,
                     data = health_data) |>
  step_normalize(all_numeric_predictors(), -all_outcomes())

# Define the model
rf_spec <- rand_forest(mtry = tune(),
                       trees = 1000, 
                       min_n = tune()) |>
  set_engine("ranger") |>
  set_mode("classification") 

# Define the workflow
rf_wkf <- workflow() |>
  add_recipe(LR2_recipe) |>
  add_model(rf_spec)

# Fit the model on the entire dataset
#colnames(health_data)
final_rf_model <- fit(rf_wkf, data = health_data)

# 1. Define the pred endpoint

# Helper function to find the most prevalent class
get_most_prevalent_class <- function(column) {
  column %>%
    na.omit() %>%
    table() %>%
    which.max() %>%
    names()
}

# Default values for predictors
default_values <- list(
  HighBP = get_most_prevalent_class(diabetes_train$HighBP),
  HighChol = get_most_prevalent_class(diabetes_train$HighChol),
  BMI = mean(diabetes_train$BMI, na.rm = TRUE),
  Smoker = get_most_prevalent_class(diabetes_train$Smoker),
  HeartDiseaseorAttack = get_most_prevalent_class(diabetes_train$HeartDiseaseorAttack),
  PhysActivity = get_most_prevalent_class(diabetes_train$PhysActivity),
  Sex = get_most_prevalent_class(diabetes_train$Sex)
)


#   # Make prediction with the fitted model
# 1. Define the pred endpoint
# @param HighBP The blood pressure status (default: most prevalent)
# @param HighChol The cholesterol status (default: most prevalent)
# @param BMI The body mass index (default: mean value)
# @param Smoker Smoking status (default: most prevalent)
# @param HeartDiseaseorAttack Heart disease or attack status (default: most prevalent)
# @param PhysActivity Physical activity status (default: most prevalent)
# @param Sex Gender (default: most prevalent)
# @get /pred
function(HighBP = default_values$HighBP,
         HighChol = default_values$HighChol,
         BMI = default_values$BMI,
         Smoker = default_values$Smoker,
         HeartDiseaseorAttack = default_values$HeartDiseaseorAttack,
         PhysActivity = default_values$PhysActivity,
         Sex = default_values$Sex) {
  
  # Create a new data frame with input values
  input_data <- tibble(
    HighBP = factor(HighBP, levels = c("No", "Yes")),
    HighChol = factor(HighChol, levels = c("No", "Yes")),
    BMI = as.numeric(BMI),
    Smoker = factor(Smoker, levels = c("No", "Yes")),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, levels = c("No", "Yes")),
    PhysActivity = factor(PhysActivity, levels = c("No", "Yes")),
    Sex = factor(Sex, levels = c("Female", "Male"))
  )
  
  # Make prediction with the fitted model
  prediction <- predict(final_rf_model, input_data) %>%
    pull(.pred_class)
  
  return(list(prediction = prediction))
}

# 2. Define the info endpoint
# @get /info
function() {
  return(list(
    name = "Arti",
    url = "https://github.com/Arti2190/Final-Project.git"
  ))
}

# 3. Define the confusion endpoint
# @get /confusion
function() {
  # Get the predictions for the entire dataset
  predictions <- predict(final_rf_model, health_data) %>%
    bind_cols(health_data %>% select(Diabetes_binary))
  
  # Create a confusion matrix
  cm <- conf_mat(predictions, truth = Diabetes_binary, estimate = .pred_class)
  
  # Plot the confusion matrix
  cm_plot <- autoplot(cm)
  
  return(list(confusion_matrix_plot = cm_plot))
}

 #   name = "Arti",
#    github_url = "https://github.com/Arti2190/Final-Project.git"


