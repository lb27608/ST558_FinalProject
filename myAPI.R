#ST558 - Final Project
#Lee Bennett
#API file for final model

#Load required packages
library(readr)
library(tidyverse)
library(tidymodels)
library(ranger)

#Load raw data
diabetes_raw <- read_csv(file="diabetes_binary_health_indicators_BRFSS2015.csv",show_col_types=FALSE)

#Create an analysis dataset containing only the relevant variables
yn_label <- c("No","Yes")
diabetes_ad <- diabetes_raw |> select(Diabetes_binary, HighBP ,HighChol, BMI, Smoker, Sex, AnyHealthcare, GenHlth) |>
  mutate(diabetes = factor(Diabetes_binary, levels=c(0,1), labels=c("No diabetes", "Diabetes or prediabetes")),
         high_bp = factor(HighBP, levels=c(0,1), labels=yn_label),
         high_chol = factor(HighChol, levels=c(0,1), labels=yn_label),
         sex = factor(Sex, levels=c(0,1), labels=c("Female","Male")),
         smoker = factor(Smoker, levels=c(0,1), labels=c("Less than 100 packs or none", "At least 100 packs")),
         healthcare = factor(AnyHealthcare, levels=c(0,1), labels=yn_label),
         gen_health = factor(GenHlth, levels=c(1,2,3,4,5), labels=c("Excellent", "Very good", "Good", "Fair", "Poor")),
         .keep="unused")


#Fit the final random forest model to the full dataset
rf_spec <- rand_forest(mtry = 3) |> set_engine("ranger") |> set_mode("classification")
tree_recipe <- recipe(diabetes ~ ., data = diabetes_ad) |> step_normalize(BMI)
rf_wkf <- workflow() |> add_recipe(tree_recipe) |> add_model(rf_spec)

rf_full_fit <- rf_wkf |> fit(diabetes_ad)


#* Choose a variable
#* @param var Predictor variable
#* @param val Value of the predictor
#* @get /pred

#http://localhost:PORT/pred?var=&val=

#Info endpoint
#* @get /info
function(){
  
  
}

