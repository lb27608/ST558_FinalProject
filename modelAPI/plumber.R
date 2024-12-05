#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
library(readr)
library(tidyverse)
library(tidymodels)
library(ranger)

#Create the final random forest model for the API
#Load raw data
diabetes_raw <- read_csv(file="../diabetes_binary_health_indicators_BRFSS2015.csv",show_col_types=FALSE)

#Create an analysis dataset containing only the relevant variables
yn_label <- c("No","Yes")
diabetes_ad <- diabetes_raw |> select(Diabetes_binary, HighBP ,HighChol, BMI, Smoker, Sex, AnyHealthcare, GenHlth) |>
  mutate(diabetes = factor(Diabetes_binary, levels=c(0,1), labels=c("No diabetes", "Diabetes or prediabetes"))) |>
  select(-Diabetes_binary)

#Fit the final random forest model to the full dataset
rf_spec <- rand_forest(mtry = 3) |> set_engine("ranger") |> set_mode("classification")
tree_recipe <- recipe(diabetes ~ ., data = diabetes_ad) |>
  step_mutate(high_bp = factor(HighBP, levels=c(0,1), labels=yn_label),
              high_chol = factor(HighChol, levels=c(0,1), labels=yn_label),
              sex = factor(Sex, levels=c(0,1), labels=c("Female","Male")),
              smoker = factor(Smoker, levels=c(0,1), labels=c("Less than 100 packs or none", "At least 100 packs")),
              healthcare = factor(AnyHealthcare, levels=c(0,1), labels=yn_label),
              gen_health = factor(GenHlth, levels=c(1,2,3,4,5), labels=c("Excellent", "Very good", "Good", "Fair", "Poor"))) |>
  step_normalize(BMI)
rf_wkf <- workflow() |> add_recipe(tree_recipe) |> add_model(rf_spec)

rf_full_fit <- rf_wkf |> fit(diabetes_ad)

#Get predicted and true values for confusion matrix
preds <- predict(rf_full_fit, new_data=diabetes_ad)
cm_tib<-tibble(diabetes_ad$diabetes,preds$.pred_class)
names(cm_tib)<-c("truth","predicted")

#Create shell of tibble for prediction with mean/most prevalent classes
pred_shell <- diabetes_ad[1,] |> select(-c(diabetes))

#* @apiTitle ST558 Final Project API
#* @apiDescription Plumber example description.

#* Info endpoint
#* @serializer print
#* @get /info
function() {
    urlname <- "https://lb27608.github.io/ST558_FinalProject/EDA.html"
    fstring <- paste0("Lee Bennett,",urlname)
    print(fstring)
}

#* Predictor endpoint
#* @param HighBP 0 or 1
#* @param HighChol 0 or 1
#* @param BMI numeric
#* @param Smoker 0 or 1
#* @param Sex 0 or 1
#* @param AnyHealthcare 0 or 1
#* @param GenHlth numeric 1 to 5\
#* @serializer print
#* @get /pred
function(HighBP=0, HighChol=0, BMI=28.38, Smoker=0, Sex=0, AnyHealthcare=1, GenHlth=2){
  new_data <- pred_shell
  new_data$HighBP <- as.numeric(HighBP)
  new_data$HighChol <- as.numeric(HighChol)
  new_data$BMI <- as.numeric(BMI)
  new_data$Smoker <- as.numeric(Smoker)
  new_data$Sex <- as.numeric(Sex)
  new_data$AnyHealthcare <- as.numeric(AnyHealthcare)
  new_data$GenHlth <- as.numeric(GenHlth)
  
  newpred <- predict(rf_full_fit,new_data=new_data)
  print(newpred$.pred_class)
}


#Confusion matrix endpoint
#* Display confusion matrix
#* @serializer png 
#* @get /confusion
function(){
  cm <- cm_tib |> conf_mat(truth,predicted)
  cmplot <- autoplot(cm, type="heatmap")
  print(cmplot)
}
