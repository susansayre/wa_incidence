#functions for fitting predictions

#' Create data set to use for predicting
#' @param input_data full dataset of all possible variables/observation
#' @param analysis_variables dataframe identifying variables to use in this prediction case
#' @param remove_missing option to remove data with missing observations for any needed data (default = F)
#' @param remove_outliers optional percentage of outliers to remove, can be either a single two-element numeric vector that is applied to all dependent variables or a named list of two-element vectors that are independently applied to each dependent variable. In all cases, the first element covering the percentage to eliminate at the lower end and the second element is the percentage to eliminate at the upper end
#' @return a dataset with the subset of the observations in prediction_data
#' for which all of the variables listed in analysis_variables are non-missing
#' @export
create_prediction_set <- function(input_data, analysis_variables, remove_missing = F, remove_outliers = c()){

  predictor_variables <- analysis_variables %>%
    filter(cat == "P")

  predictor_var_list <- predictor_variables$var_name

  dependent_variables <- analysis_variables %>% filter(cat == "O")
  dependent_var_list <- dependent_variables$var_name

  factor_vars <- predictor_variables%>%
    filter(type %in% c("F","O"))

  needed_data <- input_data %>%
    ungroup() %>%
    select(one_of(dependent_var_list),one_of(predictor_var_list)) %>%
    mutate(dplyr::across(all_of(factor_vars$var_name), as_factor),
           dplyr::across(tidyselect::everything(), ~ replace(., is.nan(.), NA)))

  if (length(remove_outliers)>0) {

    for (i in dependent_var_list) {

      remove_shr <- tryCatch({remove_outliers[[i]]}, error = function(e) {remove_outliers})

      pvals <- c(remove_shr[1], 1 - remove_shr[2])
      bounds <- quantile(needed_data[[i]], pvals, na.rm = T)

      needed_data[[i]] <- replace(needed_data[[i]], needed_data[[i]] < bounds[1], NA)
      needed_data[[i]] <- replace(needed_data[[i]], needed_data[[i]] > bounds[2], NA)

    }

  }

  if (remove_missing) {
    needed_data <- needed_data %>%
      drop_na()
  }

  return(needed_data)

}

#' save predictor var info
#' @param prediction_set data used to predict outputs
#' @param analysis_variables dataframe identifying variables to use in this prediction case
#' @param output_dir directory to save file in
#' @return output file name
#' @export
save_predictor_var_info <- function(prediction_set, analysis_variables, output_dir){

  factor_vars <- analysis_variables %>%
    filter(cat %in% c("P"), type == "F") %>%
    select(var_name) %>%
    simplify()

  unordered_factor_levels <- map(factor_vars, ~ levels(prediction_set[[.x]])) %>%
    set_names(factor_vars)

  ordered_vars <- analysis_variables %>%
    filter(cat %in% c("P"), type == "O") %>%
    select(var_name) %>%
    simplify()

  ordered_factor_levels <- map(ordered_vars, ~ levels(prediction_set[[.x]])) %>%
    set_names(ordered_vars)

  predictor_info <- list(factor_var_levels = unordered_factor_levels,
                         ordered_var_levels = ordered_factor_levels)

  file_name <- file.path(output_dir, "factor_levels.rds")

  saveRDS(predictor_info, file = file_name)

  return(file_name)
}

#'
#' find predictor vars
#' @param analysis_variables tibble of analysis variables categorized
#' @return list of predictor variable names
#' @export
find_predictor_vars <- function(analysis_variables) {
  predictor_variables <- analysis_variables %>% filter(cat=="P")
  predictor_var_list <- predictor_variables$var_name
}

#' find dependent vars
#' @param analysis_variables tibble of analysis variables categorized
#' @return list of dependent variable names
#' @export
find_dependent_vars <- function(analysis_variables) {
  dependent_variables <- analysis_variables %>% filter(cat == "O")
  dependent_var_list <- dependent_variables$var_name
}

#' split data into training and testing
#' @param input_data input data set to split
#' @param training_share share to use for training
#' @return a list of two datasets, the first one is the training data and the
#' second is the testing data
#' @export
split_samples <- function(input_data, training_share) {
  set.seed(123)
  training.samples <- input_data$tot_income %>%
    caret::createDataPartition(p = training_share, list = F)
  train.data <- input_data[training.samples, ]
  test.data <- input_data[-training.samples, ]

  samples <- list(train = train.data, test = test.data)

}

#' fit and save a model
#' @param data training data set
#' @param analysis_vars analysis variables for this case
#' @param output_var variable to fit
#' @param output_dir directory to store outputs in
#' @return path to folder containing saved fit objects
#' @export
fit_and_save.dhxgboost <- function(samples, analysis_vars, output_var, output_dir, reuse = F){

  output_file = here::here(output_dir, str_c(output_var, 
                                             c("_fit_buy.rds", "_fit_amt.rds")))
  if (any(c(!file.exists(output_file),!reuse))){
  predictor_var_list <- find_predictor_vars(analysis_vars)

    input_data <- samples$train %>%
      rename(output_value = !!output_var) %>%
      select(output_value, one_of(predictor_var_list))

    set.seed(123)

   if (output_var %in% c("totexp", "taxable_expend")){
     fit_buy <- "not relevant"
   } else {
     buy_data <- input_data %>%
       mutate(purchased = as.factor(output_value > 0)) %>%
       select(-output_value)

     fit_buy <- caret::train(
       purchased ~ .,
       data = buy_data,
       method = "xgbTree",
       trControl = caret::trainControl("cv", number = 10),
       verbosity = 0
     )
     }

   saveRDS(fit_buy, output_file[[1]])

   amt_data <- input_data %>%
     filter(output_value > 0) %>%
     mutate(log_amt = log(output_value)) %>%
     select(-output_value)

   fit_amt <- caret::train(
     log_amt ~ .,
     data = amt_data,
     method = "xgbTree",
     trControl = caret::trainControl("cv", number = 10),
     verbosity = 0
   )

   saveRDS(fit_amt, output_file[[2]])
}
  return(output_file)
}

#' test a fit object
#' @param data testing data set
#' @param output_var variable to test
#' @param fit_files directory containing fit models
#' @return tibble row with product name and rmse
#' @export
test_fit.dhxgboost <- function(new_data, output_var, fit_files){

  predict <- predict_product.dhxgboost(new_data$test, output_var, fit_files)
  output <- tibble(product = output_var,
                   rmse = sqrt(mean((predict - new_data$test[[output_var]])^2)))
}

#' simulate expected expenditures
#' @param synthetic_data data on households to predict output for
#' @param products list of products to predict
#' @param fit_files list of fit_file_names
#' @return dataset of simulated expenditures coupled with prediction and simulation variables
#' @export
predict_expenditures.dhxgboost <- function(synthetic_data, products, fit_info){

  #temporary kludge to handle missing data
  synthetic_data <- synthetic_data %>% drop_na()

  predictions <-  set_names(products) %>%
    map_dfc( ~ predict_product.dhxgboost(synthetic_data,
                                         .x,
                                         fit_info[[which(str_detect(names(fit_info), .x))]])) %>%
    bind_cols(select(synthetic_data, GEOID, hhkey, final_weight)) %>%
    pivot_longer(cols = -c("GEOID", "hhkey", "final_weight"),
                 names_to = "product", values_to = "consumption")

  return(predictions)
}

#' predict single product dhxgboost
#' @param new_data predictor_variable data
#' @param output_var varible to fit
#' @param fit_file names of the files containing the fit model for this product
#' @return vector of prediction values
#' @export
predict_product.dhxgboost <- function(new_data, output_var, fit_files){

  amt_model <- readRDS(fit_files[[2]])
  predict_log_amt <- predict(amt_model, new_data)

  if (output_var %in% c("totexp", "taxable_expend")){
    predict <- exp(predict_log_amt)
  } else {

    buy_model <- readRDS(fit_files[[1]])
    predict_buy <- predict(buy_model, new_data)
    predict <- (predict_buy == TRUE)*exp(predict_log_amt)
  }

  return(predict)
  }
