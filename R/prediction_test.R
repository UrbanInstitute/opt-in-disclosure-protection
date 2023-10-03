#' Prediction accuracy attribute inference test
#' 
#' This test evaluate how well a model trained on the synthetic data can predict
#' values on the confidential data.
#'
#' @param postsynth A postsynth object or tibble with synthetic data
#' @param data A data frame with the original data
#' @param formula A formula for the predictive model
#' @param model A model object from the parsnip package. Defaults to a decision 
#' tree.
#' @param recipe A recipe object from the recipes package. Defaults to no 
#' feature or target engineering.
#'
#' @return A numeric RMSE
#' 
#' @export
#'
prediction_test <- function(postsynth, data, formula, model = NULL, recipe = NULL) {
  
  if ("postsynth" %in% class(postsynth)) {
    
    synthetic_data <- postsynth$synthetic_data
    
  } else {
    
    synthetic_data <- postsynth
    
  }
  
  # train model -------------------------------------------------------------
  if (is.null(model)) {
    
    model <- parsnip::decision_tree(cost_complexity = 0.001) %>%
      parsnip::set_mode("regression") %>%
      parsnip::set_engine("rpart") 
    
  }
  
  if (is.null(recipe)) {
    
    recipe <- recipes::recipe(
      formula = formula,
      data = as.data.frame(synthetic_data)
    )
    
  }
  
  wflow <- 
    workflows::workflow() %>%
    workflows::add_model(model) %>% 
    workflows::add_recipe(recipe)
  
  synth_fit <- 
    wflow %>% 
    parsnip::fit(data = synthetic_data)
  
  # make predictions --------------------------------------------------------
  data <- dplyr::bind_cols(
    data,
    predict(synth_fit, new_data = data)
  )
  
  # calculate the RMSE
  yardstick::rmse(data = data, 
                  truth = rlang::f_lhs(formula), estimate = .pred)$.estimate
  
}