#' Compute AUC and var importance for a single synthetic data set
#' 
#' @param postsynth A data frame with the synthetic data
#' @param data A postsynth object or data frame with confidential data
#' @param formula A formula for the predictive model
#' @param cp A hyperparameter for rpart
#' @param model Model type - "decision tree" or "random forest"
#' 
#' @return List with AUC and var importances
#' 
#' @importFrom stats predict
#' @importFrom rlang .data
#' 
#' @export
discriminator_auc <- function(
    postsynth, 
    data, 
    formula = id ~ ., 
    cp = 0.01, 
    model = "decision tree"
) {
  
  if ("postsynth" %in% class(postsynth)) {
    
    synthetic_data <- postsynth$synthetic_data
    
  } else {
    
    synthetic_data <- postsynth
    
  }
  
  ## combine original and synthetic data and add group indicator
  comb_data <- dplyr::bind_rows(
    `original` = data,
    `synthetic` = dplyr::select(synthetic_data, colnames(data)),
    .id = "id"
  ) %>%
    dplyr::mutate(id = factor(.data$id))
  
  disc_recipe <- comb_data |>
    recipes::recipe(formula = formula)
  
  if (model == "decision tree") {
    
    disc_mod <- 
      parsnip::decision_tree() |>
      parsnip::set_engine(engine = "rpart") |>
      parsnip::set_mode(mode = "classification")
  
  } else if (model == "random forest") {
    
    disc_mod <- 
      parsnip::rand_forest() |>
      parsnip::set_engine(engine = "ranger", importance = "impurity") |>
      parsnip::set_mode(mode = "classification")

  }
  
  disc_wf <- 
    workflows::workflow() |>
    workflows::add_recipe(disc_recipe) |>
    workflows::add_model(disc_mod)
  
  # fit model
  disc_fit <- disc_wf |>
    parsnip::fit(data = comb_data)
  
  # add predictions to original data
  comb_data <- dplyr::bind_cols(
    comb_data,
    `original` = predict(disc_fit, new_data = comb_data, type = "prob")$.pred_original
  )
  
  # calculate the auc
  auc <- yardstick::roc_auc_vec(truth = comb_data$id, estimate = comb_data$original)
  
  # calculate the variable importance
  # ignore root models
  if (auc == 0.5) {
  
    var_importance <- NULL
    
  } else {
    
    var_importance <- disc_fit |>
      workflows::extract_fit_parsnip() |> 
      vip::vip(num_features = 20)
    
  }
  
  return(
    list(
      auc = auc,
      var_importance = var_importance
    )
  )
  
}


