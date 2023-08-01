membership_inference_test <- function(postsynth, data, holdout_data) {
  
  if ("postsynth" %in% class(postsynth)) {
    
    synthetic_data <- postsynth$synthetic_data
    
  } else {
    
    synthetic_data <- postsynth
    
  }
  
  # calculate threshold percentile for when the data are imblanced
  threshold_percentile <- nrow(data) / (nrow(data) + nrow(holdout_data))
  
  # combine records from the training data and holdout data
  blended_data <- dplyr::bind_rows(
    training = data,
    holdout = holdout_data,
    .id = "source"
  ) |>
    dplyr::mutate(source = factor(source))
  
  # for each record in the blended data, calculate the distance to the closest 
  # record in the synthetic data
  distances <- gower::gower_topn(
    dplyr::select(blended_data, -source), 
    synthetic_data,
    n = 1
  )
  
  # convert distances into predictions for if the record from the blended data
  # was used to train the synthetic data
  threshold <- quantile(distances$distance, probs = threshold_percentile)
  
  prediction <- ifelse(distances$distance[1, ] < threshold, "training", "holdout")
  
  pseudo_probabilities <- distances$distance[1, ] / max(distances$distance[1, ])
  
  blended_data <- bind_cols(
    blended_data,
    prediction = prediction,
    pseudo_probability = pseudo_probabilities
  ) |>
    dplyr::mutate(prediction = factor(prediction))
  
  # calculate metrics
  list(
    precision = yardstick::precision(blended_data, truth = source, estimate = prediction),
    recall = yardstick::recall(blended_data, truth = source, estimate = prediction),
    auc = yardstick::roc_auc_vec(truth = blended_data$source, estimate = blended_data$pseudo_probability),
    conf_mat = yardstick::conf_mat(blended_data, truth = source, estimate = prediction)
  )
  
}