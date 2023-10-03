#' Regression confidence interval overlap
#' 
#' @param postsynth A postsynth object or data frame with the synthetic data
#' @param data A data frame with confidential data
#' @param formula Regression specification
#' 
#' @return Regression CI overlap and coefficients as a list

regression_ci_overlap <- function(postsynth, data, formula) {
  
  if ("postsynth" %in% class(postsynth)) {
    
    synthetic_data <- postsynth$synthetic_data
    
  } else {
    
    synthetic_data <- postsynth
    
  }
  
  # original model ------------------------------------------------------
  lm_original <- lm(formula = formula, data = data)
  
  # synthetic model ---------------------------------------------------------
  lm_synth <- lm(formula = formula, data = synthetic_data)
  
  coefficients <- dplyr::bind_rows(
    `original` = broom::tidy(lm_original, conf.int = TRUE),
    synthetic = broom::tidy(lm_synth, conf.int = TRUE),
    .id = "source"
  )
  
  diff_table <- dplyr::full_join(
    broom::tidy(lm_original, conf.int = TRUE),
    broom::tidy(lm_synth, conf.int = TRUE),
    by = "term",
    suffix = c("_original", "_synthetic")
  )
  
  ci_overlap <- diff_table |>
    # calculate max bounds for formula
    dplyr::mutate(
      overlap_lower = pmax(conf.low_original, conf.low_synthetic),
      overlap_upper = pmin(conf.high_original, conf.high_synthetic)
    ) |>
    # calculate confidence interval overlap
    dplyr::mutate(
      overlap = 0.5 * (((overlap_upper - overlap_lower) / (conf.high_original - conf.low_original)) +
                         ((overlap_upper - overlap_lower) / (conf.high_synthetic - conf.low_synthetic)))
    ) |>
    # calculate other regression metrics
    dplyr::mutate(
      coef_diff = estimate_synthetic - estimate_original,
      std_coef_diff = (estimate_synthetic - estimate_original) / std.error_original,
      sign_match = (estimate_original < 0 & estimate_synthetic < 0) | 
        (estimate_original > 0 & estimate_synthetic > 0),
      inference_match = (p.value_original < 0.05 & p.value_synthetic < 0.05) | 
        (p.value_original > 0.05 & p.value_synthetic > 0.05)
    ) |>
    dplyr::select(term, overlap, coef_diff, std_coef_diff, sign_match, inference_match)
  
  
  list(
    ci_overlap = ci_overlap,
    coefficient = coefficients
  )
  
}