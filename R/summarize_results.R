summarize_results <- function(postsynth, data) {
  
  # univariate utility
  proportions <- proportions(postsynth = postsynth, data = data)
  
  proportions_race <- proportions(postsynth = postsynth, data = gsds, group_var = race)
  
  moments <- moments(postsynth =  postsynth, data = data)
  
  moments_race <- moments(postsynth =  postsynth, data = data, group_var = race)
  
  percentiles <- percentiles(
    postsynth = dplyr::select(postsynth$synthetic_data, -prob_opt_in),
    data = data, 
    probs = seq(0, 1, 0.1)
  )
  
  # bivariate utility
  cor_fit <- cor_fit(postsynth = dplyr::select(postsynth$synthetic_data, -prob_opt_in), data = data)
  
  # multivariate utility
  k1_marginals <- kmarginals(postsynth = postsynth, data = data, k = 1)
  k2_marginals <- kmarginals(postsynth = postsynth, data = data, k = 2)
  k3_marginals <- kmarginals(postsynth = postsynth, data = data, k = 3) 
  
  
  
  # descriminator_auc <- descriminator_auc(
  #   postsynth = postsynth, 
  #   data = data
  # )
  
  # disclosure metrics
  ldiveristy <- postsynth$ldiversity |>
    dplyr::select(dplyr::where(is.numeric)) |>
    dplyr::summarize(dplyr::across(everything(), ~mean(.x < 10))) |>
    rename_with(.fn = ~paste0("ldiveristy_", .x))
  
  incwelfr_rmse <- prediction_test(
    postsynth = dplyr::select(postsynth$synthetic_data, -prob_opt_in, -opt_in), 
    data = data, 
    formula = incwelfr ~ .
  )
  
  list(
    proportions = proportions,
    proportions_race = proportions_race,
    moments = moments,
    moments_race = moments_race,
    percentiles = percentiles,
    cor_fit = cor_fit,
    k1_marginals = k1_marginals,
    k2_marginals = k2_marginals,
    k3_marginals = k3_marginals,
    # descriminator_auc = descriminator_auc
    ldiveristy = ldiveristy,
    incwelfr_rmse = incwelfr_rmse
  )
  
}
