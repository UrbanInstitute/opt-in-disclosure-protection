#' Run utility and disclosure metrics/tests
#' 
#' @param postsynth A data frame with the synthetic data
#' @param data A data frame with the confidential data
#' @param holdout_data Holdout set of confidential data
#' 
#' @return Utility and disclosure results

summarize_results <- function(postsynth, data, holdout) {
  
  # univariate utility
  proportions <- proportions(postsynth = postsynth, data = data)
  
  proportions_race <- proportions(postsynth = postsynth, data = gsds, group_var = race)
  
  combine_race <- function(data) {
    
    data |>
      mutate(
        race_eth = case_when(
          hispan != "Not Hispanic" ~ "Hispanic",
          race == "Black/African American" ~ "Black",
          race == "White" ~ "White",
          TRUE ~ "Other Races and Ethnicities"
        )
      )
    
  }
  
  proportions_race_eth <- proportions(
    postsynth = combine_race(postsynth$synthetic_data), 
    data = combine_race(data = gsds), 
    group_var = race_eth
  )
  
  moments <- moments(postsynth =  postsynth, data = data)
  
  moments_race <- moments(postsynth =  postsynth, data = data, group_var = race)
  
  moments_race_eth <- moments(
    postsynth = combine_race(postsynth$synthetic_data), 
    data = combine_race(data = gsds), 
    group_var = race_eth
  )
  
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
  
  discriminator <- discriminator_auc(postsynth = postsynth, data = data)
  
  # specific utility
  prep_model_data <- function(data) {
    
    lm_data <- data |>
      dplyr::mutate(
        years_of_educ = dplyr::case_match(
          educd,
          3 ~ 0,
          2 ~ 0,
          5 ~ 1,
          6 ~ 1,
          8 ~ 2,
          9 ~ 3,
          10 ~ 4,
          11 ~ 5,
          14 ~ 6,
          15 ~ 7,
          17 ~ 8, # grade 5
          18 ~ 9,
          19 ~ 10,
          20 ~ 11,
          21 ~ 12,
          23 ~ 13,
          25 ~ 13,
          26 ~ 13,
          27 ~ 13,
          29 ~ 14,
          31 ~ 15,
          36 ~ 17,
          41 ~ 19,
          42 ~ 19,
          43 ~ 21
        )
      ) |>
      dplyr::mutate(
        white = race == "White",
        potential_experience = pmax(age - years_of_educ - 6, 0),
        potential_experience_sq = potential_experience ^ 2
      ) |>
      dplyr::filter(incwage > 0)
    
  }
  
  # regression metrics
  regression_ci_overlap <- regression_ci_overlap(
    postsynth = prep_model_data(postsynth$synthetic_data) |>
      mutate(sex = factor(sex, levels = c("Male", "Female"))), 
    data = prep_model_data(data), 
    formula = log(incwage) ~ years_of_educ + potential_experience + potential_experience_sq + white + sex
  )

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
  
  # membership inference test
  synth_factors <- postsynth$synthetic_data |>
    filter(opt_in) |> 
    select(-opt_in, -prob_opt_in) |>
    mutate(
      sex = factor(sex, c("Male", "Female")),
      hispan = factor(hispan, levels = c("Not Hispanic", "Mexican", "Puerto Rican", "Cuban", "Other")),
      marst = factor(marst, levels = c("Married, spouse present", "Married, spouse absent", "Separated", "Divorced", "Widowed", "Never married/single" )),
      statefip = factor(statefip, levels = c("Florida", "Michigan", "Pennsylvania")),
      empstat = factor(empstat, levels = c("Employed", "Unemployed", "Not in labor force"))
    ) |>
    mutate(across(where(is.character), factor)) |>
    dplyr::slice_sample(n = nrow(holdout))
  
  training_data <- data[-postsynth$opt_out_index, ] |>
    dplyr::slice_sample(n = nrow(holdout))
  
  membership_inference_test <- membership_inference_test(
    postsynth = synth_factors, 
    data = training_data, 
    holdout_data = holdout
  )

  list(
    proportions = proportions,
    proportions_race = proportions_race,
    proportions_race_eth = proportions_race_eth,
    moments = moments,
    moments_race = moments_race,
    moments_race_eth = moments_race_eth,
    percentiles = percentiles,
    cor_fit = cor_fit,
    k1_marginals = k1_marginals,
    k2_marginals = k2_marginals,
    k3_marginals = k3_marginals,
    discriminator_auc = discriminator$auc,
    discriminator_vip = discriminator$var_importance,
    regression_ci_overlap = regression_ci_overlap,
    ldiveristy = ldiveristy,
    incwelfr_rmse = incwelfr_rmse,
    membership_inference_test = membership_inference_test
  )
  
}
