synthesize_acs <- function(data, id, description) {
  
  data_opt_in <- data |>
    dplyr::filter(opt_in)
  
  data_opt_out <- data |>
    dplyr::filter(!opt_in) 
  
  opt_out_index <- which(!data$opt_in)
  
  # create "starting data"
  starting_data <- data_opt_in |> 
    dplyr::select(opt_in, prob_opt_in, race)
  
  synthesis_order <- c(
    "hispan", 
    "statefip",
    "marst",
    "sex",
    "hcovany",
    "empstat",
    "labforce",
    "age",
    "educd",
    "ftotinc",
    "inctot",
    "incwage",
    "incwelfr",
    "incresid"
  )
  
  # visit_sequence
  visit_sequence <- visit_sequence(
    conf_data = synthesis_order,
    start_data = starting_data,
    type = "manual"
  )
  
  # roadmap
  roadmap <- roadmap(
    conf_data = data_opt_in,
    start_data = starting_data,
    visit_sequence = visit_sequence
  )
  
  # synth_spec
  rpart_mod_cat <- parsnip::decision_tree() %>% 
    parsnip::set_mode("classification") %>%
    parsnip::set_engine("rpart")
  
  rpart_mod_num <- parsnip::decision_tree() %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("rpart")
  
  algorithms <- construct_algos(
    roadmap = roadmap,
    default_algo = rpart_mod_cat,
    custom_algos = list(
      list(
        vars = c("age", "educd", "ftotinc", "inctot", "incwage", "incwelfr", "incresid"),
        algorithm = rpart_mod_num
      )
    )
  )
  
  synth_spec <- synth_spec(
    roadmap = roadmap,
    synth_algorithms = algorithms,
    recipes = construct_recipes(roadmap = roadmap),
    predict_methods = sample_rpart
  )
  
  # noise
  # don't add noise to predictions
  noise <- noise(
    roadmap = roadmap,
    add_noise = FALSE,
    exclusions = 0
  )
  
  # constraints
  # don't impose constraints
  constraints <- constraints(
    roadmap = roadmap,
    constraints = NULL,
    max_z = 0
  )
  
  # replicates
  replicates <- replicates(
    replicates = 1,
    workers = 1,
    summary_function = NULL
  )
  
  presynth <- presynth(
    roadmap = roadmap,
    synth_spec = synth_spec,
    noise = noise, 
    constraints = constraints,
    replicates = replicates
  )
  
  synth <- synthesize(presynth)
  
  synth$synthetic_data <- dplyr::bind_rows(synth$synthetic_data, data_opt_out)
  
  synth$description <- description
  
  synth$opt_out_index <- opt_out_index
  
  saveRDS(synth, here("data", "results", "acs", paste0("synth-acs_", id, ".rds")))

  return(synth)
  
}