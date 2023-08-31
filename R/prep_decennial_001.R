
#' Prep starting data 001
#' States: DC and IA
#' Attributes: age bucket, sex, race simple
#'
#'
#' @return Dataframe starting data 001
#'
prep_decennial_001 <- function() {
  
  source(here::here("R", "process_pums.R"))
  
  state_list <- c("DC", "IA")

  df_comb <- purrr::map_dfr(state_list, process_pums)

  keepvars <- c("serialno")
  geovars <- c("state")
  attribs <- c("age_bucket", "sex_val", "race_simple")
  
  starting <- df_comb |>
    dplyr::mutate(
      race_simple = case_when(
        raceshort_val == "White alone" ~ "White",
        raceshort_val == "Black or African American alone" ~ "Black",
        hispan_val != "Not Hispanic or Latino" ~ "Hispanic",
        .default = "Other"),
      age_bucket = case_when(
        between(age, 0, 17) == TRUE ~ "Child",
        between(age, 18, 64) == TRUE ~ "Adult",
        age >= 65 ~ "Senior")
      ) |>
    dplyr::select(all_of(c(keepvars, geovars, attribs)))
  
  return(starting)
  
}