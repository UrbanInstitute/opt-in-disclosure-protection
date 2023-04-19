
#' Prep starting data 001
#' States: DC and IA
#' Attributes: age bucket, sex, race simple
#'
#'
#' @return A list containing starting data 001 and accompanying lookup table
#'
prep001 <- function(){
  
  source(here("R", "process-pums.R"))
  
  state_list <- c("DC", "IA")

  pums_list <- map(state_list, process_pums)
  df_comb <- bind_rows(pums_list)
  
  keepvars <- c("serialno", "state", "state_val", "puma", 
                "subsampl", "hweight", "persons", "unittype", "hsubflg", 
                "pnum", "psub", "pweight")
  
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
    dplyr::select(all_of(c(attribs, keepvars)))
  
  lookup <- starting |>
    dplyr::select(all_of(attribs)) |>
    dplyr::distinct()
  
  prep001 <- list(starting = starting, lookup = lookup)
  
  return(prep001)
  
}