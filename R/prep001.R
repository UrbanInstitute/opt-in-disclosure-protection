
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
  
  keepvars <- c("SERIALNO", "STATE", "STATE_VAL", "PUMA", 
                "SUBSAMPL", "HWEIGHT", "PERSONS", "UNITTYPE", "HSUBFLG", 
                "PNUM", "PSUB", "PWEIGHT")
  
  attributes <- c("AGEBUCKET", "SEX_VAL", "RACESIMPLE")
  
  df <- df_comb |>
    dplyr::mutate(
      RACESIMPLE = case_when(
        RACESHORT_VAL == "White alone" ~ "WHITE",
        RACESHORT_VAL == "Black or African American alone" ~ "BLACK",
        HISPAN_VAL != "Not Hispanic or Latino" ~ "HISPANIC",
        .default = "OTHER"),
      AGEBUCKET = case_when(
        between(AGE, 0, 17) == TRUE ~ "CHILD",
        between(AGE, 18, 64) == TRUE ~ "ADULT",
        AGE >= 65 ~ "SENIOR")
      ) |>
    select(c(attributes, keepvars))
  
  lookup <- df |>
    dplyr::select(attributes) |>
    dplyr::distinct()
  
  prep001 <- list(df = df, lookup = lookup)
  
  return(prep001)
  
}