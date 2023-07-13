
#' Prep starting data 002
#' States: DC and IA
#' Attributes: hispanic simple
#'
#'
#' @return Dataframe starting data 002
#'
prep002 <- function() {
  
  source(here::here("R", "process_pums.R"))
  
  state_list <- c("DC", "IA")
  
  df_comb <- purrr::map_dfr(state_list, process_pums)
  
  keepvars <- c("serialno")
  geovars <- c("state")
  attribs <- c("hisp_simple")
  
  starting <- df_comb |>
    dplyr::mutate(
      hisp_simple = dplyr::if_else(
        hispan_val == "Not Hispanic or Latino", 
        "Not Hispanic", 
        "Hispanic")) |>
    dplyr::select(all_of(c(keepvars, geovars, attribs)))
  
  return(starting)
  
}
