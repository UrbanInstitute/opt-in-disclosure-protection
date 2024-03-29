
#' Import and process Census Stateside PUMS files
#'
#' @param state_abb A US state abbreviation
#' @param subsample Optional list of subsample numbers (see Stateside PUMS technical documentation)
#'
#' @return A dataframe of PUMS observations for selected state and subsample, if provided
#'
process_pums <- function(state_abb, 
                         subsample = NULL) {
  
  # validate state abbreviation and create url string
  state.abb.adj <- append(state.abb, "DC")
  state.name.adj <- append(state.name, "District of Columbia")
  
  stopifnot((toupper(state_abb) %in% state.abb.adj))
  
  fwf <- paste0("https://www2.census.gov/census_2010/12-Stateside_PUMS/",
                 stringr::str_replace_all(
                   state.name.adj[stringr::str_detect(state.abb.adj, 
                                                      toupper(state_abb))], 
                   " ", 
                   "_"), 
                 "/",
                 tolower(state_abb),
                 ".2010.pums.01.txt")
  
  # prep helper layout files, read PUMS fwf
  layout <- readRDS(here::here("data", "layout", "fwf-layout.rds"))
  housing_layout_vars <- layout |>
    dplyr::filter(RT == "H") |>
    dplyr::select(LEN, VARIABLE) |>
    dplyr::distinct() |>
    tidyr::drop_na()
  
  person_layout_vars <- layout |>
    dplyr::filter(RT == "P") |>
    dplyr::select(LEN, VARIABLE) |>
    dplyr::distinct() |>
    tidyr::drop_na()
  
  housing <- readr::read_fwf(fwf, 
                             readr::fwf_widths(widths = housing_layout_vars$LEN,
                                               col_names = housing_layout_vars$VARIABLE), 
                             show_col_types = FALSE) |>
    dplyr::filter(RECTYPE == "H") |>
    dplyr::select(-RECTYPE) |>
    dplyr::mutate(across(everything(), as.character))
  
  person <- readr::read_fwf(fwf, 
                            readr::fwf_widths(widths = person_layout_vars$LEN, 
                                              col_names = person_layout_vars$VARIABLE), 
                            show_col_types = FALSE) |>
    dplyr::filter(RECTYPE == "P") |>
    dplyr::select(-c(PADDING, RECTYPE)) |>
    dplyr::mutate(across(everything(), as.character))
  
  # join housing and person, exclude unoccupied housing units
  df_join <- dplyr::full_join(housing, person, by = "SERIALNO") |>
    dplyr::mutate(AGE = as.numeric(AGE))
  
  df_join_occ <- df_join |>
    dplyr::filter(!(UNITTYPE == "0" & is.na(PNUM)))
  stopifnot(!is.na(df_join_occ$PNUM))
  
  # filter to selected subsample, if provided
  if (!is.null(subsample)){
    df <- df_join_occ |>
      dplyr::filter(SUBSAMPL %in% subsample)
  } else{
    df <- df_join_occ
  }
  
  # add dictionary values
  layout_list <- readRDS(here::here("data", "layout", "fwf-layout-list.rds"))
  merge_list <- append(layout_list, list(df), after = 0)
  df_full <- Reduce(dplyr::left_join, merge_list) |>
    dplyr::rename_with(tolower, everything())
  
  return(df_full)
  
}
