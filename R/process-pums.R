
#' Import and process Census Stateside PUMS files
#'
#' @param state_abb A US state abbreviation
#' @param subsample Optional list of subsample numbers (see Stateside PUMS technical documentation)
#'
#' @return A dataframe of PUMS observations for selected state and subsample, if provided
#'
process_pums <- function(state_abb, 
                         subsample = NULL){
  
  # validate state abbreviation and create url string
  state.abb.adj <- append(state.abb, "DC")
  state.name.adj <- append(state.name, "District of Columbia")
  
  stopifnot((toupper(state_abb) %in% state.abb.adj))
  
  fwf <- sprintf("https://www2.census.gov/census_2010/12-Stateside_PUMS/%s/%s.2010.pums.01.txt", 
                 gsub(" ", "_", state.name.adj[grep(toupper(state_abb), state.abb.adj)]),
                 tolower(state_abb))
  
  # prep helper layout files, read PUMS fwf
  load(here::here("data", "layout", "fwf-layout.rda"))
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
    dplyr::select(-any_of(c("PADDING", "RECTYPE"))) |>
    dplyr::mutate(across(everything(), as.character))
  
  person <- readr::read_fwf(fwf, 
                            readr::fwf_widths(widths = person_layout_vars$LEN, 
                                              col_names = person_layout_vars$VARIABLE), 
                            show_col_types = FALSE) |>
    dplyr::filter(RECTYPE == "P") |>
    dplyr::select(-any_of(c("PADDING", "RECTYPE"))) |>
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
  load(here::here("data", "layout", "fwf-layout-list.rda"))
  merge_list <- append(layout_list, list(df), after = 0)
  df_full <- Reduce(left_join, merge_list)
  
  return(df_full)
  
}
