load_institutions_invest_data <- function(inst_f_path = "private/inst_affils") {
  assertthat::is.string(inst_f_path)
  
  fl <- list.files(inst_f_path, pattern = ".csv$", full.names = TRUE)
  
  inst_latest <- purrr::map(fl,
                            readr::read_csv,
                            show_col_types = FALSE,
                            .progress = TRUE) |>
    purrr::list_rbind()
  
  inst_latest
  inst_latest |>
    dplyr::rename(id = institution_id,
                  last_name = user_sortname,
                  first_name = user_prename) |>
    dplyr::filter(id > 1) |>
    dplyr::select(-c("role", "user_affiliation", "user_affiliation_id"))
}
