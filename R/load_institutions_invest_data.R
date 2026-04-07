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

load_inst_df <- function(inst_list_f_path = "private/inst_list") {
  assertthat::is.string(inst_list_f_path)
  
  fl <- list.files(inst_list_f_path, pattern = ".csv$", full.names = TRUE)
  latest_file <- fl[length(fl)]
  
  readr::read_csv(latest_file, show_col_types = FALSE) |>
    dplyr::rename(id = institution_id,
                  name = institution_name) |>
    dplyr::filter(id > 1)
}

make_inst_affil_df <- function() {
  inst_invest_df <- load_institutions_invest_data()
  inst_df <- load_inst_df()
  dplyr::left_join(inst_df, inst_invest_df, by = "id")
}

