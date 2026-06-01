load_inst_df <- function(inst_list_f_path = "private/inst_list") {
  assertthat::is.string(inst_list_f_path)
  
  fl <- list.files(inst_list_f_path, pattern = ".csv$", full.names = TRUE)
  latest_file <- fl[length(fl)]
  
  readr::read_csv(latest_file, show_col_types = FALSE) |>
    dplyr::rename(id = institution_id,
                  name = institution_name) |>
    dplyr::filter(id > 1)
}