export_all_institutions_sharing_data <- function(input_path = "private/inst_affils",
                                                 save_path = "private/inst_invest_shared",
                                                 vb = FALSE) {
  assertthat::assert_that(length(input_path) == 1)
  assertthat::is.string(input_path)
  assertthat::is.writeable(input_path)
  
  assertthat::assert_that(length(save_path) == 1)
  assertthat::is.string(save_path)
  assertthat::is.writeable(save_path)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  f_path <- file.path("private", "inst_list")
  fl <- list.files(file.path(f_path),
                   pattern = "*.csv$",
                   full.names = TRUE)
  if (is.null(fl)) {
    if (vb)
      message("'", fl, "' not found. Exiting.")
    return(NULL)
  }
  fl <- fl[length(fl)]
  db_inst <- readr::read_csv(fl, show_col_types = FALSE)
  inst_ids <- db_inst$institution_id |>
    sort()
  inst_ids <- inst_ids[inst_ids > 1]
  if (vb)
    message("Found n=",
            length(inst_ids),
            " institutions. Gathering sharing data for each.\n")
  
  purrr::walk(
    inst_ids,
    export_inst_user_sharing_data,
    input_path = input_path,
    save_path = save_path,
    vb = vb
  ) |>
    purrr::quietly()
}