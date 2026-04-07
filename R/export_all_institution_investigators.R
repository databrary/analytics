export_all_institution_investigators <- function(in_file_path = "private/inst_list",
                                                 out_file_path = "private/inst_affils",
                                                 vb = FALSE) {
  assertthat::assert_that(length(in_file_path) == 1)
  assertthat::is.string(in_file_path)
  assertthat::is.writeable(in_file_path)
  
  assertthat::assert_that(length(out_file_path) == 1)
  assertthat::is.string(out_file_path)
  assertthat::is.writeable(out_file_path)
  
  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))
  
  # Select latest institution list
  fns <- list.files(in_file_path, pattern = "institutions-", full.names = TRUE)
  fn <- fns[length(fns)]
  assertthat::is.readable(fn)
  institutions <- readr::read_csv(fn, show_col_types = FALSE)
  
  # Export separate affiliate files for institutions
  # Databrary has ID 1
  inst_ids <- institutions$institution_id[institutions$institution_id > 1]
  if (vb) {
    message("n=", length(inst_ids), " to export to '", out_file_path, "'.")
  }
  # Overwrites existing files.
  purrr::map(inst_ids,
             export_institution_affiliates,
             file_path = out_file_path,
             vb = vb) |>
    purrr::quietly()
}