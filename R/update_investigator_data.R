update_investigator_data <- function(refresh_investigator_data = FALSE,
                                     save_path = "private",
                                     vb = FALSE) {
  assertthat::is.string(save_path)
  assertthat::is.writeable(save_path)
  
  now_str <- lubridate::as_datetime(lubridate::now()) |>
    stringr::str_replace_all(pattern = "[: \\.]", "-")
  
  fn <- file.path(save_path, paste0("investigators-", now_str, ".csv"))
  
  if (refresh_investigator_data) {
    if (vb) message("...refreshing investigator data")
    investigators <- databraryr::list_users(
      include_suspended = FALSE,
      is_authorized_investigator = TRUE,
      exclude_self = FALSE
    )
    if (vb) message("...saving data to ", fn)
    readr::write_csv(investigators, fn)    
  }
}
