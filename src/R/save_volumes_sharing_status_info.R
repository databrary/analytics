save_volumes_sharing_status_info <- function(user_id = 7630, vb = FALSE, 
                                   save_path = tempdir()) {
  assertthat::is.number(user_id)
  assertthat::assert_that(user_id > 0)
  assertthat::assert_that(length(user_id) == 1)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  assertthat::is.string(save_path)
  assertthat::is.writeable(save_path)
  assertthat::assert_that(length(save_path) == 1)
  
  vols <- list_user_owned_volumes_sharing_info(user_id, vb)
  if (is.null(vols)) {
    if (vb) message("No volumes for user_id ", user_id, ".")
    return(NULL)
  }
  
  fn <- NULL
  if (vb) message("Saving data to file: ")
  now_str <- lubridate::as_datetime(lubridate::now()) |> 
    stringr::str_replace_all(pattern = "[: \\.]", "-")
  fn <- file.path(save_path, paste0("user-", stringr::str_pad(user_id,
                                                              width = 5,
                                                              side = "left",
                                                              pad = 0), "-", 
                                    now_str, ".csv"))
  if (vb) message("Saving data to file: ", fn)
  readr::write_csv(vols, fn)
  fn
}