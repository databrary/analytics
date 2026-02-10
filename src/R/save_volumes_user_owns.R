save_volumes_user_owns <- function(user_id = 7630, vb = FALSE, 
                                   save_path = tempdir()) {
  assertthat::is.number(user_id)
  assertthat::assert_that(user_id > 0)
  assertthat::assert_that(length(user_id) == 1)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  vols <- list_volumes_user_owns(user_id, vb)
  
  if (vb) message("Saving data to file: ")
  now_str <- lubridate::as_datetime(now()) |> 
    stringr::str_replace_all(pattern = "[: \\.]", "-")
  fn <- file.path(save_path, paste0("user-", user_id, "-", now_str, ".csv"))
  if (vb) message("Saving data to file: ", fn)
  readr::write_csv(vols, fn)
}