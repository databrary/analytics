generate_vol_data_sharing_rpts <- function(vb = FALSE, save_path = tempdir()) {
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  assertthat::is.string(save_path)
  assertthat::is.writeable(save_path)
  assertthat::assert_that(length(save_path) == 1)
  
  # Source functions
  fl <- list.files(file.path(here::here(), "src", "R"), "*.R", full.names = TRUE)
  purrr::map(fl, source)
  
  investigators <- databraryr::list_users(include_suspended = FALSE,
                                          is_authorized_investigator = TRUE)
  
  ai_ids <- investigators$user_id |>
    sort()
  
  # Save CSVs of volumes owned by authorized investigators
  purrr::map(
    ai_ids,
    save_volumes_sharing_status_info,
    vb = vb,
    save_path = save_path,
    .progress = "User reports"
  )
}