update_volume_data <- function(vol_id = 1,
                               csv_dir = "src/csv",
                               save_csvs = TRUE,
                               vb = FALSE,
                               rq = NULL) {
  
  assertthat::is.number(vol_id)
  assertthat::assert_that(vol_id > 0)
  assertthat::assert_that(length(vol_id) == 1)
  
  assertthat::is.string(csv_dir)
  assertthat::assert_that(length(csv_dir) == 1)
  assertthat::is.writeable(csv_dir)
  
  assertthat::assert_that(is.logical(save_csvs))
  assertthat::assert_that(length(save_csvs) == 1)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  assertthat::assert_that(is.list(rq) | is.null(rq))

  # Get volume blob
  if (vb) message("Retrieving data for volume ", vol_id)
  vol_data <- databraryr::get_volume_by_id(vol_id, vb = vb, rq = rq)
  if (is.null(vol_data)) {
    if (vb)
      message("No data for volume ", vol_id)
    return(NULL)
  }
  
  # funders
  if (is.null(vol_data$funding)) {
    if (vb) message("No funders data for volume ", vol_id)
  } else {
    if (vb) message("Saving funder info for volume ", vol_id)
    funder_fn <- paste0("vol-", stringr::str_pad(vol_id, 5, pad = "0"), "-funders.csv")
    funders_full_fn <- file.path(csv_dir, funder_fn)
    purrr::map(vol_data$funding, extract_funder_info) |>
      purrr::list_rbind() |>
      dplyr::mutate(vol_id = vol_id) |>
      readr::write_csv(file = funders_full_fn)    
  }

  # owners
  if (is.null(vol_data$owners)) {
    if (vb) message("No owners data for volume, ", vol_id)
  } else {
    vol_owners <- purrr::map(vol_data$owners, tibble::as_tibble) |>
      purrr::list_rbind()
    if (dim(vol_owners)[1] > 0) {
      if (vb) message("Saving owner info for volume ", vol_id)
      owner_fn <- paste0("vol-", stringr::str_pad(vol_id, 5, pad = "0"), "-owners.csv")
      owners_full_fn <- file.path(csv_dir, owner_fn)
      vol_owners |>
        dplyr::mutate(vol_id = vol_id) |>
        dplyr::rename(person_id = id, person_name = name) |>
        readr::write_csv(file = owners_full_fn)       
    } else {
      if (vb) message("No owners data for volume, ", vol_id)
    }
  }
  
  # session summary
  if (is.null(vol_data$containers)) {
    if (vb) message("No sessions data for volume, ", vol_id)
  } else {
    if (vb) message("Saving owner info for volume ", vol_id)
    sessions_fn <- paste0("vol-", stringr::str_pad(vol_id, 5, pad = "0"), "-sessions.csv")
    sessions_full_fn <- file.path(csv_dir, sessions_fn)
    purrr::map(vol_data$containers, get_info_from_session) |>
      purrr::list_rbind() |>
      dplyr::mutate(vol_id = vol_id) |>
      readr::write_csv(file = sessions_full_fn)
  }
}

#-------------------------------------------------------------------------------
extract_funder_info <- function(vol_funder_list_item) {
  assertthat::assert_that("list" %in% class(vol_funder_list_item))
  assertthat::assert_that("funder" %in% names(vol_funder_list_item))
  assertthat::assert_that("awards" %in% names(vol_funder_list_item))
  
  funder_id <- vol_funder_list_item$funder$id
  funder_name <- vol_funder_list_item$funder$name
  if (length(vol_funder_list_item$awards) == 0) {
    funder_award <- NA
  } else {
    funder_award <- vol_funder_list_item$awards |>
      unlist()
  }
  tibble::tibble(
    funder_id = funder_id,
    funder_name = funder_name,
    funder_award = funder_award
  )
}
