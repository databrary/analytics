refresh_volume_funders_df <- function(vol_ids = 1:1520,
                                      vb = FALSE,
                                      rq = NULL) {
  assertthat::is.number(vol_ids)
  assertthat::assert_that(sum(vol_ids > 0) == length(vol_ids),
                          msg = "Not all volume indices are positive numbers")
  if (vb)
    message(
      "Refreshing funders data for volumes ",
      min(vol_ids),
      ":",
      max(vol_ids),
      ". Please be patient."
    )
  
  purrr::map_df(
    .x = vol_ids,
    .f = databraryr::list_volume_funding,
    rq = rq,
    vb = vb,
    .progress = "Vol funders:"
  )
}

#-------------------------------------------------------------------------------
update_volume_funders_csv <-
  function(df,
           csv_dir = "csv",
           csv_fn = "funders.csv",
           vb = FALSE) {
    stopifnot(!rlang::is_empty(df))
    stopifnot(dir.exists(csv_dir))
    if (!file.exists(file.path(csv_dir, csv_fn))) {
      if (vb) {
        warning("File does not exist: ", file.path(csv_dir, csv_fn))
        warning("Creating new file: ", file.path(csv_dir, csv_fn))
      }
    }
    readr::write_csv(df, file.path(csv_dir, csv_fn))
    csv_fn
  }
