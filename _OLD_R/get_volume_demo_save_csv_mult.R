get_volume_demo_save_csv_mult <- function(min_vol_id = 1,
                                          max_vol_id = 10,
                                          csv_dir = 'src/csv',
                                          vb = FALSE,
                                          rq = NULL) {
  stopifnot(is.numeric(min_vol_id))
  stopifnot(min_vol_id > 0)
  stopifnot(is.numeric(max_vol_id))
  stopifnot(max_vol_id > 0)
  stopifnot(min_vol_id < max_vol_id)
  stopifnot(is.character(csv_dir))
  stopifnot(dir.exists(csv_dir))
  
  vols_range <- min_vol_id:max_vol_id
  
  if (vb)
    message(paste0(
      "Getting demographic data for volumes ",
      min_vol_id,
      ":",
      max_vol_id,
      "\n"
    ))
  
  purrr::walk(
    vols_range,
    get_volume_ss_save_csv,
    dir = csv_dir,
    vb = vb,
    rq = rq,
    .progress = "Vol ss:"
  )
}

#-------------------------------------------------------------------------------
get_volume_ss_save_csv <-
  function(vol_id = 4,
           dir = "src/csv",
           vb = FALSE,
           rq = NULL) {
    stopifnot(is.numeric(vol_id))
    stopifnot(vol_id > 0)
    
    df <-
      databraryr::download_session_csv(
        vol_id = vol_id,
        as_df = TRUE,
        vb = vb,
        rq = rq
      )
    if (is.data.frame(df)) {
      if (vb)
        message(paste0("Imported data frame."))
      fn <-
        paste0(stringr::str_pad(vol_id, 5, pad = "0"),
               "-sess-materials.csv")
      out_fn <- file.path(dir, fn)
      if (vb)
        message("Saving ", out_fn)
      readr::write_csv(df, out_fn)
    } else {
      if (vb)
        message("Failed to convert to data frame.")
      NULL
    }
  }
