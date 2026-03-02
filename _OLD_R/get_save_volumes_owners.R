get_save_volumes_owners <-
  function(min_vol_id = 1,
           max_vol_id = 10,
           dir = "src/csv",
           vb = FALSE,
           rq = NULL) {
    stopifnot(is.numeric(min_vol_id))
    stopifnot(is.numeric(max_vol_id))
    stopifnot(min_vol_id > 0)
    stopifnot(max_vol_id > 0)
    stopifnot(min_vol_id < max_vol_id)
    stopifnot(is.character(dir))
    stopifnot(dir.exists(dir))
    
    if (vb)
      message(paste0("Getting owner data for volumes ", min_vol_id, ":", max_vol_id))
    df <- get_volumes_owners(min_vol_id, max_vol_id, vb, rq)
    if (vb)
      message(paste0("Saving owner data for volumes ", min_vol_id, "-", max_vol_id))
    save_volumes_owners(df, min_vol_id, max_vol_id, dir, fn_suffix = "-owners.csv", vb)
  }
