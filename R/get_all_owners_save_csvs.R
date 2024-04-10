get_all_owners_save_csvs <-
  function(max_vol_id = 1520,
           dir = "src/csv",
           vb = FALSE,
           rq = NULL) {
    stopifnot(is.numeric(max_vol_id))
    stopifnot(max_vol_id > 0)
    stopifnot(is.character(dir))
    stopifnot(dir.exists(dir))
    
    # TODO: Fix this hack
    get_save_volumes_owners(1, 500, vb = vb, rq = rq)
    get_save_volumes_owners(501, 1000, vb = vb, rq = rq)
    get_save_volumes_owners(1001, 1275, vb = vb, rq = rq) # skip 1276 & 1277 because no owners
    get_save_volumes_owners(1278, 1500, vb = vb, rq = rq)
    get_save_volumes_owners(1501, max_vol_id, vb = vb, rq = rq)
  }

#-------------------------------------------------------------------------------
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
    df <- get_volumes_owners(min_vol_id, max_vol_id, vb = vb, rq = rq)
    if (vb)
      message(paste0("Saving owner data for volumes ", min_vol_id, "-", max_vol_id))
    save_volumes_owners(df, min_vol_id, max_vol_id, dir, fn_suffix = "-owners.csv", vb)
  }

#-------------------------------------------------------------------------------
get_volumes_owners <- function(min_vol_id = 1,
                               max_vol_id = 10,
                               vb = FALSE,
                               rq = NULL) {
  stopifnot(is.numeric(min_vol_id))
  stopifnot(is.numeric(max_vol_id))
  stopifnot(min_vol_id > 0)
  stopifnot(max_vol_id > 0)
  stopifnot(min_vol_id < max_vol_id)
  
  vols_range <- min_vol_id:max_vol_id
  if (vb)
    message("Gathering owners from volumes ", min_vol_id, ":", max_vol_id)
  purrr::map_dfr(
    .x = vols_range,
    .f = databraryr::list_volume_owners,
    vb = vb,
    rq = rq,
    .progress = "Vol owners:"
  )
}
