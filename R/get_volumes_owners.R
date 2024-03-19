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
    rq = rq,
    .progress = "Vol owners:"
  )
}
