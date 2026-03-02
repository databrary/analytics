user_is_vol_owner <- function(user_id = 7630, vol_id = 1746, vb = FALSE) {
  assertthat::is.number(user_id)
  assertthat::assert_that(user_id > 0)
  assertthat::assert_that(length(user_id) == 1)
  
  assertthat::is.number(vol_id)
  assertthat::assert_that(vol_id > 0)
  assertthat::assert_that(length(vol_id) == 1)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  if (vb) message("Retrieving info for vol_id ", vol_id, ".")
  vol <- databraryr::get_volume_by_id(vol_id, vb = vb)
  
  if (vb) message("Retrieving owner info for vol_id ", vol_id, ".")
  vol_owner <- list_volume_owner_info(vol_id, vb = vb)
  
  vol_owner$owner_id == user_id
}