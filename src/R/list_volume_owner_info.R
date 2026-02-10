list_volume_owner_info <- function(vol_id = 1746, vb = FALSE) {
  assertthat::is.number(vol_id)
  assertthat::assert_that(vol_id > 0)
  assertthat::assert_that(length(vol_id) == 1)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  vol <- databraryr::get_volume_by_id(vol_id, vb = vb) # This is faster than list_volume_info().
  if (is.null(vol)) {
    if (vb) message("No data found for vol_id ", vol_id, ".")
    return(NULL)
  }
  if (vb) message("Found data for vol_id ", vol_id, ".")
  owner <- vol$owner_connection[[1]][[3]]
  
  vol_owner <- NULL
  vol_owner <- tibble::tibble(vol_id = vol_id,
                      owner_id = owner$id,
                      owner_first_name = owner$first_name,
                      owner_last_name = owner$last_name,
                      owner_is_authorized_investigator = owner$is_authorized_investigator,
                      owner_has_avatar = owner$has_avatar,
                      owner_affiliation_id = owner$affiliation$id,
                      owner_affiliation_name = owner$affiliation_name
  )
  vol_owner
}