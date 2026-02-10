list_volumes_user_owns <- function(user_id = 7630, vb = FALSE) {
  assertthat::is.number(user_id)
  assertthat::assert_that(user_id > 0)
  assertthat::assert_that(length(user_id) == 1)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  user_vols <- databraryr::list_user_volumes(user_id = user_id, vb = vb)
  if (is.null(user_vols)) {
    if (vb)
      message("No volumes found for user_id ", user_id, ".")
    return(NULL)
  }
  
  vol_ids <- user_vols$vol_id
  assertthat::is.number(vol_ids)
  
  if (vb)
    message("Retrieving owners for volumes accessible to user_id ",
            user_id,
            ".")
  vol_owners <- purrr::map(vol_ids,
                           list_volume_owner_info,
                           vb = vb,
                           .progress = "Volume owners...") |>
    purrr::list_rbind()
  
  if (is.null(vol_owners)) {
    if (vb)
      message("No volumes owned by user_id ", user_id, ".")
    return(NULL)
  }
  
  owned_vol_ids <- vol_ids[vol_owners$owner_id == user_id]
  if (vb)
    message("There are n = ",
            length(owned_vol_ids),
            " volumes owned by user_id ",
            user_id,
            ".")
  
  user_vols |>
    filter(vol_id %in% owned_vol_ids)
}