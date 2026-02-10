list_user_owned_volumes_sharing_info <- function(user_id = 7630, vb = FALSE) {
  assertthat::is.number(vol_id)
  assertthat::assert_that(vol_id > 0)
  assertthat::assert_that(length(vol_id) == 1)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  user_vols <- list_owned_vols(user_id, vb)
  n_private <- sum(user_vols$vol_sharing_level == "private")
  n_overview <- sum(user_vols$vol_sharing_level == "public_overview_only")
  n_public = sum(user_vols$vol_sharing_level == "public")
  vol_stats <- tibble(
    user_id = user_id,
    n_vols = dim(user_vols)[1],
    n_public = n_public,
    n_overview = n_overview,
    n_private = n_private
  )
  vol_stats
}