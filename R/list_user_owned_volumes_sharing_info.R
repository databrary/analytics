list_user_owned_volumes_sharing_info <- function(user_id = 7630, vb = FALSE) {
  assertthat::assert_that(length(user_id) == 1)
  assertthat::is.number(user_id)
  assertthat::assert_that(user_id > 0)

  assertthat::assert_that(length(vb) == 1)
  assertthat::assert_that(is.logical(vb))

    # User info
  user_info <- databraryr::get_user_by_id(user_id, vb)
  if (is.null(user_info)) {
    if (vb) message("No Databrary data for user_id ", user_id)
    return(NULL)
  }
  
  # User vols
  user_vols <- list_volumes_user_owns(user_id, vb)
  if (is.null(user_vols)) {
    if (vb) message("User ", user_id, " has no volumes.")
  }
  
  vol_stats <- NULL
  if (is.null(user_vols)) {
    return(NULL)
  } else {
    n_private <- sum(user_vols$vol_sharing_level == "private")
    n_overview <- sum(user_vols$vol_sharing_level == "public_overview_only")
    n_public = sum(user_vols$vol_sharing_level == "public")
    
    vol_stats <- tibble::tibble(
      user_id = user_id,
      first_name = user_info$prename,
      last_name = user_info$sortname,
      n_vols = dim(user_vols)[1],
      n_public = n_public,
      n_overview = n_overview,
      n_private = n_private
    )    
  }
  
  vol_stats
}