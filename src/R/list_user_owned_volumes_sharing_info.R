list_user_owned_volumes_sharing_info <- function(user_id = 7630, vb = FALSE) {
  assertthat::is.number(user_id)
  assertthat::assert_that(user_id > 0)
  assertthat::assert_that(length(user_id) == 1)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  vol_stats <- NULL
  
  # User info
  user_info <- get_user_by_id(user_id, vb)
  if (is.null(user_info)) {
    return(NULL)
  }
  
  # User vols
  user_vols <- list_volumes_user_owns(user_id, vb)
  
  if (is.null(user_vols)) {
    return(NULL)
  } else {
    n_private <- sum(user_vols$vol_sharing_level == "private")
    n_overview <- sum(user_vols$vol_sharing_level == "public_overview_only")
    n_public = sum(user_vols$vol_sharing_level == "public")
    vol_stats <- tibble::tibble(
      user_id = user_id,
      user_first_name = user_info$prename,
      user_last_name = user_info$sortname,
      user_affiliation = user_info$affiliation,
      user_affiliation_id = user_info$affiliation_id,
      n_vols = dim(user_vols)[1],
      n_public = n_public,
      n_overview = n_overview,
      n_private = n_private
    )    
  }
  
  vol_stats
}