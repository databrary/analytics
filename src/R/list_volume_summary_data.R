# Note: As of 2026-02-17, this function only returns valid data for volumes I own or
# which are "public".
# See list_user_volumes() for a function that returns volume-level data, but not
# session- or asset-level data.

list_volume_summary_data <- function(vol_id = 1, vb = FALSE) {
  assertthat::is.number(vol_id)
  assertthat::assert_that(vol_id > 0)
  assertthat::assert_that(length(vol_id) == 1)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  vol_assets <- databraryr::list_volume_assets(vol_id, vb)
  if (is.null(vol_assets)) {
    return(NULL)
  }
  
  n_sessions <- length(unique(vol_assets$session_id))
  n_assets <- length(unique(vol_assets$asset_id))
  n_assets_private <- sum(vol_assets$asset_permission == "private")
  n_assets_overview <- sum(vol_assets$asset_permission == "public_overview_only")
  n_assets_public = sum(vol_assets$asset_permission == "public")
  
  n_formats <- length(unique(vol_assets$asset_mime_type))
  total_bytes <- sum(vol_assets$asset_size)
  
  tibble::tibble(vol_id = vol_id,
                 n_sessions = n_sessions,
                 n_assets = n_assets,
                 n_assets_private = n_assets_private,
                 n_assets_overview = n_assets_overview,
                 n_assets_public = n_assets_public,
                 n_formats = n_formats,
                 total_bytes = total_bytes)
}