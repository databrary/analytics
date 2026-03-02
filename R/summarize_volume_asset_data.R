summarize_volume_asset_data <- function(vol_id = 2, 
                                        bytes_2_gb = 1e9,
                                        vb = FALSE) {
  assertthat::is.number(vol_id)
  assertthat::assert_that(vol_id > 0)
  assertthat::assert_that(length(vol_id) == 1)
  
  assertthat::is.number(bytes_2_gb)
  assertthat::assert_that(bytes_2_gb > 0)
  assertthat::assert_that(length(bytes_2_gb) == 1)  
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  assets <- list_volume_assets(vol_id, vb)
  if (is.null(assets)) {
    return(NULL)
  }
  
  tibble::tibble(vol_id = vol_id,
                 n_sessions = length(unique(assets$session_id)),
                 n_assets = length(unique(assets$asset_id)),
                 tot_size_gb = sum(assets$asset_size)/(bytes_2_gb))
  
}