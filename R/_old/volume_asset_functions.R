# R/volume_asset_functions.R

library(tidyverse)

FIGSHARE_TB <- 3500 #https://knowledge.figshare.com/plus
DRYAD_TB <- 5000 #https://datadryad.org/stash/faq

#-------------------------------------------------------------------------------
get_assets_in_vol <- function(vol_id, vb = FALSE) {
  stopifnot(is.numeric(vol_id))
  stopifnot(vol_id > 0)
  stopifnot(is.logical(vb))
  
  if (vb)
    message(paste0(" Extracting assets from volume ", vol_id))
  vol_data <- databraryr::list_assets_in_volume(vol_id)
  
  if (is.null(vol_data)) {
    if (vb)
      message(" No available assets.")
    NULL
  } else {
    # some volumes have no assets with duration or size attribute
    if (!('duration' %in% names(vol_data))) {
      vol_data <- dplyr::mutate(vol_data, duration = NA)
    }
    if (!('size' %in% names(vol_data))) {
      vol_data <- dplyr::mutate(vol_data, size = NA)
    }
    vol_data <- vol_data %>%
      dplyr::mutate(vol_id = vol_id) %>%
      dplyr::select(vol_id, size, duration, mimetype, extension, asset_type)
    vol_data
  }
}






storage_gb_by_vol <-
  function(df,
           deposit_fee_figshare_gb = FIGSHARE_TB/1000,
           deposit_fee_dryad_gb = DRYAD_TB/1000) {
    df_new <- df |>
      dplyr::group_by(vol_id) |>
      dplyr::summarise(n_tot_files = sum(n_files),
                       tot_gb = sum(tot_size_gb)) |>
      dplyr::mutate(
        figshare_fee = tot_gb * deposit_fee_figshare_gb,
        dryad_fee = tot_gb * deposit_fee_dryad_gb
      )
    df_new
  }