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

