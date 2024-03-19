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



#-------------------------------------------------------------------------------
generate_volume_asset_csv_list <- function(csv_dir = "src/csv") {
  stopifnot(is.character(csv_dir))
  stopifnot(dir.exists(csv_dir))
  
  fl <- list.files(csv_dir, '-assets\\.csv', full.names = TRUE)
  if (length(fl) == 0) {
    warning("No volume asset files found in '", csv_dir, "'.")
    NULL
  } else {
    fl
  }
}

#-------------------------------------------------------------------------------
make_volume_assets_stats_df <-
  function(csv_fns = "src/csv", vb = FALSE) {
    stopifnot(is.character(csv_fns))
    if (vb)
      message("Making df from CSVs.")
    df <-
      purrr::map_df(csv_fns,
                    readr::read_csv,
                    show_col_types = FALSE,
                    .progress = "Asset CSVs:")
    dplyr::arrange(df, vol_id)
  }

#-------------------------------------------------------------------------------
bytes_to_gb <- function(b) {
  b / (1.024e9)
}

#-------------------------------------------------------------------------------
ms_to_secs <- function(ms) {
  if (!is.numeric(ms)) {
    stop('`ms` must be a number.')
  }
  ms / 1000
}

#-------------------------------------------------------------------------------
secs_to_mins <- function(s) {
  if (!is.numeric(s)) {
    stop('`s` must be a number.')
  }
  s / 60
}

#-------------------------------------------------------------------------------
mins_to_hrs <- function(m) {
  if (!is.numeric(m)) {
    stop('`m` must be a number.')
  }
  m / 60
}

#-------------------------------------------------------------------------------
ms_to_mins <- function(ms) {
  if (!is.numeric(ms)) {
    stop('`ms` must be a number.')
  }
  ms_to_secs(ms) %>% secs_to_mins(.)
}

#-------------------------------------------------------------------------------
ms_to_hrs <- function(ms) {
  if (!is.numeric(ms)) {
    stop('`ms` must be a number.')
  }
  ms_to_secs(ms) %>% secs_to_mins(.) %>% mins_to_hrs(.)
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