update_all_vol_stats <- function(max_volume_id,
                                 vols_per_pass = 50,
                                 save_file = TRUE,
                                 save_path = 'src/csv',
                                 vb = FALSE,
                                 rq = NULL) {
  stopifnot(is.numeric(max_volume_id))
  stopifnot(max_volume_id > 0)
  
  stopifnot(is.numeric(vols_per_pass))
  stopifnot(vols_per_pass > 0)
  
  stopifnot(is.logical(save_file))
  
  stopifnot(is.character(save_path))
  stopifnot(dir.exists(save_path))
  
  stopifnot(is.logical(vb))
  
  # Handle NULL rq
  if (is.null(rq)) {
    if (vb) {
      message("NULL request object. Will generate default.")
      message("Not logged in. Only public information will be returned.")
    }
    rq <- databraryr::make_default_request()
  }
  
  options(dplyr.summarise.inform = FALSE)
  
  # if (!db_login_status) stop("Not logged in to Databrary")
  
  #databraryr::login_db(db_login)
  
  # It may be unnecessary, but I do this in separate chunks
  # Some of the larger volumes have a lot of assets, and this
  # chunking gives the analyst some feedback about what's happening.
  # Also, if a problem arises, it's easier to debug.
  range_start_id <-
    seq(from = 1, to = max_volume_id, by = vols_per_pass)
  range_end_id <- range_start_id + vols_per_pass
  
  purrr::walk2(
    range_start_id,
    range_end_id,
    update_vol_asset_stats,
    save_file = save_file,
    save_path = save_path,
    vb = vb,
    rq = rq
  )
  
  list.files(path = "src/csv",
             pattern = '-assets\\.csv',
             full.names = TRUE)
}

#-------------------------------------------------------------------------------
update_vol_asset_stats <-
  function(start_vol_id,
           end_vol_id,
           save_file = TRUE,
           save_path = 'src/csv',
           vb = FALSE,
           rq = NULL) {
    stopifnot(is.numeric(start_vol_id))
    stopifnot(start_vol_id > 0)
    stopifnot(is.numeric(end_vol_id))
    stopifnot(end_vol_id > 0)
    stopifnot(end_vol_id > start_vol_id)
    stopifnot(is.logical(save_file))
    stopifnot(is.character(save_path))
    stopifnot(dir.exists(save_path))
    stopifnot(is.logical(vb))
    
    message(
      paste0(
        "Updating volume asset statistics for volumes ",
        start_vol_id,
        ":",
        end_vol_id,
        "."
      )
    )
    message("Please be patient.")
    purrr::walk(
      c(start_vol_id:end_vol_id),
      calculate_vol_asset_stats,
      save_file,
      save_path,
      vb = vb,
      rq = rq,
      .progress = "Vol assets:"
    )
  }

#-------------------------------------------------------------------------------
calculate_vol_asset_stats <- function(vol_id,
                                      save_file = FALSE,
                                      save_path = 'src/csv',
                                      vb = FALSE,
                                      rq = NULL) {
  stopifnot(is.numeric(vol_id))
  stopifnot(vol_id > 0)
  stopifnot(is.logical(save_file))
  stopifnot(is.character(save_path))
  stopifnot(dir.exists(save_path))
  stopifnot(is.logical(vb))
  
  options(dplyr.summarise.inform = FALSE)
  
  if (vb)
    message(paste0('Retrieving asset data for volume ', vol_id))
  vol_assets <- databraryr::list_volume_assets(vol_id, vb, rq = rq)
  if (is.null(vol_assets)) {
    if (vb)
      message(paste0("No shared data in volume ", vol_id, "."))
    NULL
  } else {
    vol_summary <- vol_assets %>%
      dplyr::mutate(., vol_id = vol_id) %>%
      dplyr::group_by(., vol_id, format_mimetype, format_extension, format_name) %>%
      dplyr::summarise(
        .,
        n_files = n(),
        tot_size_gb = bytes_to_gb(sum(asset_size, na.rm = TRUE)),
        tot_dur_hrs = ms_to_hrs(sum(asset_duration, na.rm = TRUE))
      )
    
    if (save_file) {
      out_fn <-
        file.path(save_path, paste0(stringr::str_pad(vol_id, 5, pad = "0"), '-assets.csv'))
      if (file.exists(out_fn)) {
        if (vb)
          message("File exists: ", out_fn, ". Overwritten.")
      }
      if (vb)
        message(paste0("Saving data to ", out_fn))
      
      readr::write_csv(vol_summary, file = out_fn)
      out_fn
    } else {
      vol_summary
    }
  }
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
