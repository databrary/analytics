get_many_volumes_data <-
  function(min_vol = 1,
           max_vol = 10,
           vb = FALSE,
           rq = NULL) {
    stopifnot(is.numeric(min_vol))
    stopifnot(min_vol > 0)
    stopifnot(is.numeric(max_vol))
    stopifnot(max_vol > 0)
    stopifnot(min_vol < max_vol)
    
    vol_index <- min_vol:max_vol
    vols_data <-
      purrr::map(
        vol_index,
        get_volume_data,
        vb = vb,
        rq = rq,
        .progress = "Vols data:"
      )
    
    # Merge data frames
    purrr::list_rbind(vols_data)
  }

#-------------------------------------------------------------------------------
get_volume_data <-
  function(vol_id = 1,
           skip_vols = c(1276, 1277),
           vb = FALSE,
           rq = NULL) {
    stopifnot(is.numeric(vol_id))
    stopifnot(vol_id > 0)
    
    if (vol_id %in% skip_vols)
      return(NULL)
    
    if (vb)
      message(paste0("Gathering data from volume ", vol_id))
    
    vol_data <-
      databraryr::get_volume_by_id(vol_id, vb = vb, rq = rq)
    
    if(is.null(vol_data)) {
      if (vb) message("No data in volume ", vol_id)
      return(NULL)
    }
    
    vol_owners <-
      purrr::map(vol_data$owners, tibble::as_tibble) |>
      purrr::list_rbind() |>
      dplyr::rename(owner_id = id, owner_name = name) |>
      dplyr::filter(!(stringr::str_detect(owner_name, "Databrary")))
    
    vol_sessions <-
      purrr::map(vol_data$containers, get_info_from_session) |>
      purrr::list_rbind()
    
    this_vol <- tibble::tibble(
      vol_id = vol_data$id,
      vol_name = vol_data$name,
      sessions_shared = ifelse(
        is.null(vol_data$publicsharefull),
        FALSE,
        vol_data$publicsharefull
      ),
      public_access = vol_data$publicaccess,
      n_sessions = dim(vol_sessions)[1] - 1,
      created_date = lubridate::as_datetime(vol_data$creation)
    )
    
    dplyr::cross_join(vol_owners, this_vol)
  }

#-------------------------------------------------------------------------------
get_info_from_session <-
  function(volume_container, ignore_materials = FALSE) {
    # ignore materials
    if (ignore_materials) {
      if ("top" %in% names(volume_container))
        return(NULL)
    } else {
      if (!("name" %in% names(volume_container)))
        volume_container$name <- NA
      if (!("date" %in% names(volume_container)))
        volume_container$date <- NA
      if (!("release" %in% names(volume_container)))
        volume_container$release <- NA
    }
    
    tibble::tibble(
      session_id = volume_container$id,
      session_name = volume_container$name,
      session_date = volume_container$date,
      session_release = volume_container$release
    )
  }