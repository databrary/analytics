#-------------------------------------------------------------------------------
get_save_many_inst_csvs <-
  function(min_id = 1,
           max_id = 100,
           update_geo = FALSE,
           csv_dir = 'src/csv',
           vb = FALSE,
           rq = NULL) {
    stopifnot(is.numeric(min_id))
    stopifnot(min_id > 0)
    stopifnot(is.numeric(max_id))
    stopifnot(max_id > 0)
    stopifnot(max_id >= min_id)
    stopifnot(is.character(csv_dir))
    stopifnot(dir.exists(csv_dir))
    
    if (!ggmap::has_google_key()) {
      if (vb)
        message("No Google maps key found. No geo info will be updated.")
      update_geo = FALSE
    }
    
    purrr::walk(
      min_id:max_id,
      get_inst_info_save_csv,
      update_geo,
      csv_dir,
      vb = vb,
      rq = rq,
      .progress = "Inst CSVs:"
    )
    
    #list.files(csv_dir, "inst\\-[0-9]+\\.csv", full.names = TRUE)
  }

#-------------------------------------------------------------------------------
get_inst_info_save_csv <-
  function(party_id = 8,
           update_geo = FALSE,
           csv_dir = "src/csv",
           vb = FALSE,
           non_insts = c(2, 9, 10, 15),
           rq = NULL) {
    stopifnot(is.numeric(party_id))
    stopifnot(party_id > 0)
    stopifnot(is.character(csv_dir))
    stopifnot(dir.exists(csv_dir))
    
    if (party_id %in% non_insts) {
      if (vb)
        message("Party ", party_id, " is not a physical institution. Skipping.")
      return(NULL)
    }
    this_inst <-
      get_inst_info(
        inst_id = party_id,
        update_geo = update_geo,
        vb = vb,
        rq = rq
      )
    
    if (!is.null(this_inst)) {
      fn <-
        file.path(csv_dir, paste0("inst-", stringr::str_pad(party_id, 5, pad = "0"), ".csv"))
      if (vb)
        message(" Saving ", fn)
      readr::write_csv(this_inst, fn)
      fn
    } else {
      if (vb) message("No institutional data saved for party_id ", party_id)
      NULL
    }
  }

#-------------------------------------------------------------------------------
get_inst_info <-
  function(inst_id = 8,
           update_geo = FALSE,
           vb = FALSE,
           rq = NULL) {
    if (!is.numeric(inst_id)) {
      warning('`inst_id` must be a number.')
      inst_id <- as.numeric(inst_id)
    }
    
    if (inst_id > 0) {
      if (databraryr::is_institution(inst_id)) {
        if (vb)
          message("Getting information for institution ", inst_id)
        
        #inst_df <- databraryr::list_party(inst_id)
        inst_df <-
          databraryr::get_party_by_id(party_id = inst_id,
                                      vb = vb,
                                      rq = rq)
        df <- data.frame(
          inst_id = inst_df$id,
          inst_name = inst_df$sortname,
          inst_url = ifelse('url' %in% names(inst_df), inst_df$url, NA),
          databrary_url = paste0("https://nyu.databrary.org/party/", inst_id)
        )
        
        if (!is.null(inst_df$children)) {
          ais_df <- purrr::map(inst_df$children, as.data.frame) |>
            purrr::list_rbind()
          
          df$n_auth_invest <- dim(ais_df)[1]
        } else {
          df$n_auth_invest <- 0
        }
        
        if (!is.null(inst_df$parents)) {
          auth_inst <- purrr::map(inst_df$parents, as.data.frame) |>
            purrr::list_rbind()
          if (dim(auth_inst)[1] > 0) {
            if ("Databrary" %in% auth_inst$party.sortname) {
              df$daa <- TRUE
            } else {
              df$daa = FALSE
            }
          } else {
            df$daa = FALSE
          }
        } else {
          df$daa <- FALSE
        }
        # Get lat and lon
        if (update_geo == TRUE) {
          if (vb)
            message(" Updating lat/lon coords.")
          df <- update_inst_lat_lon(df, vb)
        } else {
          df$lat = NA
          df$lon = NA
        }
        df
      } else {
        if (vb)
          message("Party ", inst_id, " is not an institution. Skipping.")
        NULL
      }
    } else {
      if (vb)
        message("`inst_id` must be a positive number.")
    }
  }

#-------------------------------------------------------------------------------
extract_inst_csv_id <- function(csv_dir = "src/csv", vb = FALSE) {
  stopifnot(is.character(csv_dir))
  stopifnot(dir.exists(csv_dir))
  
  inst_fl <- list.files(csv_dir, "^inst\\-[0-9]{5}")
  as.numeric(stringr::str_extract(inst_fl, "[0-9]{5}"))
}
#-------------------------------------------------------------------------------
update_inst_lat_lon <- function(inst_df, vb = FALSE) {
  stopifnot(is.data.frame(inst_df))
  
  if (vb)
    message(" Calling ggmap::geocode() with '", inst_df$inst_name, "'.")
  ll <-
    ggmap::geocode(as.character(inst_df$inst_name), override_limit = TRUE)
  
  inst_df$lat = NA
  inst_df$lon = NA
  
  if (!is.na(ll$lat))
    inst_df$lat <- ll$lat
  if (!is.na(ll$lon))
    inst_df$lon <- ll$lon
  
  inst_df
}