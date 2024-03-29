# institution_investigator_functions.R

################################################################################
# Institutions and Authorized Investigators (AIs)
#
# These functions process data about institutions and authorized investigators

#-------------------------------------------------------------------------------
update_inst_invest_df <-
  function(csv_dir = "csv", csv_fn = "institutions-investigators.csv") {
    stopifnot(is.character(csv_dir))
    stopifnot(is.character(csv_fn))
    
    old_df <- load_old_inst_invest_data(csv_dir, csv_fn)
    new_item <- get_inst_invest_datab()
    
    new_df <- old_df
    next_entry <- dim(old_df)[1] + 1
    new_df[next_entry, ] = NA
    new_df[next_entry, ] <- new_item
    new_df
  }

#-------------------------------------------------------------------------------
load_old_inst_invest_data <-
  function(csv_dir = "csv", csv_fn = "institutions-investigators.csv") {
    stopifnot(dir.exists(csv_dir))
    stopifnot(file.exists(file.path(csv_dir, csv_fn)))
    
    old_stats <-
      readr::read_csv(file.path(csv_dir, csv_fn), show_col_types = FALSE)
    dplyr::mutate(old_stats, date = lubridate::as_datetime(date))
  }

#-------------------------------------------------------------------------------
get_inst_invest_datab <- function() {
  # suppressPackageStartupMessages(require(tidyverse))
  
  new_stats <- databraryr::get_db_stats()
  new_stats$date <- lubridate::as_datetime(new_stats$date)
  
  new_stats <- new_stats |>
    dplyr::select(date, institutions, investigators, affiliates) |>
    dplyr::mutate(date = lubridate::as_datetime(date))
  
  if (rlang::is_empty(new_stats)) {
    warning("Unable to retrieve new statistics from Databrary.")
    NULL
  } else {
    new_stats
  }
}

#-------------------------------------------------------------------------------
update_inst_invest_csv <-
  function(df,
           csv_dir = "csv",
           csv_fn = "institutions-investigators.csv") {
    stopifnot(!rlang::is_empty(df))
    stopifnot(dir.exists(csv_dir))
    stopifnot(file.exists(file.path(csv_dir, csv_fn)))
    
    fn <- file.path(csv_dir, csv_fn)
    readr::write_csv(df, fn)
    fn
  }

#-------------------------------------------------------------------------------
update_invest_csv <- function(all_inst_df,
                              csv_dir = "src/csv",
                              vb = FALSE) {
  stopifnot(is.data.frame(all_inst_df))
  stopifnot(is.character(csv_dir))
  stopifnot(dir.exists(csv_dir))
  stopifnot(is.logical(vb))
  
  if (vb)
    message("Filtering for active institutions with AIs.")
  inst_ids <-
    dplyr::filter(all_inst_df, daa == TRUE, n_auth_invest > 0) |>
    dplyr::select(inst_id)
  ids <- as.integer(unlist(inst_ids))
  
  if (vb)
    message("There are n=",
            dim(ids)[1],
            " institutions with AIs. Retrieving AI info.")
  
  ais_l <-
    purrr::map(ids, get_ais_from_inst, vb, .progress = "AIs from insts:")
  
  if (vb)
    message("Making data frame.")
  ais_df <- purrr::list_rbind(ais_l)
  
  fn <- file.path(csv_dir, "all-ais.csv")
  if (vb)
    message("Writing CSV: ", fn)
  readr::write_csv(ais_df, fn)
  
  fn
}

#-------------------------------------------------------------------------------
make_ais_df <- function(all_inst_df, vb = TRUE) {
  
  stopifnot(is.data.frame(all_inst_df))
  stopifnot(is.logical(vb))
  
  if (vb)
    message("Filtering for active institutions with AIs.")
  inst_ids <-
    dplyr::filter(all_inst_df, daa == TRUE, n_auth_invest > 0) |>
    dplyr::select(inst_id)
  ids <- as.integer(unlist(inst_ids))
  
  if (vb)
    message("There are n = ",
            length(ids)[1],
            " institutions with AIs. Retrieving AI info.")
  
  ais_l <-
    purrr::map(ids, get_ais_from_inst, vb, .progress = "AIs from insts:")
  
  if (vb)
    message("Making data frame.")
  purrr::list_rbind(ais_l)
  
}

#-------------------------------------------------------------------------------
get_all_inst <- function(csv_dir = "src/csv",
                         save_csv = TRUE,
                         vb = FALSE) {
  inst_fl <- list.files(csv_dir, "inst\\-[0-9]+", full.names = TRUE)
  
  if (vb)
    message("Loading institution CSVs from ", csv_dir)
  inst_l <-
    purrr::map(inst_fl, readr::read_csv, show_col_types = FALSE)
  
  if (vb)
    message("Making dataframe.")
  inst_df <- purrr::list_rbind(inst_l)
  
  if (save_csv) {
    fn <- file.path(csv_dir, "all-institutions.csv")
    if (vb)
      message("Writing ", fn)
    readr::write_csv(inst_df, fn)
  }
  inst_df
}

#-------------------------------------------------------------------------------
get_ais_from_inst <- function(inst_id = 8, vb = FALSE) {
  if (vb)
    message("Getting AIs from institution ", inst_id)
  inst_df <- databraryr::list_party(inst_id)
  
  if (!is.null(dim(inst_df$children))) {
    ais_df <- as.data.frame(inst_df$children$party)
    ais_df <- dplyr::rename(ais_df,
                            ai_id = id,
                            ai_last = sortname,
                            ai_first = prename)
    
    df <- tibble::tibble(ais_df)
    df <- dplyr::mutate(
      df,
      inst_id = inst_df$id,
      inst_name = inst_df$sortname,
      inst_db_url = paste0("https://nyu.databrary.org/party/", inst_df$id),
      ai_db_url = paste0("https://nyu.databrary.org/party/", ai_id)
    )
    df$n_affils <- count_affiliates_for_ais(df$ai_id)
    
    df <- dplyr::arrange(df, desc(n_affils), ai_last, ai_first)
    df
  } else {
    NULL
  }
}

#-------------------------------------------------------------------------------
count_affiliates_for_ai <- function(ai_id) {
  affils <- databraryr::list_affiliates(ai_id)
  if (is.null(affils)) {
    x <- 0
  } else {
    x <- dim(affils)[1]
  }
  x
}

#-------------------------------------------------------------------------------
count_affiliates_for_ais <- function(ai_ids) {
  purrr::map_dbl(ai_ids, count_affiliates_for_ai)
}

#-------------------------------------------------------------------------------
update_inst_csv <- function(csv_fn = "src/csv/institutions.csv",
                            max_id = 10868,
                            save_new = TRUE,
                            update_geo = FALSE,
                            vb = FALSE,
                            db_login_status = FALSE) {

  if (file.exists(csv_fn)) {
    if (vb)
      message("Reading from saved file.")
    old_inst <- readr::read_csv(csv_fn, show_col_types = FALSE)
    max_old_inst_id <- unique(max(old_inst$inst_id))
    if (max_old_inst_id < max_id) {
      if (vb)
        message(" Retrieving data from `party_id` ",
                max_old_inst_id + 1,
                ":",
                max_id)
      new_inst <-
        purrr::map_df((max_old_inst_id + 1):max_id,
                      get_inst_info,
                      update_geo,
                      db_login_status = db_login_status,
                      .progress = "Geo coords:")
      if (vb)
        message(" ", paste0(dim(new_inst)[1], " new institutions added."))
      df <- rbind(old_inst, new_inst)
      if (save_new) {
        if (vb)
          message(paste0(" Writing new file: ", csv_fn))
        readr::write_csv(df, csv_fn)
      }
    } else {
      if (vb)
        message(" No update needed.")
      df <- old_inst
    }
  } else {
    if (vb)
      message("No file found. Recreating from inst_id 1:", max_id)
    df <-
      purrr::map_df(1:max_id,
                    get_inst_info,
                    update_geo = update_geo,
                    vb,
                    db_login_status = db_login_status,
                    .progress = "Inst :")
    if (save_new) {
      if (vb)
        message(paste0(" Writing new file: ", csv_fn))
      readr::write_csv(df, csv_fn)
    }
  }
  df
}

#-------------------------------------------------------------------------------
get_inst_info <-
  function(inst_id = 8,
           update_geo = FALSE,
           vb = FALSE,
           db_login_status = FALSE) {
    if (!is.numeric(inst_id)) {
      warning('`inst_id` must be a number.')
      inst_id <- as.numeric(inst_id)
    }
    
    # suppressPackageStartupMessages(require(databraryr))
    # 
    # if (!db_credentials_valid()) {
    #   message(
    #     "Not logged in to Databrary. Running `databraryr::login_db()` with default credentials."
    #   )
    #   databraryr::login_db(Sys.getenv("DATABRARY_LOGIN"))
    #   return(NULL)
    # }
    
    if(!db_login_status) stop("Not logged in to Databrary.")
    
    if (inst_id > 0) {
      if (databraryr::is_institution(inst_id)) {
        if (vb)
          message("Getting information for institution ", inst_id)
        
        inst_df <- databraryr::list_party(inst_id)
        df <- data.frame(
          inst_id = inst_df$id,
          inst_name = inst_df$sortname,
          inst_url = ifelse('url' %in% names(inst_df), inst_df$url, NA),
          databrary_url = paste0("https://nyu.databrary.org/party/", inst_id)
        )
        if (!is.null(dim(inst_df$children))) {
          df$n_auth_invest <- dim(inst_df$children)[1]
        } else {
          df$n_auth_invest = 0
        }
        if (!is.null(dim(inst_df$parents))) {
          df$daa <- TRUE
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
get_inst_info_save_csv <-
  function(party_id = 8,
           update_geo = FALSE,
           csv_dir = "src/csv",
           vb = FALSE,
           non_insts = c(2, 9, 10, 15),
           db_login_status = FALSE) {
    stopifnot(is.numeric(party_id))
    stopifnot(party_id > 0)
    stopifnot(is.character(csv_dir))
    stopifnot(dir.exists(csv_dir))
    
    if (party_id %in% non_insts) {
      if (vb)
        message("Party ", party_id, " is not a physical institution. Skipping.")
      return(NULL)
    }
    this_inst <- get_inst_info(party_id, update_geo, vb, db_login_status = db_login_status)
    
    if (!is.null(this_inst)) {
      fn <-
        file.path(csv_dir, paste0("inst-", stringr::str_pad(party_id, 5, pad = "0"), ".csv"))
      if (vb)
        message(" Saving ", fn)
      readr::write_csv(this_inst, fn)
    }
  }


#-------------------------------------------------------------------------------
get_save_many_inst_csvs <-
  function(min_id = 1,
           max_id = 100,
           update_geo = FALSE,
           csv_dir = 'src/csv',
           vb = FALSE,
           db_login_status = FALSE) {
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
    
    purrr::walk(min_id:max_id,
                get_inst_info_save_csv,
                update_geo,
                csv_dir,
                vb,
                db_login_status = db_login_status,
                .progress = "Inst CSVs:")
    
    list.files(csv_dir, "inst\\-[0-9]+\\.csv", full.names = TRUE)
  }

#-------------------------------------------------------------------------------
load_inst_df_from_csvs <- function(csv_fl, vb = FALSE) {
  stopifnot(is.character(csv_fl))
  
  if (vb)
    message("Creating data frame from institution CSVs.")
  purrr::map(csv_fl,
             readr::read_csv,
             show_col_types = FALSE,
             .progress = "Inst CSVs:") |> purrr::list_rbind()
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

#-------------------------------------------------------------------------------
extract_inst_csv_id <- function(csv_dir = "src/csv", vb = FALSE) {
  stopifnot(is.character(csv_dir))
  stopifnot(dir.exists(csv_dir))
  
  inst_fl <- list.files(csv_dir, "^inst\\-[0-9]{5}")
  as.numeric(stringr::str_extract(inst_fl, "[0-9]{5}"))
}

#-------------------------------------------------------------------------------
make_inst_df_from_csvs <-
  function(csv_dir = "src/csv",
           omit_inst_id = '00002',
           vb = FALSE) {
    stopifnot(is.character(csv_dir))
    stopifnot(dir.exists(csv_dir))
    
    fl <-
      list.files(csv_dir, "inst\\-[0-9]+\\.csv", full.names = TRUE)
    
    if (vb) message("Retrieved n CSVs: ", length(fl))
    
    omitted_inst <- stringr::str_detect(fl, omit_inst_id)
    fl <- fl[!omitted_inst]
    load_inst_df_from_csvs(fl, vb)
  }