# institution_investigator_functions.R

################################################################################
# Institutions and Authorized Investigators (AIs)
#
# These functions process data about institutions and authorized investigators






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