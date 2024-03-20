# functions.R

#-------------------------------------------------------------------------------
auth_to_google <- function(gacct = "rick.o.gilmore") {
  googledrive::drive_auth(email = gacct)
}


###############################################################################
# Tags and keywords

#-------------------------------------------------------------------------------
load_old_tags_data <-
  function(csv_dir = "csv", csv_fn = "databrary-tags.csv") {
    stopifnot(dir.exists(csv_dir))
    stopifnot(file.exists(file.path(csv_dir, csv_fn)))
    
    old_tags <-
      readr::read_csv(file.path(csv_dir, csv_fn), show_col_types = FALSE)
    old_tags
  }

#-------------------------------------------------------------------------------
refresh_volume_tags_df <- function(vol_ids = 1:100, vb = TRUE) {
  message(
    "Refreshing tags & keywords data for volumes ",
    min(vol_ids),
    ":",
    max(vol_ids),
    ". Please be patient."
  )
  purrr::map_df(
    .x = vol_ids,
    .f = make_volume_tags_df,
    vb = vb,
    .progress = "Vol tags:"
  )
}

#-------------------------------------------------------------------------------
update_vol_tags_csv <-
  function(df,
           csv_dir = "csv",
           csv_fn = "databrary-tags.csv") {
    stopifnot(!rlang::is_empty(df))
    stopifnot(dir.exists(csv_dir))
    stopifnot(file.exists(file.path(csv_dir, csv_fn)))
    
    readr::write_csv(df, file.path(csv_dir, csv_fn))
  }

#-------------------------------------------------------------------------------
make_volume_tags_df <- function(vol_id, vb = FALSE) {
  if (vb)
    message(paste0("Gathering tags from volume ", vol_id))
  these_tags <- databraryr::list_volume_tags(vol_id)
  if (rlang::is_empty(these_tags)) {
    df <- data.frame(
      vol_id = vol_id,
      url = paste0("https://nyu.databrary.org/volume/",
                   vol_id),
      tags = NA,
      weight = NA
    )
  } else {
    these_tags <- these_tags %>%
      dplyr::select(., id, weight) %>%
      dplyr::rename(., tags = id)
    df <- these_tags
    df$vol_id = vol_id
    df$url <- paste0("https://nyu.databrary.org/volume/", vol_id)
  }
  dplyr::select(df, vol_id, url, tags, weight)
}

#-------------------------------------------------------------------------------
make_stem_tags_df <- function(tags_df,
                              vb = FALSE,
                              save_csv = TRUE) {
  stem_tags <- dplyr::filter(tags_df, tags %in% select_tags)
  stem_tags <- dplyr::arrange(stem_tags, vol_id, tags)
  
  # Unique vol ids to get volume metadata, esp title
  stem_vol_ids <- unique(stem_tags$vol_id)
  
  # Pull titles
  if (vb)
    message("Gathering STEM-related tags from n=",
            length(stem_vol_ids),
            " volumes.")
  stem_vols_df <- purrr::map_df(
    .x = stem_vol_ids,
    .f = databraryr::list_volume_metadata,
    .progress = "STEM tags:"
  )
  
  stem_vols_df <-
    dplyr::left_join(stem_vols_df, stem_tags, multiple = "all")
  
  stem_vols_df <- stem_vols_df %>%
    dplyr::filter(., vol_id != 109) %>% # Empty volume
    dplyr::select(., -owners, -permission, -doi)
  
  stem_vols_df
}

###############################################################################
# Funders

#-------------------------------------------------------------------------------
get_volume_funding <- function(vol_id, vb = FALSE) {
  stopifnot(is.numeric(vol_id))
  
  if (vb)
    message('Getting funders for volume ', vol_id)
  databraryr::list_volume_funding(vol_id)
}

#-------------------------------------------------------------------------------
make_all_session_df <- function(csv_dir = "src/csv") {
  stopifnot(is.character(csv_dir))
  stopifnot(dir.exists(csv_dir))
  
  fl <-
    list.files(csv_dir, "[0-9]{4}\\-sess\\-materials\\.csv", full.names = TRUE)
  purrr::map_df(fl, make_cleaned_session_df, csv_dir, .progress = "Import sessions:")
}


#-------------------------------------------------------------------------------
vol_csv_avail <- function(vol_id) {
  stopifnot(is.numeric(vol_id))
  stopifnot(vol_id > 0)
  
  this_url <-
    paste0("https://nyu.databrary.org/volume/", vol_id, "/csv")
  r <- httr::GET(this_url,
                 httr::authenticate(
                   Sys.getenv("DATABRARY_LOGIN"),
                   keyring::key_get(
                     service = "databrary",
                     username = Sys.getenv("DATABRARY_LOGIN")
                   )
                 ))
  
  if (httr::status_code(r) == 404) {
    FALSE
  } else {
    TRUE
  }
}

################################################################################
# Demographics

#-------------------------------------------------------------------------------
get_volume_demog <- function(vol_id = 4, vb = FALSE) {
  stopifnot(is.numeric(vol_id))
  stopifnot(vol_id > 0)
  
  this_url <-
    paste0("https://nyu.databrary.org/volume/", vol_id, "/csv")
  
  if (vb)
    message(paste0("Gathering demog data from ", this_url))
  
  r <- try(httr::GET(this_url,
                     httr::authenticate(
                       Sys.getenv("DATABRARY_LOGIN"),
                       keyring::key_get(
                         service = "databrary",
                         username = Sys.getenv("DATABRARY_LOGIN")
                       )
                     )), silent = TRUE)
  
  if (inherits(r, 'try-error')) {
    if (vb)
      message(" Error accessing volume ", vol_id)
    return(NULL)
  }
  
  if (httr::status_code(r) == 404) {
    if (vb)
      message(" 404 error from ", this_url)
    return(NULL)
  }
  
  if (is.null(r)) {
    if (vb)
      message(" NULL response from ", this_url)
    return(NULL)
  }
  
  c <- httr::content(r,
                     show_col_types = FALSE,
                     type = "text/csv",
                     encoding = "utf-8")
  
  if (is.null(c)) {
    if (vb)
      message("  No CSV data returned from ", this_url)
    return(NULL)
  }
  
  if (vb)
    message("Converting response to data frame.")
  if (is.data.frame(c)) {
    if (vb)
      message(paste0("Imported data frame. Cleaning up."))
    r_df <- dplyr::mutate(c, vol_id = vol_id)
    names(r_df) <- stringr::str_replace_all(names(r_df),
                                            pattern = "[\\-|\\.| ]", replacement = "_")
    r_df <-
      dplyr::filter(r_df,!(stringr::str_detect(session_date, 'materials')))
    r_df <- dplyr::mutate(r_df,
                          session_id = as.character(session_id))
    # TODO: Devise a more elegant way to clean-up/normalize the spreadsheet data
    if ("participant_ID" %in% names(r_df))
      r_df <-
      dplyr::mutate(r_df, participant_ID = as.character(participant_ID))
    if ("participant1_ID" %in% names(r_df))
      r_df <-
      dplyr::mutate(r_df, participant1_ID = as.character(participant1_ID))
    if ("participant2_ID" %in% names(r_df))
      r_df <-
      dplyr::mutate(r_df, participant2_ID = as.character(participant2_ID))
    if ("participant3_ID" %in% names(r_df))
      r_df <-
      dplyr::mutate(r_df, participant3_ID = as.character(participant3_ID))
    if ("participant4_ID" %in% names(r_df))
      r_df <-
      dplyr::mutate(r_df, participant4_ID = as.character(participant4_ID))
    if ("participant4_ID" %in% names(r_df))
      r_df <-
      dplyr::mutate(r_df, participant5_ID = as.character(participant5_ID))
    if ("session_date" %in% names(r_df))
      r_df <-
      dplyr::mutate(r_df, session_date = as.character(session_date))
    if ("participant_birthdate" %in% names(r_df))
      r_df <-
      dplyr::mutate(r_df, participant_birthdate = as.character(participant_birthdate))
    if ("session_release" %in% names(r_df))
      r_df <-
      dplyr::mutate(r_df, session_release = as.character(session_release))
    if ("session_name" %in% names(r_df))
      r_df <-
      dplyr::mutate(r_df, session_name = as.character(session_name))
    if ("group_name" %in% names(r_df))
      r_df <-
      dplyr::mutate(r_df, group_name = as.character(group_name))
    return(r_df)
  }
  else {
    if (vb)
      message("Can't coerce to data frame. Skipping.\n")
    return(NULL)
  }
  r_df
}


#-------------------------------------------------------------------------------
get_volume_birthdate <- function(vol_id, vb = FALSE) {
  v_ss <-
    try(databraryr::get_session_as_df(vol_id), silent = TRUE)
  if (vb)
    message(paste0(
      "....Gathering participant_birthdate data from volume ",
      vol_id
    ))
  
  if ("participant_birthdate" %in% names(v_ss)) {
    dplyr::select(v_ss, vol_id, session_id, participant_birthdate)
  } else {
    if (vb)
      message(".....participant_birthdate not found for volume ", vol_id)
    data.frame(
      vol_id = vol_id,
      session_id = NA,
      participant_birthdate = NA
    )
  }
}

#-------------------------------------------------------------------------------
get_volume_session_date <- function(vol_id, vb = FALSE) {
  v_ss <-
    try(databraryr::get_session_as_df(vol_id), silent = TRUE)
  if (vb)
    message(paste0("....Gathering session_date data from volume ",
                   vol_id))
  
  if ("session_date" %in% names(v_ss)) {
    dplyr::select(v_ss, vol_id, session_date)
  } else {
    message(".....session_date not found for volume ", vol_id)
    data.frame(vol_id = vol_id,
               session_id = NA,
               session_date = NA)
  }
}

#-------------------------------------------------------------------------------
get_volume_gender <- function(vol_id, vb = FALSE) {
  v_ss <-
    try(databraryr::get_session_as_df(vol_id), silent = TRUE)
  if (vb)
    message(paste0("....Gathering participant_gender data from volume ", vol_id))
  
  if ("participant_gender" %in% names(v_ss)) {
    dplyr::select(v_ss, vol_id, session_id, participant_gender)
  } else {
    if (vb)
      message(".....participant_gender not found for volume ", vol_id)
    data.frame(
      vol_id = vol_id,
      session_id = NA,
      participant_gender = NA
    )
  }
}

#-------------------------------------------------------------------------------
get_volume_race <- function(vol_id, vb = FALSE) {
  v_ss <-
    try(databraryr::get_session_as_df(vol_id), silent = TRUE)
  if (vb)
    message(paste0("....Gathering participant_race data from volume ", vol_id))
  
  if ("participant_race" %in% names(v_ss)) {
    dplyr::select(v_ss, vol_id, session_id, participant_race)
  } else {
    if (vb)
      message(".....participant_race not found for volume ", vol_id)
    data.frame(
      vol_id = vol_id,
      session_id = NA,
      participant_race = NA
    )
  }
}

#-------------------------------------------------------------------------------
get_volume_ethnicity <- function(vol_id, vb = FALSE) {
  v_ss <-
    try(databraryr::get_session_as_df(vol_id), silent = TRUE)
  if (vb)
    message(paste0(
      "....Gathering participant_ethnicity data from volume ",
      vol_id
    ))
  
  if ("participant_ethnicity" %in% names(v_ss)) {
    dplyr::select(v_ss, vol_id, session_id, participant_ethnicity)
  } else {
    if (vb)
      message(".....participant_ethnicity not found for volume ", vol_id)
    data.frame(
      vol_id = vol_id,
      session_id = NA,
      participant_ethnicity = NA
    )
  }
}

#-------------------------------------------------------------------------------
get_volumes_demo <- function(min_vol_id = 1,
                             max_vol_id = 10,
                             vb = FALSE) {
  stopifnot(is.numeric(min_vol_id))
  stopifnot(min_vol_id > 0)
  stopifnot(is.numeric(max_vol_id))
  stopifnot(max_vol_id > 0)
  stopifnot(min_vol_id < max_vol_id)
  
  vols_range <- min_vol_id:max_vol_id
  
  db_status <- databraryr::login_db(Sys.getenv("DATABRARY_LOGIN"))
  if (!db_status) {
    message("Unable to login to Databrary. Only public data will be gathered")
  }
  
  if (vb)
    message(paste0(
      "Getting demographic data for volumes ",
      min_vol_id,
      ":",
      max_vol_id,
      "\n"
    ))
  
  ml <-
    purrr::map_df(vols_range, get_volume_demog, vb, .progress = "Vol demog:")
  
  databraryr::logout_db()
  
  purrr::list_rbind(ml)
}


#-------------------------------------------------------------------------------
save_volumes_demo <- function(df, min_vol_id, max_vol_id,
                              dir = "participant-demographics/csv") {
  message(paste0(
    "Saving demographic data for volumes ",
    min_vol_id,
    "-",
    max_vol_id,
    "\n"
  ))
  fn <-
    paste0(
      dir,
      "/",
      stringr::str_pad(min_vol_id, 5, pad = "0"),
      "-",
      stringr::str_pad(max_vol_id, 5, pad = "0"),
      "-demog.csv"
    )
  fn
  readr::write_csv(df, fn)
}

# Won't work because participants could have different demographic characteristics
n_particip_in_session <- function(df) {
  p_indices <-
    stringr::str_match(names(v2), "participant([0-9]+)")[, 2]
  p_indices <- p_indices[!is.na(p_indices)]
  max(as.numeric(p_indices))
}

#-------------------------------------------------------------------------------
pivot_ss_longer <- function(df) {
  require(tidyverse)
  df %>%
    tidyr::pivot_longer(
      .,
      cols = matches('participant[0-9]*\\-ID'),
      names_to = NULL,
      values_to = "participant-id"
    ) %>%
    dplyr::distinct() %>%
    tidyr::pivot_longer(
      .,
      cols = matches('participant[0-9]*\\-birthdate'),
      names_to = NULL,
      values_to = "participant-birthdate"
    ) %>%
    # dplyr::distinct() %>%
    # tidyr::pivot_longer(., cols = matches('participant[0-9]*\\-race'), names_to = NULL, values_to = "participant-race") %>%
    # dplyr::distinct() %>%
    # tidyr::pivot_longer(., cols = matches('participant[0-9]*\\-gender'), names_to = NULL, values_to = "participant-gender") %>%
    # dplyr::distinct() %>%
    # tidyr::pivot_longer(., cols = matches('participant[0-9]*\\-ethnicity'), names_to = NULL, values_to = "participant-ethnicity") %>%
    dplyr::distinct()
}

################################################################################
# Downloads new demographic data from Databrary and saves it as a new CSV
#
# For efficiency reasons, the code takes 10 volumes at a time and saves them
# in individual CSVs labeled with the volume numbers.
get_save_volumes_demo <- function(min_vol_id = 1,
                                  max_vol_id = 10,
                                  dir = "src/csv",
                                  vb = FALSE) {
  stopifnot(is.numeric(min_vol_id))
  stopifnot(is.numeric(max_vol_id))
  stopifnot(min_vol_id > 0)
  stopifnot(max_vol_id > 0)
  stopifnot(min_vol_id < max_vol_id)
  stopifnot(is.character(dir))
  stopifnot(dir.exists(dir))
  
  suppressPackageStartupMessages(require(tidyverse))
  
  df <- get_volumes_demo(min_vol_id, max_vol_id, vb)
  save_volumes_demo(df, min_vol_id, max_vol_id, dir)
}

#-------------------------------------------------------------------------------
regenerate_vol_demo_csvs <- function(new_vol_rg_min = 1261,
                                     new_vol_rg_max = 1270,
                                     csv_dir = "participant-demographics/csv",
                                     vb = FALSE) {
  require(purrr)
  
  if (!is.numeric(new_vol_rg_min)) {
    stop('`new_vol_rg_min` must be a number.')
  }
  if (new_vol_rg_min < 1) {
    stop('`new_vol_rg_min` must > 0.')
  }
  if (!is.numeric(new_vol_rg_max)) {
    stop('`new_vol_rg_max` must be a number.')
  }
  if (new_vol_rg_max < 1) {
    stop('`new_vol_rg_max` must > 0.')
  }
  if (new_vol_rg_min > new_vol_rg_max) {
    stop('`new_vol_rg_min` must < `new_vol_rg_max.')
  }
  if (!is_character(csv_dir)) {
    stop('`csv_dir` must be a character string.')
  }
  if (!dir.exists(csv_dir)) {
    stop('Directory not found: `', csv_dir, '`.')
  }
  
  lo <- seq(from = new_vol_rg_min, to = new_vol_rg_max, by = 10)
  hi <- seq(from = new_vol_rg_min + 9, to = new_vol_rg_max, by = 10)
  
  # Rick really loves functional programming
  purrr::map2(.x = lo,
              .y = hi,
              get_save_volumes_demo,
              csv_dir,
              vb)
}

#-------------------------------------------------------------------------------
load_demog_csvs <- function(dir = "participant-demographics/csv") {
  if (!is.character(dir)) {
    stop("'dir' must be a string")
  }
  if (!dir.exists(dir)) {
    stop("'dir' does not exist.")
  }
  
  fl <- list.files(dir, "-demog\\.csv$", full.names = TRUE)
  if (is.null(fl)) {
    stop(paste0("No csv files in ", dir, "."))
  }
  purrr::map_dfr(fl,
                 readr::read_csv,
                 col_types = 'c',
                 show_col_types = FALSE)
}


get_volume_first_owner <- function(vol_id) {
  df <- databraryr::list_volume_owners(vol_id)
  if (rlang::is_empty(df)) {
    NULL
  } else {
    df[1, ]
  }
}

#-------------------------------------------------------------------------------
get_volumes_first_owners <-
  function(min_vol_id = 1,
           max_vol_id = 10,
           vb = FALSE) {
    stopifnot(is.numeric(min_vol_id))
    stopifnot(is.numeric(max_vol_id))
    stopifnot(min_vol_id > 0)
    stopifnot(max_vol_id > 0)
    stopifnot(min_vol_id < max_vol_id)
    
    vols_range <- min_vol_id:max_vol_id
    if (vb)
      message("Gathering first owners from volumes ",
              min_vol_id,
              ":",
              max_vol_id)
    purrr::map_dfr(.x = vols_range,
                   .f = get_volume_first_owner,
                   .progress = "Vol 1st owners:")
  }




#-------------------------------------------------------------------------------
get_save_volumes_first_owners <-
  function(min_vol_id = 1,
           max_vol_id = 10,
           dir = "src/csv",
           vb = FALSE) {
    stopifnot(is.numeric(min_vol_id))
    stopifnot(is.numeric(max_vol_id))
    stopifnot(min_vol_id > 0)
    stopifnot(max_vol_id > 0)
    stopifnot(min_vol_id < max_vol_id)
    stopifnot(is.character(dir))
    stopifnot(dir.exists(dir))
    
    if (vb)
      message(paste0(
        "Getting first owner data for volumes ",
        min_vol_id,
        ":",
        max_vol_id
      ))
    
    df <- get_volumes_first_owners(min_vol_id, max_vol_id)
    
    if (vb)
      message(paste0(
        "Saving first owner data for volumes ",
        min_vol_id,
        ":",
        max_vol_id
      ))
    
    save_volumes_owners(df, min_vol_id, max_vol_id, dir, fn_suffix = "-first-owners.csv", vb)
  }


#-------------------------------------------------------------------------------
load_owner_csvs <- function(dir = "src/csv",
                            fn_suffix = "-owners") {
  if (!is.character(dir)) {
    stop("'dir' must be a string")
  }
  if (!dir.exists(dir)) {
    stop("'dir' does not exist.")
  }
  
  fl <-
    list.files(dir, paste0(fn_suffix, "\\.csv$"), full.names = TRUE)
  if (is.null(fl)) {
    stop(paste0("No csv files in ", dir, "."))
  }
  purrr::map_dfr(fl,
                 readr::read_csv,
                 col_types = 'c',
                 show_col_types = FALSE)
}

#-------------------------------------------------------------------------------
render_participant_demog_report <- function(db_login) {
  if (!is.character(db_login)) {
    stop('`db_login` must be a character string.')
  }
  
  rmarkdown::render(
    "participant-demographics/participant-demog-report.Rmd",
    params = list(db_login = db_login)
  )
  databraryr::logout_db()
  if (file.exists("participant-demographics/.databrary.RData")) {
    file.remove("participant-demographics/.databrary.RData")
  }
}

#-------------------------------------------------------------------------------
my_gender <- function(v, a) {
  if (a == 'warning') {
    warning('some warning')
    return_value <- get_volume_gender(v)
  } else if (a == 'error') {
    warning('Error detected, ignored.')
    return_value = NULL
  } else {
    return_value <- get_volume_gender(v)
  }
}

#-------------------------------------------------------------------------------
get_volume_ss <- function(vol_id = 1,
                          omit_materials = FALSE,
                          vb = FALSE) {
  message(paste0("Gathering spreadsheet data from volume ", vol_id))
  
  if (!vol_csv_avail(vol_id)) {
    if (vb)
      message("No CSV available for volume ", vol_id)
    return(NULL)
  }
  
  v_ss <-
    try(databraryr::get_session_as_df(vol_id), silent = TRUE)
  
  if ('try-error' %in% class(v_ss)) {
    message(".Error loading CSV for volume ", vol_id)
    df <- NULL
    return(df)
  } else if (is.null(v_ss)) {
    message(".NULL CSV for volume ", vol_id)
    df <- NULL
    return(df)
  } else {
    df <- v_ss
    if (omit_materials) {
      df <-
        dplyr::filter(df,!(stringr::str_detect(session_date, 'materials')))
    }
    df$vol_id <- vol_id
    return(df)
  }
}

#-------------------------------------------------------------------------------
get_volume_ss_httr <- function(vol_id = 1,
                               omit_materials = FALSE,
                               vb = FALSE) {
  
}

#-------------------------------------------------------------------------------
get_volume_ss_vroom <- function(vol_id = 4,
                                omit_materials = FALSE,
                                vb = FALSE) {
  if (vb)
    message(paste0("Gathering spreadsheet data from volume ", vol_id))
  this_url <-
    paste0("https://nyu.databrary.org/volume/", vol_id, "/csv")
  
  if (!vol_csv_avail(vol_id)) {
    if (vb)
      message("No CSV available for volume ", vol_id)
    return(NULL)
  }
  
  v_ss <-
    try(vroom::vroom(
      this_url,
      delim = ",",
      col_types = "cccccccccccccccccccc",
      show_col_types = FALSE
    ),
    silent = TRUE)
  if (inherits(v_ss, 'try-error')) {
    if (vb)
      message(" Error loading CSV from ", this_url)
    df <- NULL
    return(df)
  } else if (is.null(v_ss)) {
    if (vb)
      message(" NULL CSV for volume ", vol_id)
    df <- NULL
    return(df)
  } else {
    df <- v_ss
    names(df) <- stringr::str_replace_all(names(df), '[-\\. ]', '_')
    if (omit_materials) {
      df <-
        dplyr::filter(df,!(stringr::str_detect(session_date, 'materials')))
    }
    df$vol_id <- as.character(vol_id)
    return(df)
  }
}

#-------------------------------------------------------------------------------
get_save_volume_ss <- function(vol_id = 4,
                               dir = "src/csv",
                               omit_materials = FALSE,
                               vb = FALSE) {
  stopifnot(is.numeric(vol_id))
  stopifnot(vol_id > 0)
  stopifnot(is.character(dir))
  stopifnot(is.logical(omit_materials))
  stopifnot(is.logical(vb))
  
  # df <- get_volume_ss(vol_id, omit_materials, vb)
  df <- get_volume_ss_vroom(vol_id, omit_materials, vb)
  
  if (!is.null(df)) {
    if (omit_materials) {
      fn <-
        paste0(dir,
               "/",
               stringr::str_pad(vol_id, 5, pad = "0"),
               "-sess.csv")
    } else {
      fn <-
        paste0(dir,
               "/",
               stringr::str_pad(vol_id, 5, pad = "0"),
               "-sess-materials.csv")
    }
    
    readr::write_csv(df, fn)
    if (vb)
      message(" Saved ", fn)
    
  } else {
    if (vb)
      message(" No data available for volume: ", vol_id)
  }
}

#-------------------------------------------------------------------------------
get_save_multiple_volume_ss <-
  function(vol_id_min,
           vol_id_max,
           csv_dir = "src/csv",
           omit_materials = FALSE,
           vb = FALSE) {
    stopifnot(is.numeric(vol_id_min))
    stopifnot(is.numeric(vol_id_max))
    stopifnot(vol_id_min > 0)
    stopifnot(vol_id_max > 0)
    stopifnot(vol_id_min < vol_id_max)
    stopifnot(is.character(csv_dir))
    stopifnot(dir.exists(csv_dir))
    
    db_status <-
      databraryr::login_db(Sys.getenv("DATABRARY_LOGIN"))
    if (!db_status) {
      message("Unable to login to Databrary. Only public data will be gathered")
    }
    
    purrr::walk(
      c(vol_id_min:vol_id_max),
      get_save_volume_ss,
      csv_dir,
      omit_materials,
      vb,
      .progress = "Vol ss:"
    )
  }

#-------------------------------------------------------------------------------
extract_sessions_from_vol_csv <-
  function(csv_fn = "src/csv/0002-sess-materials.csv", vb = FALSE) {
    stopifnot(is.character(csv_fn))
    stopifnot(file.exists(csv_fn))
    
    df <- read.csv(csv_fn, colClasses = "character")
    
    if (dim(df)[1] == 0) {
      if (vb)
        message("CSV empty: ", csv_fn)
      NULL
    } else {
      dplyr::filter(df,!(stringr::str_detect(session_date, 'materials')))
    }
  }

#-------------------------------------------------------------------------------
count_sessions_materials_folders <-
  function(csv_fn = "participant-demographics/csv/0002-sess-materials.csv") {
    stopifnot(is.character(csv_fn))
    stopifnot(file.exists(csv_fn))
    
    df <- readr::read_csv(csv_fn, show_col_types = FALSE)
    
    if (dim(df)[1] == 0) {
      message("CSV empty: ", csv_fn)
      NULL
    } else {
      df_sess <-
        dplyr::filter(df,!(stringr::str_detect(session_date, 'materials')))
      n_sess <- dim(df_sess)[1]
      
      df_materials <-
        dplyr::filter(df, (stringr::str_detect(session_date, 'materials')))
      n_materials <- dim(df_materials)[1]
      
      vol_id <- stringr::str_extract(csv_fn, "[0-9]{4}")
      
      tibble::tibble(vol_id, n_sess, n_materials)
    }
    
  }

#-------------------------------------------------------------------------------
detect_n_participants <- function(df) {
  sum(stringr::str_detect(names(df), "participant[1-9]?_ID"))
}

#-------------------------------------------------------------------------------
generate_sessions_materials_df <-
  function(csv_folder = "participant-demographics/csv") {
    stopifnot(is.character(csv_folder))
    stopifnot(file.exists(csv_folder))
    
    fl <-
      list.files(csv_folder,
                 "[0-9]{4}\\-sess\\-materials\\.csv",
                 full.names = TRUE)
    if (length(fl) <= 0) {
      message("No session/materials CSV files found: ", csv_folder)
      NULL
    } else {
      purrr::map_df(fl, count_sessions_materials_folders)
    }
  }

#-------------------------------------------------------------------------------
extract_participant_vars <- function(df) {
  stopifnot(is.data.frame(df))
  
  stringr::str_match(names(df), "participant[1-9]?_([A-Za-z]+)")[, 2]
}

#-------------------------------------------------------------------------------
extract_participant_identifiers <- function(df) {
  stopifnot(is.data.frame(df))
  
  x <- unique(stringr::str_extract(names(df), "participant[1-9]?"))
  x[!is.na(x)]
}

#-------------------------------------------------------------------------------
# If there are multiple participants, augmented data frame
alter_sess_df_w_mult_part <- function(df, vb = FALSE) {
  if (is.null(df))
    return(NULL)
  stopifnot(is.data.frame(df))
  
  n_particip <- detect_n_participants(df)
  if (n_particip > 0) {
    purrr::map_df(1:n_particip, select_particip_sess_by_number, df)
  } else {
    if (vb)
      message(" No participant data:...skipping")
    NULL
  }
}

#-------------------------------------------------------------------------------
# Helper function to extract data frame for participant1, participant2, etc.
select_particip_sess_by_number <- function(i, df) {
  stopifnot(is.numeric(i))
  stopifnot(i > 0)
  stopifnot(is.data.frame(df))
  
  vars_not_part_specific <-
    !(stringr::str_detect(names(df), "participant"))
  if (i == 1) {
    part_specific_vars <-
      stringr::str_detect(names(df), "participant[1]?_")
  } else {
    part_specific_vars <-
      stringr::str_detect(names(df), paste0("participant[", i, "]{1}"))
  }
  select_these <- vars_not_part_specific | part_specific_vars
  out_df <- df[, select_these]
  stripped_names <-
    stringr::str_replace(names(out_df),
                         paste0("participant[", i, "]{1}"),
                         "participant")
  names(out_df) <- stripped_names
  out_df
}

#-------------------------------------------------------------------------------
make_cleaned_session_df <- function(csv_fn, vb = FALSE) {
  stopifnot(is.character(csv_fn))
  stopifnot(file.exists(csv_fn))
  
  if (vb)
    message("Extracting session info: ", csv_fn)
  df <- extract_sessions_from_vol_csv(csv_fn, vb)
  alter_sess_df_w_mult_part(df, vb)
}

#-------------------------------------------------------------------------------
download_csv_vroom <- function(vol_id = 4, vb = FALSE) {
  this_url <-
    paste0("https://nyu.databrary.org/volume/", vol_id, "/csv")
  x <-
    try(vroom::vroom(this_url, col_types = "cccccccccccccccccccc",  delim = ","),
        silent = TRUE)
  if (inherits(x, 'try-error'))  {
    if (vb)
      message("Error in downloading spreadsheet from  ", this_url)
    return(NULL)
  } else {
    names(x) <- stringr::str_replace_all(names(x), '[-\\. ]', '_')
    x$vol_id <- as.character(vol_id)
    x
  }
}

#-------------------------------------------------------------------------------
create_aggregate_demog_df <-
  function(csv_dir = 'src/csv', vb = FALSE) {
    stopifnot(is.character(csv_dir))
    stopifnot(dir.exists(csv_dir))
    
    fl <-
      list.files(csv_dir, "[0-9]+\\-sess\\-materials.csv", full.names = TRUE)
    df <-
      purrr::map(fl, convert_vol_ss_csv_to_particip_df, vb, .progress = "Particip df") %>%
      list_rbind()
    
    rename_with(df, ~ gsub("[- ]", "_", .x))
  }


################################################################################
# Data about shared volumes and their sessions.

#-------------------------------------------------------------------------------
get_volume_name <- function(vol_id) {
  message("Getting name for volume ", vol_id)
  vol_metadata <- databraryr::list_volume_metadata(vol_id)
  if (!is.null(vol_metadata)) {
    tibble::tibble(vol_id = vol_id,
                   vol_name = as.character(vol_metadata$name))
  } else {
    tibble::tibble(vol_id = vol_id, vol_name = NA)
  }
}