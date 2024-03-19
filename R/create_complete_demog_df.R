create_complete_demog_df <- function(fl, vb = FALSE) {
  stopifnot(is.character(fl))
  df <-
    purrr::map(fl, convert_vol_ss_csv_to_particip_df, vb, .progress = "Particip df") %>%
    list_rbind()
  
  rename_with(df, ~ gsub("[- ]", "_", .x))
}

#-------------------------------------------------------------------------------
convert_vol_ss_csv_to_particip_df <- function(csv_fn,
                                              vb = FALSE) {
  stopifnot(is.character(csv_fn))
  
  vol_id <- unique(str_extract(basename(csv_fn), '([0-9]{5})'))
  
  if (vb)
    message(" Reading ", csv_fn)
  df <-
    readr::read_csv(csv_fn, col_types = "cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc",
                    show_col_types = FALSE)
  convert_vol_ss_to_particip_df(df, vol_id, vb)
}

#-------------------------------------------------------------------------------
convert_vol_ss_to_particip_df <- function(df, vol_id, vb = FALSE) {
  stopifnot(is.data.frame(df))
  stopifnot(is.character(vol_id))
  
  if (vb)
    message(" Extracting unique participants")
  unique_p_info <- extract_unique_participant_info(df, vb)
  if (dim(unique_p_info)[2] == 0) {
    if (vb)
      message(" No participant data.")
    return(NULL)
  }
  if (vb)
    message(" Making participant df.")
  p_ids <- extract_participant_ids(unique_p_info)
  p_ids <- p_ids[!is.na(p_ids)]
  if (length(p_ids) > 1) {
    if (vb)
      message(" Multiple participants per session...")
    df <-
      purrr::map(p_ids, extract_single_participant, unique_p_info, vb) |>
      list_rbind()
  } else {
    if (vb)
      message(" Single participant per session...")
    df <- calculate_age_days(unique_p_info, vb)
  }
  df$vol_id <- vol_id
  if (dim(df)[1] < 1) {
    if (vb)
      message("No data in ", vol_id)
    df <- NULL
  }
  df
}

#-------------------------------------------------------------------------------
extract_single_participant <- function(i, df, vb = FALSE) {
  if (vb)
    message(" Extracting participant ", i)
  d <-
    select(df, `session-date`, contains(paste0('participant', i)))
  d <- calculate_age_days(d, vb)
  rename_with(.data = d, ~ gsub("[0-9]+", "", .x))
}

#-------------------------------------------------------------------------------
extract_unique_participant_info <- function(df, vb = FALSE) {
  if (vb)
    message(" Extracting participant info.")
  df %>%
    remove_materials() %>%
    extract_particip_info_session_date() %>%
    distinct()
}

#-------------------------------------------------------------------------------
calculate_age_days <- function(df, vb = FALSE) {
  df_dates <-
    select(df, starts_with('session-date'), ends_with('birthdate'))
  if (dim(df_dates)[1] > 0) {
    if (vb)
      message(' Data frame contains data needed to calculate ages.')
    df_dates <- rename_with(df_dates, ~ gsub("[0-9]+", "", .x))
    if ('participant-birthdate' %in% names(df_dates)) {
      df_dates <-
        mutate(
          df_dates,
          `session-date` = lubridate::ymd(`session-date`),
          `participant-birthdate` = lubridate::ymd(`participant-birthdate`)
        )
      df_dates <-
        mutate(df_dates, age_days = `session-date` - `participant-birthdate`)
      df$age_days <- df_dates$age_days
    } else {
      df$age_days <- NA
    }
  } else {
    if (vb)
      message(' Data frame lacking data needed to calculate ages.')
  }
  df
}

#-------------------------------------------------------------------------------
extract_participant_ids <- function(df) {
  these_cols <- colnames(df)
  str_extract(these_cols, '([0-9]+)') |> unique()
}

#-------------------------------------------------------------------------------
extract_particip_info <- function(df) {
  select(df, contains('participant'))
}

#-------------------------------------------------------------------------------
extract_particip_info_session_date <- function(df) {
  select(df, contains(c('participant', 'session-date')))
}

#-------------------------------------------------------------------------------
remove_materials <- function(df) {
  dplyr::filter(df,!str_detect(df$`session-date`, '[Mm]aterials'))
}
