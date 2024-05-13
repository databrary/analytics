# _targets.R

library(targets)
library(tarchetypes)

# source("R/report_functions.R")
# source("R/volume_asset_functions.R")
# source("R/institution_investigator_functions.R")
# source("R/constants.R")
# Source files
list.files("R", "\\.R$", full.names = TRUE) |>
  purrr::walk(source, echo = FALSE, print.eval = FALSE)

tar_option_set(
  packages = c(
    "readr",
    "dplyr",
    "ggplot2",
    "purrr",
    "stringr",
    "databraryr",
    "broom",
    "knitr",
    "rmarkdown",
    "cowplot",
    "lubridate"
  )
)

# Create default httr2 request
drq <- databraryr::make_default_request()

# Parameters for debugging
verbose_feedback <- FALSE
n_time <- 5
time_units <- "days" # Typically days or weeks

list(
  #----------------------------------------------------------------------------
  # Login Databrary
  # tar_target(
  #   databrary_login_status,
  #   databraryr::login_db(
  #     Sys.getenv("DATABRARY_LOGIN"),
  #     store = TRUE,
  #     rq = drq
  #   ),
  #   cue = tar_cue(mode = "always")
  # ),
  #----------------------------------------------------------------------------
  # Max party and volume ids
  tar_target(max_ids,
             update_max_vol_party_ids(),
             cue = tar_cue(mode = "always")),
  tar_target(max_party_id,
             max_ids$MAX_PARTY_ID),
  tar_target(max_vol_id,
             max_ids$MAX_VOL_ID),
  #----------------------------------------------------------------------------
  # institution and investigator aggregate numbers
  tarchetypes::tar_age(
    inst_invest_df,
    update_inst_invest_df(paste0(here::here(), '/src/csv')),
    age = as.difftime(n_time, units = time_units)
  ),
  tar_target(
    inst_invest_csv,
    format = "file",
    update_inst_invest_csv(inst_invest_df, paste0(here::here(), '/src/csv'),
                           vb = verbose_feedback)
  ),
  #----------------------------------------------------------------------------
  # Volume tags and keywords
  tarchetypes::tar_age(
    volume_tags_df,
    refresh_volume_tags_df(1:max_vol_id),
    age = as.difftime(4, units = "weeks")
  ),
  tar_target(vol_tags_csv,
             update_vol_tags_csv(volume_tags_df, "src/csv")
  ),
  #----------------------------------------------------------------------------
  # Funders
  tar_target(
    volume_funders_df,
    refresh_volume_funders_df(1:max_vol_id, vb = verbose_feedback)
  ),
  tar_target(
    volume_funders_csv,
    update_volume_funders_csv(volume_funders_df,
                              paste0(here::here(), '/src/csv'),
                              vb = verbose_feedback)
  ),
  #----------------------------------------------------------------------------
  # Volume assets
  tarchetypes::tar_age(
    volume_asset_stats_csvs,
    update_all_vol_stats(max_vol_id, vb = verbose_feedback),
    format = "file",
    age = as.difftime(n_time, units = time_units)
  ),
  tarchetypes::tar_age(
    volume_asset_csv_list,
    generate_volume_asset_csv_list(paste0(here::here(), '/src/csv')),
    age = as.difftime(n_time, units = time_units)
  ),
  tarchetypes::tar_age(
    volume_asset_stats_df,
    make_volume_assets_stats_df(volume_asset_csv_list),
    age = as.difftime(n_time, units = time_units)
  ),
  #----------------------------------------------------------------------------
  # Volume demographics from spreadsheets
  tarchetypes::tar_age(
    volume_owners_csv,
    get_all_owners_save_csvs(max_vol_id, vb = verbose_feedback,
                             rq = drq),
    age = as.difftime(n_time, units = time_units)
  ),
  tarchetypes::tar_age(
    volume_ss_csvs,
    get_volume_demo_save_csv_mult(1, max_vol_id, vb = verbose_feedback,
                                  rq = drq),
    age = as.difftime(n_time, units = time_units)
  ),
  tar_target(
    volume_ss_csv_fl,
    list.files(
      paste0(here::here(), '/src/csv'),
      "[0-9]+\\-sess\\-materials\\.csv",
      full.names = TRUE
    ),
    cue = tar_cue(mode = "always")
  ),
  # tar_target(volume_demog_df,
  #            create_complete_demog_df(volume_ss_csv_fl)),
  # #----------------------------------------------------------------------------
  # Institutions and investigators (detailed)
  tarchetypes::tar_age(inst_df,
                       make_inst_df_from_csvs(),
                       age = as.difftime(n_time, units = time_units)),
  # tarchetypes::tar_age(
  #   invest_df,
  #   readr::read_csv(paste0(here::here(), '/src/csv/all-ais.csv'),
  #                   show_col_types = FALSE),
  #   age = as.difftime(n_time, units = time_units)
  # ),
  tarchetypes::tar_age(
    name = add_new_inst_csvs,
    # Starts with the max current party_id stored locally or 1 (if no files)
    command = get_save_many_inst_csvs(
      max(extract_inst_csv_id(), 1),
      max_party_id,
      update_geo = TRUE,
      rq = drq
    ),
    age = as.difftime(n_time, units = time_units)
  ),
  # Regenerate/update all institutions every 4 weeks
  tarchetypes::tar_age(
    update_all_inst_csvs,
    get_save_many_inst_csvs(
      1,
      max_party_id,
      update_geo = TRUE,
      vb = verbose_feedback,
      rq = drq
    ),
    age = as.difftime(4, units = "weeks")
  ),
  tarchetypes::tar_age(
    make_all_ais_csv,
    update_invest_csv(inst_df, vb = verbose_feedback, rq = drq),
    format = "file",
    age = as.difftime(n_time, units = time_units)
  ),
  #-----------------------------------------------------------------------------
  # Volume-level sessions
  tar_target(
    vols_sess_df,
    get_many_volumes_data(1, max_vol_id, vb = verbose_feedback,
                          rq = drq)
  )
)
