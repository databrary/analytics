# _targets.R

library(targets)
library(tarchetypes)

source("R/report_functions.R")
source("R/volume_asset_functions.R")
source("R/institution_investigator_functions.R")
source("R/constants.R")

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

list(
  #----------------------------------------------------------------------------
  # Login Databrary
  tar_target(
    databrary_login_status,
    databraryr::login_db(Sys.getenv("DATABRARY_LOGIN"), store = TRUE),
    cue = tar_cue(mode = "always")
  ),
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
    age = as.difftime(6, units = "days")
  ),
  tar_target(
    inst_invest_csv,
    format = "file",
    update_inst_invest_csv(inst_invest_df, paste0(here::here(), '/src/csv'))
  ),
  #----------------------------------------------------------------------------
  # Volume tags and keywords
  # tarchetypes::tar_age(
  #   volume_tags_df,
  #   refresh_volume_tags_df(1:max_vol_id),
  #   age = as.difftime(4, units = "weeks")
  # ),
  # tar_target(vol_tags_csv,
  #            format = "file",
  #            update_vol_tags_csv(volume_tags_df, "src/csv")
  # ),
  #----------------------------------------------------------------------------
  # Funders
  tar_target(volume_funders_df,
             refresh_volume_funders_df(1:max_vol_id)),
  tar_target(
    volume_funders_csv,
    update_volume_funders_csv(volume_funders_df, paste0(here::here(), '/src/csv'))
  ),
  #----------------------------------------------------------------------------
  # Volume assets
  tarchetypes::tar_age(
    volume_asset_stats_csvs,
    update_all_vol_stats(max_vol_id,
                         db_login_status = databrary_login_status),
    format = "file",
    age = as.difftime(4, units = "weeks")
  ),
  tarchetypes::tar_age(
    volume_asset_csv_list,
    generate_volume_asset_csv_list(paste0(here::here(), '/src/csv')),
    age = as.difftime(4, units = "weeks")
  ),
  tarchetypes::tar_age(
    volume_asset_stats_df,
    make_volume_assets_stats_df(volume_asset_csv_list),
    age = as.difftime(4, units = "weeks")
  ),
  #----------------------------------------------------------------------------
  # Volume demographics from spreadsheets
  tar_target(volume_owners_csv,
             get_all_owners_save_csvs(max_vol_id)),
  tar_target(
    volume_ss_csvs,
    get_volume_demo_save_csv_mult(1, max_vol_id,
                                  db_login_status = databrary_login_status)
  ),
  tar_target(
    volume_ss_csv_fl,
    list.files(paste0(here::here(), '/src/csv'), "[0-9]+\\-sess\\-materials\\.csv", full.names = TRUE),
    cue = tar_cue(mode = "always")
  ),
  tar_target(volume_demog_df,
             create_complete_demog_df(volume_ss_csv_fl)),
  #----------------------------------------------------------------------------
  # Institutions and investigators (detailed)
  #
  # Every 6 days, we create a new inst_df from the saved CSVs, just to keep it fresh
  # Every 6 days, we create a new invest_df from the saved CSVs, just to keep it fresh
  # Every 2 weeks, we update the institution CSVs starting with the max party_id
  # Every 4 weeks, we update _all_ of the institution CSVs to capture changes
  # Every 4 weeks, we update the investigators at all of the institutions and
  #  save it as a CSV 'src/csv/all-ais.csv'.
  tarchetypes::tar_age(
    update_all_inst_csvs,
    get_save_many_inst_csvs(
      1,
      max_party_id,
      update_geo = FALSE,
      db_login_status = databrary_login_status
    ),
    format = "file",
    age = as.difftime(4, units = "weeks")
  ),
  tarchetypes::tar_age(
    name = add_new_inst_csvs,
    # Starts with the max current party_id stored locally or 1 (if no files)
    command = get_save_many_inst_csvs(
      max(extract_inst_csv_id(), 1),
      max_party_id,
      update_geo = FALSE,
      db_login_status = databrary_login_status
    ),
    format = "file",
    age = as.difftime(2, units = "weeks")
  ),
  tarchetypes::tar_age(inst_df,
                       make_inst_df_from_csvs(),
                       age = as.difftime(6, units = "days")),
  tarchetypes::tar_age(
    invest_df,
    readr::read_csv(paste0(here::here(), '/src/csv/all-ais.csv'), show_col_types = FALSE),
    age = as.difftime(6, units = "days")
  ),
  tarchetypes::tar_age(
    make_all_ais_csv,
    update_invest_csv(inst_df),
    format = "file",
    age = as.difftime(4, units = "weeks")
  ),
  #-----------------------------------------------------------------------------
  # Volume-level sessions
  tar_target(
    vols_sess_df,
    get_many_volumes_data(1, max_vol_id,
                          db_login_status = databrary_login_status)
  )
)
