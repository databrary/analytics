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
