storage_gb_by_vol <-
  function(df,
           deposit_fee_figshare_gb = FIGSHARE_TB/1000,
           deposit_fee_dryad_gb = DRYAD_TB/1000) {
    df_new <- df |>
      dplyr::group_by(vol_id) |>
      dplyr::summarise(n_tot_files = sum(n_files),
                       tot_gb = sum(tot_size_gb)) |>
      dplyr::mutate(
        figshare_fee = tot_gb * deposit_fee_figshare_gb,
        dryad_fee = tot_gb * deposit_fee_dryad_gb
      )
    df_new
  }
