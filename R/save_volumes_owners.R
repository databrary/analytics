save_volumes_owners <- function(df,
                                min_vol_id,
                                max_vol_id,
                                dir = "src/csv",
                                fn_suffix = "-owners.csv",
                                vb = FALSE) {
  stopifnot(is.data.frame(df))
  stopifnot(is.numeric(min_vol_id))
  stopifnot(is.numeric(max_vol_id))
  stopifnot(min_vol_id > 0)
  stopifnot(max_vol_id > 0)
  stopifnot(min_vol_id < max_vol_id)
  stopifnot(is.character(dir))
  stopifnot(dir.exists(dir))
  
  fn <-
    paste0(
      dir,
      "/",
      stringr::str_pad(min_vol_id, 5, pad = "0"),
      "-",
      stringr::str_pad(max_vol_id, 5, pad = "0"),
      fn_suffix
    )
  if (vb)
    message(paste0("Saving volume owner data to ", fn, "."))
  readr::write_csv(df, fn)
}