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