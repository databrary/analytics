update_inst_invest_csv <-
  function(df,
           csv_dir = "src/csv",
           csv_fn = "institutions-investigators.csv",
           vb = FALSE) {
    stopifnot(!rlang::is_empty(df))
    stopifnot(dir.exists(csv_dir))
    stopifnot(file.exists(file.path(csv_dir, csv_fn)))
    
    fn <- file.path(csv_dir, csv_fn)
    readr::write_csv(df, fn)
    fn
  }
