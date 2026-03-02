update_nsf_funding_csv <- function(max_vol_id = 1779, csv_path = "src/csv") {
  assertthat::assert_that(length(max_vol_id) == 1)
  assertthat::is.number(max_vol_id)
  assertthat::assert_that(max_vol_id > 0)
  
  assertthat::assert_that(length(csv_path) == 1)
  assertthat::is.string(csv_path)
  
  if (!exists("add_clean_nsf_award_id")) {
    return(NULL)
  }
  require(purrr)

  db_w_nsf <-
    purrr::map(1:max_vol_id,
               get_vol_awards,
               funder_str = "NSF",
               .progress = "Databrary vols w/ NSF:") |> 
    purrr::list_rbind() |>
    add_clean_nsf_award_id()
  
  out_fn <- file.path(csv_path, "databrary-vols-w-nsf-funding-all.csv")
  assertthat::is.writeable(out_fn)
  
  readr::write_csv(db_w_nsf, out_fn)
}