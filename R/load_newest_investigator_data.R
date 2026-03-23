load_newest_investigator_data <- function(save_path = "private",
                                          vb = FALSE){
  assertthat::is.string(save_path)
  assertthat::is.readable(save_path)
  assertthat::assert_that(length(save_path) == 1)
  
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  fns <- list.files(save_path, pattern = "investigators-", full.names = TRUE)
  
  # Newest last
  fn <- fns[length(fns)]
  x <- assertthat::is.readable(fn)
  
  readr::read_csv(fn, show_col_types = FALSE)  
}

