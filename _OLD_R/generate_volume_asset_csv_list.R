generate_volume_asset_csv_list <- function(csv_dir = "src/csv") {
  stopifnot(is.character(csv_dir))
  stopifnot(dir.exists(csv_dir))
  
  fl <- list.files(csv_dir, '-assets\\.csv', full.names = TRUE)
  if (length(fl) == 0) {
    warning("No volume asset files found in '", csv_dir, "'.")
    NULL
  } else {
    fl
  }
}
