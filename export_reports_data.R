# Gather data for reports
fl <- list.files(file.path(here::here(), "R"), "*.R", 
                 full.names = TRUE)
purrr::map(fl, suppressPackageStartupMessages(source)) |>
  purrr::quietly()

# Login to Databrary
logged_in <- databraryr::login_db(email = Sys.getenv("DATABRARY_LOGIN"),
                          password = Sys.getenv("DATABRARY_PASSWORD"),
                          client_id = Sys.getenv("DATABRARY_CLIENT_ID"),
                          client_secret = Sys.getenv("DATABRARY_CLIENT_SECRET"),
                          store = FALSE,
                          overwrite = FALSE)

if (logged_in) {
  start_upload <- proc.time()
  
  message("Exporting institution list.")
  export_institution_list()
  message("Exporting institution investigators.")
  export_all_institution_investigators()
  message("Exporting sharing data.")
  export_all_institutions_sharing_data()
  
  t <- proc.time() - start_upload
  message("Processing time data: ", t)
} else {
  message("Unable to log in to Databrary.")
}

databraryr::logout_db()
