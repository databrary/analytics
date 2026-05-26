# Gather data for reports
fl <- list.files(file.path(here::here(), "R"), "*.R", 
                 full.names = TRUE)
purrr::map(fl, suppressPackageStartupMessages(source)) |>
  purrr::quietly()

# Login to Databrary
logged_in <- databraryr::login_db(email = Sys.getenv("USERNAME"),
                          password = Sys.getenv("PASSWORD"),
                          client_id = Sys.getenv("CLIENT_ID"),
                          client_secret = Sys.getenv("CLIENT_SECRET"),
                          store = FALSE,
                          overwrite = FALSE)

if (logged_in) {
  message("Exporting institution list.")
  export_institution_list()
  message("Exporting institution investigators.")
  export_all_institution_investigators()
  message("Exporting sharing data.")
  export_all_institutions_sharing_data()  
} else {
  message("Unable to log in to Databrary.")
}

databraryr::logout_db()
