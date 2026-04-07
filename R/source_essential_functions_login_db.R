source_essential_functions_login_db <- function() {
  # Source functions
  fl <- list.files(file.path(here::here(), "R"), "*.R", 
                   full.names = TRUE)
  p <- purrr::map(fl, suppressPackageStartupMessages(source)) |>
    purrr::quietly()
  
  # Login to Databrary
  x <- databraryr::login_db(email = Sys.getenv("USERNAME"),
                            password = Sys.getenv("PASSWORD"),
                            client_id = Sys.getenv("CLIENT_ID"),
                            client_secret = Sys.getenv("CLIENT_SECRET"),
                            store = FALSE,
                            overwrite = FALSE)
  
}