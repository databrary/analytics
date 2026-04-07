# Source functions
fl <- list.files(file.path(here::here(), "R"), "*.R", full.names = TRUE)
p <- purrr::map(fl, suppressPackageStartupMessages(source)) |>
  purrr::quietly()

# Login to Databrary
x <- databraryr::login_db(
  email = Sys.getenv("USERNAME"),
  password = Sys.getenv("PASSWORD"),
  client_id = Sys.getenv("CLIENT_ID"),
  client_secret = Sys.getenv("CLIENT_SECRET"),
  store = FALSE,
  overwrite = FALSE
)

export_institution_list()
export_all_institution_investigators()
export_all_institutions_sharing_data(vb = TRUE)

institution_ids <- list(8, 12, 43)
institution_names <- list(
  "New York University",
  "The Pennsylvania State University: University Park",
  "University of Wisconsin"
)

nyu_params <- list(
  institution_id = 8,
  institution_name = "New York University",
  date = "today"
)

pad_n_zeros <- function(str, n) {
  stringr::str_pad(str, n, pad = "0")
}

make_inst_report_name <- function(inst_id) {
  paste0("inst-", pad_n_zeros(inst_id, 5), "-report.html")
}

quarto::quarto_render(
  "_institution_report.qmd",
  output_file = "inst-00008-report.html")

rmarkdown::render(input = "_institution_report.qmd", 
                  output_dir = "institution_reports", 
                  output_file = "inst-00012-report.html", 
                  params = list(institution_name= "Penn State", 
                                institution_id = 12))

rmarkdown::render(input = "_institution_report.qmd", 
                  output_dir = "institution_reports", 
                  output_file = "inst-00008-report.html", 
                  params = list(institution_name= "New York University", 
                                institution_id = 8))

rmarkdown::render(input = "_institution_report.qmd", 
                  output_dir = "institution_reports", 
                  output_file = "inst-00043-report.html", 
                  params = list(institution_name= "University of Wisconsin", 
                                institution_id = 43))

extract_report_params <- function(fn = "private/inst_invest_shared/inst-00008-shared-vols.csv") {
  id <- stringr::str_extract(string = fn, pattern = "[0-9]{5}")
  inst_metadata <- databraryr::get_institution_by_id(as.numeric(id))
  name <- inst_metadata$name
  list(institution_id = id, institution_name = name)
}

render_inst_report <- function(fn = NULL, vb = FALSE) {
  # Open file, if not empty, then...
  inst_data <- readr::read_csv(fn, show_col_types = FALSE)
  these_params = extract_report_params(fn)
  if (dim(inst_data)[1] > 0) {
    these_params$render_vols_data = "render"
  } else {
    if (vb) message("\nNo volume data for '", these_params$institution_name, "'.")
    these_params$render_vols_data = "no"
  }
  if (vb) message("\nRendering report for '", these_params$institution_name, "'.")
    rmarkdown::render(input = "_institution_report.qmd", 
                      output_dir = "institution_reports",
                      output_file = paste0("inst-", these_params$institution_id, "-report.html"),
                      params = these_params)
}

fl_inst <- list.files("private/inst_invest_shared/", "*.csv$", full.names = TRUE)