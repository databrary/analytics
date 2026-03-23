# _targets.R

library(targets)
library(tarchetypes)
tar_source()
tar_option_set(packages = c("readr", "dplyr", "databraryr", "purr", "assertthat",
                            "kableExtra", "lubridate"))

list(
  tar_target(name = investigator_data, 
             command = update_investigator_data(refresh_data = TRUE),
             format = "file")
)