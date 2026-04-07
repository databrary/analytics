# _targets.R

library(targets)
library(tarchetypes)
tar_source()
tar_option_set(packages = c("readr", "dplyr", "databraryr", "purr", "assertthat",
                            "kableExtra", "lubridate"))

list(
  targets::tar_target(name = institution_list,
                      command = export_institution_list(),
                      format = "file"),
  targets::tar_target(name = export_all_institution_investigators,
                      command = export_all_institution_investigators()),
  targets::tar_target(name = investigator_data, 
             command = update_investigator_data(refresh_data = TRUE),
             format = "file")
)