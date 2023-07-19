# analytics

Analyses of Databrary usage, downloads, & data.

## Installation and usage

- Clone the repo.
- From the project root directory, source `R/starting_fresh_functions.R` at the R console via `source("R/starting_fresh_functions.R")`.
- Execute `setup_anew("<my_email@my_inst.edu>")` from the R console, substituting your Databrary login email for `<my_email@my_inst.edu>`. **Note**: This generates local copies of various CSV files needed for the 
system-wide reports. The process can take 1.5-2 hours.
- Execute `bookdown::render_book('src')` to render the reports.
