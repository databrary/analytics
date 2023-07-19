# analytics

Analyses of Databrary usage, downloads, & data.

## Installation and usage

- Clone the repo.
- When you open the project, the `renv` package dependency manager is likely to install and run.
- Run `renv::restore()` and agree to install package dependencies that are suggested.
- Then, execute `renv::status()`. If this gives "No issues found -- the project is in a consistent state.", then proceed. If there are package issues, you should resolve them by installing missing packages using "renv::install('package_name')" for each package you want to install.
- From the project root directory, source `R/starting_fresh_functions.R` at the R console via `source("R/starting_fresh_functions.R")`.
- Execute `setup_anew("<my_email@my_inst.edu>")` from the R console, substituting your Databrary login email for `<my_email@my_inst.edu>`. **Note**: This will complete the setup needed for running the analytics reports, including generating local copies of various CSV files needed for the 
system-wide reports. The process can take 1.5-2 hours, so please be patient.
- When `setup_anew()` finishes, the (`bookdown`) rendered report should open in your default browser.
