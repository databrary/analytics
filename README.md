# Databrary analytics

Analyses of Databrary usage.

## Installation and usage

- Clone the repo.
- When you open the project, the `renv` package dependency manager is likely to install and run.
- Run `renv::restore()` and agree to install package dependencies that are suggested.
- Save parameters in your user account's `.Renviron` file:
  - `DB_LOGIN_ACCT`: Your Databrary account (email address)
  - `DB_LOGIN_PW`: Your Databrary password
  - `DB_API_CLIENT_ID`: The API client ID you have created.
  - `DB_API_CLIENT_SECRET`: The API client secret provided by Databrary when you are given API access permission.
- Run `quarto render src` in a terminal. 
Note that this will fail most of the time for most users because default behavior relies on unshared CSV data files.
