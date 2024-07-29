#' Update Databrary Data And Render Report
#' 
#' @param src_dir A character string. The directory for the source files for the report.
#' @param open_rp A logical value. Open the rendered report in a browser.
#' @param rpt_URL A character string. The local path to the rendered report.
report_update_render <- function(src_dir = 'src',
                                 open_rpt = TRUE,
                                 rpt_URL = 'docs/index.html') {
  
  assertthat::is.string(src_dir)
  assertthat::is.readable(src_dir)
  assertthat::assert_that(is.logical(open_rpt))
  assertthat::is.string(rpt_URL)
  
  suppressPackageStartupMessages(require(targets))
  suppressPackageStartupMessages(require(bookdown))
  
  start_time <- Sys.time()
  message("\n-------Updating data-------")
  targets::tar_make()
  
  message("\n-------Rendering report-------")
  bookdown::render_book(src_dir)
  
  message("\n------- Report rendered in ", Sys.time() - start_time)
  
  if (open_rpt)
    browseURL(rpt_URL)
}

