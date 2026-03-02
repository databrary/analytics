list_auth_invest_ids <- function(vb = FALSE,
                                 descreasing = FALSE) {
  assertthat::assert_that(is.logical(vb))
  assertthat::assert_that(length(vb) == 1)
  
  assertthat::assert_that(is.logical(descreasing))
  assertthat::assert_that(length(descreasing) == 1)
  
  investigators <- databraryr::list_users(include_suspended = FALSE,
                            is_authorized_investigator = TRUE)
                            
  if (is.null(investigators)) return(NULL)
  
  investigators$user_id |>
    sort(decreasing = decreasing)
}