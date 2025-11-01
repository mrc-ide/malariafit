#' @title Count leap days between two dates
#' @param date1 Date object
#' @param date2 Date object
n_leap_days <- function(date1, date2){
  dates <- seq(from = date1,to = date2, by = 1)
  return(sum(format(dates, "%m-%d") == "02-29"))
}

#' @title Check whether a date has been entered correctly
#' @param x The date item to check
#' @param arg_name the
to_date <- function(x, arg_name) {
  # Accept Date objects as-is
  if (inherits(x, "Date")) return(x)

  # Try to parse if it's a character
  if (is.character(x)) {
    parsed <- tryCatch(
      as.Date(x, format = "%Y-%m-%d"),
      error = function(e) NA
    )
    if (is.na(parsed)) {
      stop(sprintf(
        "%s is not in a standard unambiguous format. Have you entered it in the form 'YYYY-MM-DD'?",
        arg_name
      ), call. = FALSE)
    }
    return(parsed)
  }

  # Anything else is invalid
  stop(sprintf(
    "%s must be either a Date object or a character string in 'YYYY-MM-DD' format.",
    arg_name
  ), call. = FALSE)
}
