#' Wrap into backticks
#'
#' For error messages and similar.
#'
#' @param x String (or coercible to string).
#'
#' @return String of length `length(x)`.
#'
#' @noRd
wrap_in_backticks <- function(x) {
  paste0("`", x, "`")
}



#' Wrap into quotation marks
#'
#' For error messages and similar.
#'
#' @param x String (or coercible to string).
#'
#' @return String of length `length(x)`.
#'
#' @noRd
wrap_in_quotes <- function(x) {
  paste0("\"", x, "\"")
}



#' Wrap into quotation marks if string
#'
#' For error messages and similar. `x` is returned unchanged unless it's a
#' string, in which case it's treated as in `wrap_in_quotes()`.
#'
#' @param x Any object.
#'
#' @return String of length `length(x)`.
#'
#' @noRd
wrap_in_quotes_if_string <- function(x) {
  if (is.character(x)) {
    paste0("\"", x, "\"")
  } else {
    x
  }
}



#' Wrap into quotation marks if string, else in backticks
#'
#' For error messages and similar. Like `wrap_in_quotes_if_string()` except a
#' non-string `x` is wrapped into backticks (rather than being returned
#' unchanged).
#'
#' @param x Any object.
#'
#' @return String of length `length(x)`.
#'
#' @noRd
wrap_in_quotes_or_backticks <- function(x) {
  if (is.character(x)) {
    paste0("\"", x, "\"")
  } else {
    paste0("`", x, "`")
  }
}

