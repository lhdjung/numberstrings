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



#' Censor left and right
#'
#' `censor()` is used in some of scrutiny's unit tests. The `left` and `right`
#' arguments should only be length 1, although this is not checked.
#'
#' @param x Numeric.
#' @param left Numeric. Lower bound. Any elements of `x` that are less than
#'   `left` will be replaced by `left`.
#' @param right Numeric. Upper bound. Any elements of `x` that are greater than
#'   `right` will be replaced by `right`.
#'
#' @return Numeric vector of length `length(x)`.
#'
#' @noRd
censor <- function(x, left, right) {
  x[x < left] <- left
  x[x > right] <- right
  x
}



#' Check for arguments with or via dots
#'
#' @description `check_new_args_without_dots()` checks a call to
#'   `restore_zeros_df()` for certain kinds of errors
#'   that used to be part of the design of this function, but no longer are:
#'
#'   1. Column names are selected via the dots, `...`.
#'   2. Argument names are prefixed with a dot, like `.check_decimals`.
#'
#'   If any of these cases, a precisely informative error is thrown. There is
#'   also a more generic error if any other argument is passed through the dots,
#'   `...`. This used to be checked within `restore_zeros_df()` itself.

#' @param data Input data frame of the main function itself.
#' @param dots Captures in the main function with `rlang::enquos(...)`.
#' @param old_args String vector with the old, dot-prefixed arguments.
#' @param name_fn String. Name of the main function.
#'
#' @details Error 2 also points the user to the shift from `col*` to `end*` if
#'   `.col1` or `.col2` were specified, much like error 3 does.
#'
#' @return No return value; might throw an error.
#'
#' @noRd
check_new_args_without_dots <- function(data, dots, old_args, name_fn) {

  if (length(dots) == 0L) {
    return(invisible(NULL))
  }

  dots_names <- names(purrr::map(dots, rlang::as_label))

  # Error 1: Column names are selected via the dots, `...`.
  offenders1 <- dots_names[dots_names %in% colnames(data)]
  if (length(offenders1) > 0L) {
    if (length(offenders1) == 1L) {
      msg_cols <- glue::glue("{offenders1}")
    } else {
      msg_cols <- stringr::str_flatten(as.character(offenders1), ", ")
      msg_cols <- paste0("c(", msg_cols, ")")
    }
    cli::cli_abort(c(
      "!" = "`{name_fn}()` no longer uses the dots, `...`, \\
      for column selection.",
      "i" = "Use the `cols` argument instead, like `cols = {msg_cols}`.",
      "*" = "Apologies for the inconvenience."
    ))
  }

  arg_names <- names(rlang::caller_call())

  # Error 2: Argument names are prefixed with a dot, like `.transform`.
  offenders2 <- arg_names[arg_names %in% old_args]
  if (length(offenders2) > 0L) {
    if (length(offenders2) == 1L) {
      msg_was_were <- "was"
      msg_dot_dots <- "a dot"
    } else {
      msg_was_were <- "were"
      msg_dot_dots <- "dots"
    }
    msg_new_args <- stringr::str_remove(offenders2, ".")

    msg_new_args <- wrap_in_backticks(msg_new_args)
    offenders2 <- wrap_in_backticks(offenders2)
    cli::cli_abort(c(
      "!" = "In `{name_fn}()`, {offenders2} {msg_was_were} \\
      renamed to {msg_new_args} (without {msg_dot_dots}).",
      "*" = "Apologies for the inconvenience."
    ))
  }

  # Finally, check that no other arguments are passed through the dots:
  rlang::check_dots_empty(env = rlang::caller_env(n = 1L))
}



#' Count integer places
#'
#' Used in unit testing. Analogous to `decimal_places()`.
#'
#' @param x Numeric (or string that can be coerced to numeric). Object with
#'   integer places to count.
#'
#' @return Integer.
#'
#' @noRd
integer_places <- function(x) {
  x %>%
    stringr::str_trim() %>%
    stringr::str_split_fixed("\\.", n = 2L) %>%
    .[, 1L] %>%
    stringr::str_length()
}



#' Check whether numbers are whole
#'
#' @description For each element of a numeric vector, `is_whole_number()` checks
#'   whether that element is a whole number.
#'
#'   This is not the same as the integer data type, so doubles and integers are
#'   tested the same way. See the note in `?integer`. To test if R itself
#'   considers a vector integer-like, use `rlang::is_integerish()` instead.
#'
#' @param x Numeric.
#'
#' @return Logical vector of the same length as `x`.
#'
#' @noRd
is_whole_number <- function(x, tolerance = .Machine$double.eps^0.5) {
  dplyr::near(x, floor(x), tol = tolerance)
}


