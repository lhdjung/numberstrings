
# Helpers (not exported) --------------------------------------------------

check_length_parens_sep <- function(sep) {
  if (!any(length(sep) == c(1L, 2L))) {
    cli::cli_abort(c(
      "!" = "`sep` must have length 1 or 2.",
      "x" = "It has length {length(sep)}: {wrap_in_backticks(sep)}."
    ))
  }
}



#' Match the `delim` keyword to actual separators
#'
#' @description `translate_delim_to_length2_seps()` is called within
#'   `separate_wider_parens()` to replace the legal keywords `"parens"`,
#'   `"brackets"`, or `"braces"` to the characters they describe.
#'
#'   A length-2 `delim` object is returned as it is because it is meant to
#'   contain actual (custom) separators, not a keyword. If `delim` is neither
#'   length 2 nor any of the keywords from above, an error is thrown.
#'
#' @param delim String (length 1 or 2).
#'
#' @return String (length 1 or 2).
#'
#' @noRd
translate_delim_to_length2_seps <- function(delim) {
  check_length_parens_sep(delim)
  if (length(delim) == 2L) {
    delim
  } else if (any(delim == c("parens", "(", "\\("))) {
    c("\\(", "\\)")
  } else if (any(delim == c("brackets", "[", "\\["))) {
    c("\\[", "\\]")
  } else if (any(delim == c("braces", "{", "\\{"))) {
    c("\\{", "\\}")
  } else {
    cli::cli_abort(c(
      "!" = "`delim` must be either \"parens\", \"brackets\", or \\
        \"braces\"; or \"(\", \"[\", or \"{{\".",
      "x" = "It was given as {wrap_in_quotes_or_backticks(delim)}.",
      "i" = "Alternatively, choose two custom separators; e.g., \\
        `delim = c(\"<\", \">\")` for strings such as \"2.65 <0.27>\"."
    ))
  }
}


# Warning thrown within tidyselect-supporting functions:
warn_wrong_columns_selected <- function(names_wrong_cols,
                                        msg_exclusion, msg_reason,
                                        msg_it_they = c("It doesn't", "They don't")) {
  if (length(names_wrong_cols) == 1L) {
    msg_col_cols <- "1 column"
    msg_it_they <- msg_it_they[1L]
    msg_exclusion <- msg_exclusion[1L]
  } else {
    msg_col_cols <- paste0(length(names_wrong_cols), " columns")
    msg_it_they <- msg_it_they[max(1, length(msg_it_they))]
    msg_exclusion <- msg_exclusion[max(1, length(msg_exclusion))]
  }
  names_wrong_cols <- wrap_in_backticks(names_wrong_cols)
  cli::cli_warn(c(
    "!" = "{msg_col_cols} {msg_exclusion}: {names_wrong_cols}.",
    "x" = "{msg_it_they} {msg_reason}."
  ))
}


proto_split_parens <- function(string, sep = "parens") {

  if (length(sep) == 2L) {
    sep_open  <- sep[1L]
    sep_close <- sep[2L]
  } else {
    separators <- translate_delim_to_length2_seps(sep)
    sep_open   <- separators[1L]
    sep_close  <- separators[2L]
  }

  out <- stringr::str_split(string, sep_open)
  out <- unlist(out, use.names = FALSE)
  out <- sub(paste0(sep_close, ".*"), "", out)

  divisor <- length(out) / length(string)
  split(out, ceiling(seq_along(out) / divisor))
}



# Main functions ----------------------------------------------------------

#' Extract substrings from before and inside parentheses
#'
#' @description `before_parens()` and `inside_parens()` extract substrings from
#'   before or inside parentheses, or similar separators like brackets or curly
#'   braces.
#'
#'   See [`split_by_parens()`] to split some or all columns in a data frame into
#'   both parts.
#'
#' @param string Vector of strings with parentheses or similar.
#' @param sep String. What to split by. Either `"parens"`, `"brackets"`,
#'   `"braces"`, or a length-2 vector of custom separators. See examples for
#'   [`split_by_parens()`]. Default is `"parens"`.
#'
#' @export
#'
#' @return String vector of the same length as `string`. The part of `string`
#'   before or inside `sep`, respectively.
#'
#' @name parens-extractors
#'
#' @examples
#' x <- c(
#'   "3.72 (0.95)",
#'   "5.86 (2.75)",
#'   "3.06 (6.48)"
#' )
#'
#' before_parens(string = x)
#'
#' inside_parens(string = x)


before_parens <- function(string, sep = "parens") {
  check_length_parens_sep(sep)
  out <- proto_split_parens(string, sep)
  out <- vapply(out, function(x) x[1L], character(1L), USE.NAMES = FALSE)
  stringr::str_trim(out)
}


#' @rdname parens-extractors
#' @export

inside_parens <- function(string, sep = "parens") {

  check_length_parens_sep(sep)

  if (length(sep) == 2L) {
    sep_close <- sep[2L]
  } else {
    if (any(sep == c("parens", "(", "\\("))) {
      sep_close <- "\\)"
    } else if (any(sep == c("brackets", "[", "\\["))) {
      sep_close <- "\\]"
    } else if (any(sep == c("braces", "{", "\\{"))) {
      sep_close <- "\\}"
    }
  }

  out <- proto_split_parens(string, sep)
  out <- vapply(out, function(x) x[2L], "", USE.NAMES = FALSE)
  sub(paste0(sep_close, ".*"), "", out)
}

