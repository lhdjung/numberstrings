
#' Split a string into columns by parentheses or similar
#'
#' @param data
#' @param cols
#' @param delim What to split by. Either `"parens"`, `"brackets"`, or
#'   `"braces"`; or a length-2 vector of custom delimiters (see Examples).
#'   Default is `"parens"`.
#' @param end1,end2 Name endings of the new columns before and inside the
#'   parenthesis-like characters. Defaults are `"x"` and `"sd"`, for strings
#'   with the pattern `"x (sd)"`.
#' @param names_sep Optionally, choose a delimiter between the original column
#'   names and `end1` / `end2`. Default is `NULL`.

# @param names_repair

#' @param name_after Name of the column that captures the
#'   part of the string after the closing `delim` element, if there is such a
#'   part. Change it to avoid name conflicts, in particular. Default is
#'   `"after"`.
#' @param too_few
#' @param trim Should whitespace around the delimiters be
#'   removed? Default is `TRUE`.
#'
#' @inheritParams tidyr::separate_wider_regex
#'
#' @return
#' @export

#' @seealso [tidyr::separate_wider_regex()], the workhorse behind this function.
#'   Indeed, `separate_wider_parens()` was designed for consistency with tidyr's
#'   `separate_wider_*()` functions because their use cases are very similar.
#'
#' @examples
separate_wider_parens <- function(data,
                                  cols,
                                  delim = "parens",
                                  ...,
                                  end1 = "x",
                                  end2 = "sd",
                                  names_sep = NULL,
                                  names_repair = "check_unique",
                                  name_after = "after",
                                  too_few = c("error", "debug", "align_start"),
                                  check_delim = TRUE,
                                  cols_remove = TRUE,
                                  trim = TRUE,
                                  transform = FALSE) {

  # Capturing `cols` here and unquoting it later using `!!` instead of `{{`
  # leads to a more informative error message if the user fails to specify it.
  cols <- rlang::enexpr(cols)

  # Check whether a certain column name might get duplicated later:
  if (any(colnames(data) == name_after)) {
    cli::cli_abort(c(
      "Potential column name conflict: \"{name_after}\".",
      "x" = "A new \"{name_after}\" column might be added \\
      via `name_after`, but there already is a column by that name.",
      "i" = "Solution: specify `name_after` using a new name."
    ))
  }

  # By default, take care that only those columns that contain the `delim`
  # elements will be operated on. Unlike in `scrutiny::split_by_parens()`,
  # running the code block that creates `names_of_cols_with_seps` is conditional
  # on `check_delim` here because the current function only uses this variable
  # to create the `selection2` expression. `split_by_parens()` uses it for other
  # purposes, as well.
  if (check_delim) {
    # Determine which columns have suitable values with regards to the `delim`
    # elements and capture their names:
    names_of_cols_with_seps <- data |>
      dplyr::select(
        function(x) {
          sep_in_order <- translate_length1_sep_keywords(delim)
          sep_in_order <- paste0(sep_in_order[1L], "[^)]*", sep_in_order[2L])
          x |>
            stringr::str_detect(sep_in_order) |>
            all()
        }
      ) |>
      colnames()
    selection2 <- rlang::expr(all_of(names_of_cols_with_seps))
  } else {
    selection2 <- rlang::expr(dplyr::everything())
  }

  # Compute a named integer vector of index locations (within `data`) of the
  # columns that will be split:
  cols_to_select <- rlang::expr(!!cols & !!selection2)
  cols_to_select <- tidyselect::eval_select(cols_to_select, data)

  # By default, spaces around the delimiters are removed (by making
  # `tidyr::separate_wider_regex()` match them as unnamed components):
  spaces <- if (trim) {
    "\\s*"
  } else {
    NULL
  }

  # Define regex patterns to capture the substrings before and inside the
  # parenthesis-like delimiters, with the pattern being "before (inside)":
  before <- "^(?:.*?)"
  inside <- "(?:.*?)"
  names(before) <- end1
  names(inside) <- end2

  after_sep_close <- "(?:.*?)$"
  names(after_sep_close) <- name_after

  # The crucial part -- separate the strings that make up the columns selected
  # via `cols` into two columns each, using the regex pattern assembled via
  # `delim`, `end1`, `end2`, `name_after`, and `trim`:
  out <- tidyr::separate_wider_regex(
    data = data,
    cols = all_of(unname(cols_to_select)),
    patterns = c(
      before, # = "^(?:.*?)",
      spaces,
      sep_open,
      spaces,
      inside, # = "(?:.*?)",
      spaces,
      sep_close,
      spaces,
      after_sep_close
    ),
    ...,
    names_sep = names_sep,
    names_repair = names_repair,
    too_few = too_few,
    cols_remove = cols_remove
  )

  # The output is meant to have the same class as the input. Coerce `out` to a
  # tibble if it isn't a tibble, but `data` is:
  if (tibble::is_tibble(data) && !tibble::is_tibble(out)) {
    out <- tibble::as_tibble(out)
  }

  # Delete the column after the closing delimiter if it exists and contains
  # nothing but empty strings:
  if (any(colnames(out) == name_after) && all(out[[name_after]] == "")) {
    out[[name_after]] <- NULL
  }

  # Without a special transformation, nothing is left to do except for returning
  # the results. This is different from `scrutiny::split_by_parens()`, where
  # `out` is only the result of splitting the selected columns, so that the
  # other columns must be re-attached to it later. The current solution is more
  # principled and straightforward, thanks to `tidyr::separate_wider_regex()`.
  if (!transform) {
    return(out)
  }

  # Transforming the data frame into a longer shape only makes sense if the
  # output contains nothing but the separated columns:
  if (!cols_remove) {
    cli::cli_abort(c(
      "`transform` can't be `TRUE` if `cols_remove` is `FALSE`.",
      "x" = "The output can't be transformed into a longer shape if \\
      it contains any columns besides those that were separated."
    ))
  }

}


# # Test with:
# tibble::tibble(x = "0.09 (alpha) beta")
# # Or with:
# tibble::tibble(x = "0.09 [alpha] beta")
#
# df_pigs <- tibble::tibble(
#   id = 1:3,
#   flight = c("0.12 (4.31)", "2.95 (2.47)", "4.01 (6.43)")
# )

