# numberstrings (development version)

-   Initial CRAN submission.
-   This new package is a spin-off from [scrutiny](https://lhdjung.github.io/scrutiny/). Much of the code was copied from there; see [lhdjung/scrutiny#65](https://github.com/lhdjung/scrutiny/issues/65) on Github.
-   Notably, however, `scrutiny::split_by_parens()` was not included here. It was replaced by `separate_wider_parens()`, which is faster and more consistent with tidyr's `separate_wider_*()` functions. Here is how it differs from `split_by_parens()`:
    -   The `sep` and `check_sep` arguments were renamed to `delim` and `check_delim`, consistent with `tidyr::separate_wider_delim()`.
    -   Columns to be split are no longer selected automatically. You need to specify them explicitly using the `cols` argument. If you want the same column selection as with `split_by_parens()`' default, use `cols = everything()`. Note that `check_delim` also influences column selection, just like `check_sep` in `split_by_parens()`.
    -   Edge cases are handled more gracefully. For example, any characters after the closing delimiter are stored in a new column. The name of this column is controlled by the new `name_after` argument. By contrast, `split_by_parens()` would simply ignore characters after the closing delimiter.
    -   TODO: SAY HOW MUCH FASTER THE NEW FUNCTION IS COMPARED TO THE OLD ONE, BUT FIRST, ADD MISSING FUNCTIONALITY TO ENABLE A FAIR COMPARISON!
    -   Some new arguments are passed down to `separate_wider_regex()`.
    -   By default, the new `trim` argument removes any spaces around the delimiters.
    -   Internally, `separate_wider_parens()` uses `tidyr::separate_wider_regex()` as its workhorse instead of `dplyr::mutate()` with `across()`.
