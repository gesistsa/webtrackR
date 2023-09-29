#' Check if columns are present
#' @description
#' `vars_exist()` checks if columns are present in a webtrack data object.
#' By default, checks whether the data has a `panelist_id`, a `ulr` and a
#' `timestamp` column.#'
#' @param wt webtrack data object.
#' @param vars character vector of variables.
#' Defaults to `c("panelist_id", "url", "timestamp")`.
#' @return A data.table object.
vars_exist <- function(wt, vars = c("panelist_id", "url", "timestamp")) {
    vars_wt <- names(wt)
    idx <- pmatch(vars, vars_wt)
    if (any(is.na(idx))) {
        not_found <- is.na(idx)
        err <- paste0("'", paste0(vars[not_found], collapse = "', '"), "'")
        stop(paste0("couldn't find the column(s) ", err, " in the webtrack data"), call. = FALSE)
    }
    invisible(NULL)
}
