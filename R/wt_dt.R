#' An S3 class, based on [data.table], to store webtrack data
#' @details
#' A `wt_dt` table is a [data.table].
#' Therefore, it can be used by any function that would work on a [data.frame] or a [data.table].
#' Most of the operation such as variable creation, subsetting and joins are inherited from the [data.table]
#' `[]` operator, following the convention `DT[i,j,by]` (see data table package for detail).
#' @name wt_dt
#' @seealso
#' * [data.table] -- on which `wt_dt` is based
NULL

# Construction ------------------------------------------------------------

#' Convert a data.frame containing webtrack data to a wt_dt object
#' @rdname wt_dt
#' @param x data.frame containing the needed set of variables (panelist_id,url
#' and timestamp) but not necessarily named as such.
#' @param timestamp_format string. Specify the raw timestamp's formatting. Defaults to "%Y-%m-%d %H:%M:%OS".
#' @param varnames named vector of variable names that contain information "panelist_id", "url", "timestamp".
#' @return a webtrack data object
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' is.wt_dt(wt)
#' @export
as.wt_dt <- function(x, timestamp_format = "%Y-%m-%d %H:%M:%OS",
                     varnames = c(panelist_id = "panelist_id", url = "url", timestamp = "timestamp")) {
  standard_vars <- c("panelist_id", "url", "timestamp")
  if (!inherits(x, "data.frame")) {
    stop("x must be a data.frame comparable object (tibble,data.table,..)")
  }
  if (!all(names(varnames) %in% standard_vars)) {
    stop("varnames need to include mappings for 'panelist_id', 'url', and 'timestamp'")
  }
  if (!all(standard_vars %in% names(varnames))) {
    idx <- which(!standard_vars %in% names(varnames))
    named_standard_vars <- standard_vars
    names(named_standard_vars) <- standard_vars
    varnames <- c(varnames, named_standard_vars[idx])
  }

  if (!data.table::is.data.table(x)) {
    x <- data.table::as.data.table(x)
  }
  vars_exist(x, vars = varnames)
  x[, varnames[["timestamp"]] := as.POSIXct(get(varnames[["timestamp"]]), format = timestamp_format)]

  data.table::setnames(x, unname(varnames), names(varnames))
  data.table::setorder(x, panelist_id, timestamp)
  class(x) <- c("wt_dt", class(x))
  x
}

#' @rdname wt_dt
#' @return logical. TRUE if x is a webtrack data object and FALSE otherwise
#' @export
is.wt_dt <- function(x) {
  data.table::is.data.table(x) & "wt_dt" %in% class(x)
}

# printing ------
#' @keywords internal
print_wt_dt <- function(x, ...) {
  print_txt <- utils::capture.output(print(tibble::as_tibble(x), ...))
  print_txt[1] <- sub("A tibble", "webtrack data", print_txt[1])
  cat(print_txt, sep = "\n")
  invisible(x)
}

#' Print web tracking data
#' @param x object of class wt_dt
#' @param ... additional parameters for print
#' @return No return value, called for side effects
#' @export
print.wt_dt <- function(x, ...) {
  print_wt_dt(x, ...)
}

#' Summary function for webtrack data
#' @param object object of class wt_dt
#' @param ... additional parameters for summary
#' @return No return value, called for side effects
#' @export
summary.wt_dt <- function(object, ...) {
  tick <- "\u2714"
  cross <- "\u2716"
  symbols <- c(cross, tick)
  no_panelist <- length(unique(object[["panelist_id"]]))
  time_window <- range(object[["timestamp"]])
  idx <- (!is.na(pmatch(c("duration", "domain", "type"), names(object)))) + 1

  # printing
  cat("\n ==== Overview ====\n\n")
  cat("panelists: ", no_panelist, "\n")
  cat("time window: ", as.character(time_window[1]), " - ", as.character(time_window[2]), "\n")
  cat("\n")
  cat("[", symbols[idx[1]], "] duration\n", sep = "")
  cat("[", symbols[idx[2]], "] domain\n", sep = "")
  cat("[", symbols[idx[3]], "] type\n", sep = "")
  cat("\n")
  if (!is.null(attr(object, "dummy"))) {
    urldummys <- attr(object, "dummy")
    cat("urldummy variables:", paste(urldummys, collapse = ","))
  }
  if (!is.null(attr(object, "panelist"))) {
    panelist <- attr(object, "panelist")
    cat("panelist variables:", paste(panelist, collapse = ","))
  }
  cat("\n")
  cat("\n ====== DATA ======\n\n")
  NextMethod(object, ...)
}
