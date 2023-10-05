#' An S3 class to store web tracking data
#' @details
#' A `wt_dt` table is a data.frame.
#' @name wt_dt
NULL

# Construction ------------------------------------------------------------

#' Convert a data.frame containing web tracking data to a `wt_dt` object
#' @rdname wt_dt
#' @param x data.frame containing a necessary set of columns, namely
#' panelist's ID, visit URL and visit timestamp.
#' @param varnames Named vector of column names, which contain the panelist's ID
#' (`panelist_id`), the visit's URL (`url`) and the visit's timestamp (`timestamp`).
#' @param timestamp_format string. Specifies the raw timestamp's formatting.
#' Defaults to `"%Y-%m-%d %H:%M:%OS"`.
#' @param tz timezone of date. defaults to UTC
#' @return a webtrack data object with at least columns `panelist_id`, `url`
#' and `timestamp`
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' is.wt_dt(wt)
#' @export
as.wt_dt <- function(x, timestamp_format = "%Y-%m-%d %H:%M:%OS", tz = "UTC",
                     varnames = c(panelist_id = "panelist_id", url = "url", timestamp = "timestamp")) {
    standard_vars <- c("panelist_id", "url", "timestamp")
    if (!inherits(x, "data.frame")) {
        stop("x must be a data.frame comparable object (tibble,data.table,..)")
    }
    if (!all(names(varnames) %in% standard_vars)) {
        stop("varnames must include mappings for 'panelist_id', 'url', and 'timestamp'")
    }
    if (!all(standard_vars %in% names(varnames))) {
        idx <- which(!standard_vars %in% names(varnames))
        named_standard_vars <- standard_vars
        names(named_standard_vars) <- standard_vars
        varnames <- c(varnames, named_standard_vars[idx])
    }
    vars_exist(x, vars = varnames)
    x[["timestamp"]] <- as.POSIXct(x[[varnames[["timestamp"]]]], format = timestamp_format, tz = tz)
    names(x)[match(varnames, names(x))] <- names(varnames)
    x <- x[with(x, order(panelist_id, timestamp)), ]
    class(x) <- c("wt_dt", class(x))
    return(x)
}

#' @rdname wt_dt
#' @return logical. TRUE if x is a webtrack data object and FALSE otherwise
#' @export
is.wt_dt <- function(x) {
    "wt_dt" %in% class(x)
}

abort_if_not_wtdt <- function(x) {
    if (isFALSE(is.wt_dt(x))) {
        stop("input is not a wt_dt object", call. = FALSE)
    }
}

# printing ------
#' Print web tracking data
#' @param x object of class wt_dt
#' @param ... additional parameters for print
#' @return No return value, called for side effects
#' @export
print.wt_dt <- function(x, ...) {
    NextMethod(x, ...)
}

#' Summary function for web tracking data
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
