#' An S3 class, based on [data.table], to store webtrack data
#' @details
#' A `wt_dt` table is a [data.table].
#' Therefore, it can be used by any function that would work on a [data.frame] or a [data.table].
#' Most of the operation such as variable creation, subsetting and joins are inherited from the [data.table]
#' `[]` operator, following the convention `DT[i,j,by]` (see data table package for detail).
#' These operations are applied on the data.
#' @name wt_dt
#' @seealso
#' * [data.table] -- on which `wt_dt` is based
NULL

# Construction ------------------------------------------------------------

#' @rdname wt_dt
#' @param x [data.table] containing the correct set of variables
#' @export
as.wt_dt <- function(x){
  stopifnot(is.data.table(x))
  vars_exist(x,vars = c("panelist_id", "url", "timestamp"))
  data.table::setorder(x,panelist_id,timestamp)
  class(x) <- c("wt_dt",class(x))
  x
}

#' @rdname wt_dt
#' @export
is.wt_dt <- function(x){
  data.table::is.data.table(x) & "wt_dt" %in% class(x)
}

# printing ------
#' @keywords internal
print_wt_dt <- function(x, ...) {
  print_txt <- utils::capture.output(print(tibble::as_tibble(x), ...))
  print_txt[1] <- sub('A tibble', 'webtrack data', print_txt[1])
  cat(print_txt, sep = '\n')
  invisible(x)
}

#' Print webtracking data
#' @param x object of class wt_dt
#' @param ... additional parameters for print
#' @export
print.wt_dt <- function(x, ...) {
  print_wt_dt(x, ...)
}

#' Summary function for webtrack data
#' @param object object of class wt_dt
#' @param ... additional parameters for summary
#'@export
summary.wt_dt <- function(object,...){
  tick  <- '\u2714'
  cross <- '\u2716'
  symbols <- c(cross,tick)
  no_panelist <- length(unique(object[["panelist_id"]]))
  time_window <- range(object[["timestamp"]])
  idx <- (!is.na(pmatch(c("duration","domain","type"),names(object))))+1

  #printing
  cat("\n ==== Overview ====\n\n")
  cat("panelists: ",no_panelist,"\n")
  cat("time window: ",as.character(time_window[1])," - ", as.character(time_window[2]),"\n")
  cat("\n")
  cat("[",symbols[idx[1]],"] duration\n",sep="")
  cat("[",symbols[idx[2]],"] domain\n",sep="")
  cat("[",symbols[idx[3]],"] type\n",sep="")
  cat("\n")
  if(!is.null(attr(object,"dummy"))){
    urldummys <- attr(object,"dummy")
    cat("urldummy variables:",paste(urldummys,collapse=","))
  }
  if(!is.null(attr(object,"panelist"))){
    panelist <- attr(object,"panelist")
    cat("panelist variables:",paste(panelist,collapse=","))
  }
  cat("\n")
  cat("\n ====== DATA ======\n\n")
  NextMethod(object,...)
}
