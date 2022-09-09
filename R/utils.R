#' Check if columns are present
#'
#' @description checks if the required columns are present in the webtrack data
#' @param wt webtrack data as data.table object
#' @param vars character vector of variables
#' @return A data.table object
#' @export
vars_exist <- function(wt,vars = c("panelist_id", "url", "timestamp")){
  vars_wt <- names(wt)
  idx <- pmatch(vars,vars_wt)
  if(any(is.na(idx))){
    not_found <- is.na(idx)
    err <- paste0(vars[not_found],collapse = ",")
    stop(paste0("couldn't find ",err))
  }
  invisible(NULL)
}
