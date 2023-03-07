#' Check if columns are present
#'
#' @description checks if the required columns are present in the webtrack data
#' @param wt webtrack data as data.table object
#' @param vars character vector of variables
#' @return A data.table object
vars_exist <- function(wt,vars = c("panelist_id", "url", "timestamp")){
  vars_wt <- names(wt)
  idx <- pmatch(vars,vars_wt)
  if(any(is.na(idx))){
    not_found <- is.na(idx)
    err <- paste0(vars[not_found],collapse = ",")
    stop(paste0("couldn't find the column(s): ",err, " in the webtrack data"),call. = FALSE)
  }
  invisible(NULL)
}

# Check if a data.table is valid webtrack data
# @param wt webtrack data as data.table object
# @param processed logical. If TRUE, also checks if "duration" and "domain" are present. Otherwise just checks if the standard columns exist
# @param verbose should details be printed or not
# @return logical if wt is valid webtrack data or not
# is_valid_wt <- function(wt, processed = TRUE, verbose = TRUE){
#   wt_vars_opt <- c("panelist_id", "url", "timestamp", "duration", "domain")
#   tick <- "\u2714"
#   cross <- "\u2716"
#
#   if(!processed){
#     wt_vars_opt <- wt_vars_opt[1:3]
#   }
#
#   wt_vars_dat <- names(wt)
#   wt_vars_idx <- pmatch(wt_vars_opt,wt_vars_dat)
#   if(verbose){
#     # TODO: add message here
#   }
#   if(any(is.na(wt_vars_idx))){
#     return(FALSE)
#   } else{
#     return(TRUE)
#   }
# }

