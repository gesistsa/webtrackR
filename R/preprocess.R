#' Add time spend on webpage
#' @description Derive the time spend on a website from the timestamps
#' @param wt webtrack data as data.table object
#' @importFrom data.table is.data.table shift
#' @return data.table with the same columns as wt and a new column called duration
#' @export
add_duration <- function(wt){
  stopifnot(is.data.table(wt))
  vars_exist(wt,vars = c("panelist_id","timestamp"))
  wt[,duration:=as.numeric(shift(timestamp, n = 1, type = "lead", fill = 0)-timestamp),by="panelist_id"]
  wt
}

#' Extract domain from url
#' @description Extracts the domain and subdomain from the urls
#' @param wt data.table object containing tracking data
#' @importFrom data.table is.data.table
#' @return data.table with the same columns as wt and a new column called domain
#' @export
extract_domain <- function(wt){
  stopifnot(is.data.table(wt))
  vars_exist(wt,vars = "url")
  wt[,domain:=urltools::domain(url)]
  wt
}

