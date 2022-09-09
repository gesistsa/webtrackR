#' Add time spent in seconds on webpage
#' @description Derive the time spend on a website from the timestamps
#' @param wt webtrack data as data.table object
#' @param reset numeric. If duration is greater than this value, it is reset to zero, assuming a new browsing session has started
#' @importFrom data.table is.data.table shift
#' @return data.table with the same columns as wt and a new column called duration
#' @export
add_duration <- function(wt, reset = 3600){
  stopifnot(is.data.table(wt))
  vars_exist(wt,vars = c("panelist_id","timestamp"))
  wt[,duration:=as.numeric(shift(timestamp, n = 1, type = "lead", fill = NA)-timestamp),by="panelist_id"]
  # TODO: how to handle last visited page (seems to be set to zero in existing datasets)
  wt[is.na(duration),duration:=0]
  # TODO: does this make sense?
  wt[duration>reset,duration:=0]
  wt[]
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
  wt[]
}

#' Aggregate duration of consecutive visits to a website
#' @param wt data.table object containing tracking data
#' @param keep logical. if intermediary columns should be kept or not. defaults to FALSE
#' @importFrom data.table is.data.table shift .N
#' @return data.table with the same columns as wt with updated duration
#' @export
aggregate_duration <- function(wt, keep = FALSE){
  # trick to avoid NOTES from R CMD check:
  . = .N =  NULL
  stopifnot(is.data.table(wt))
  vars_exist(wt,vars = c("url","panelist_id","timestamp","domain"))
  grp_vars <- setdiff(names(wt),c("duration","timestamp"))
  wt[, visit := cumsum(url != shift(url, n = 1, type = "lag", fill = 0)), by = "panelist_id"]
  wt[, day := as.Date(timestamp)]
  wt <- wt[,
           .(visits = .N,
             duration = sum(as.numeric(duration), na.rm = TRUE),
             timestamp = min(timestamp)),
           by = eval(unique(c("visit", "day",grp_vars)))]

  if(!keep){
    wt[,c("visit","visits","day") := NULL]
  }
  wt[]
}
