#' Add time spent in seconds on webpage
#' @description Derive the time spend on a website from the timestamps
#' @param wt webtrack data object
#' @param reset numeric. If duration is greater than this value, it is reset to zero, assuming a new browsing session has started
#' @importFrom data.table is.data.table shift
#' @return webtrack data.table with the same columns as wt and a new column called duration
#' @export
add_duration <- function(wt, reset = 3600){
  stopifnot("wt is not an wt_dt object"=is.wt_dt(wt))
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
#' @param wt webtrack data object
#' @importFrom data.table is.data.table
#' @return webtrack data.table with the same columns as wt and a new column called domain
#' @export
extract_domain <- function(wt){
  stopifnot("wt is not an wt_dt object"=is.wt_dt(wt))
  vars_exist(wt,vars = "url")
  wt[,domain:=urltools::domain(url)]
  wt[]
}

#' Aggregate duration of consecutive visits to a website
#' @param wt webtrack data object
#' @param keep logical. if intermediary columns should be kept or not. defaults to FALSE
#' @importFrom data.table is.data.table shift .N
#' @return webtrack data.table with the same columns as wt with updated duration
#' @export
aggregate_duration <- function(wt, keep = FALSE){
  # trick to avoid NOTES from R CMD check:
  . = .N =  NULL
  stopifnot("wt is not an wt_dt object"=is.wt_dt(wt))
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

#' Classify domains according to prespecified classes
#' @param wt webtrack data object
#' @param domain_classes a data.table containing a column "domain" and "type". If NULL, an internal list is used
#' @param preprocess_newsportals logical. add suffix "NEWS" to domains which are classified as portals. If TRUE there needs to be a domain type "newsportals"
#' @param return.only if not null, only return the specified domain type
#' @return webtrack data.table with the same columns as wt and a new column called type. If newsportals are processed, found newsportals have an added "/NEWS" in the domain column. If return.only is used, only rows that contain a specific domain type are returned
#' @export
classify_domains <- function(wt,domain_classes = NULL, preprocess_newsportals = FALSE, return.only = NULL){
  # trick to avoid NOTES from R CMD check:
  i.type = NULL

  stopifnot("wt is not an wt_dt object"=is.wt_dt(wt))
  vars_exist(wt,vars = c("url","domain"))

  if(is.null(domain_classes)){
    domain_classes <- webtrackR::domain_list
  }

  if(!data.table::is.data.table(domain_classes)){
    stop("domain_classes needs to be a data.table")
  }

  if(!all(c("domain","type")%in%names(domain_classes))){
    stop("domain_classes must contain columns called 'domain' and 'type'")
  }

  if(!is.null(return.only)){
    stopifnot("return.only not found in domain_classes"=!return.only%in%domain_classes[["type"]])
  }

  if(preprocess_newsportals){
    if(!"newsportals"%in%domain_classes[["type"]]){
      warning("newsportals type is missing in domain_classes. No preprocessing done")
    } else{
      doms <- paste(domain_classes[["domain"]][domain_classes[["type"]]=="newsportals"], collapse = "|")
      nportal_idx <- grepl(doms,url,perl = TRUE)
      wt[, domain := data.table::fifelse(nportal_idx, paste0(domain, "/NEWS"), domain)]
    }
  }
  wt[domain_classes, on = 'domain', type := i.type]
  wt[is.na(type),type:="other"]

  if(preprocess_newsportals){
    wt[nportal_idx,type:="news"]
  }
  if(!is.null(return.only)){
    wt <- wt[type==return.only]
  }
  wt[]
}

#' Create an urldummy variable from a data.table object
#' @param wt webtrack data object
#' @param dummy a vector of urls that should be dummy coded
#' @param name name of dummy variable to create.
#' @return webtrack object with the same columns and a new column called "name" including the dummy variable
#' @export
#'
create_urldummy <- function(wt,dummy,name){
  stopifnot("wt is not an wt_dt object"=is.wt_dt(wt))
  vars_exist(wt,vars = c("url"))
  wt[,dummy:=data.table::fifelse(url%in%dummy, TRUE, FALSE)]
  data.table::setnames(wt,"dummy",name)
  wt[]
}
