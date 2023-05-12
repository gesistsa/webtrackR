#' Add time spent in seconds on webpage
#' @description Derive the time spend on a website from the timestamps
#' @param wt webtrack data object
#' @param reset numeric. If duration is greater than this value, it is reset to zero, assuming a new browsing session has started
#' @importFrom data.table is.data.table shift
#' @return webtrack data.table with the same columns as wt and a new column called duration
#' @examples
#' data("test_data")
#' wt <- as.wt_dt(test_data)
#' wt <- add_duration(wt)
#' @export
add_duration <- function(wt, reset = 3600){
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt,vars = c("panelist_id","timestamp"))
  wt[,duration:=as.numeric(shift(timestamp, n = 1, type = "lead", fill = NA)-timestamp),by="panelist_id"]
  # TODO: how to handle last visited page (seems to be set to zero in existing datasets)
  wt[is.na(duration),duration:=0]
  # TODO: does this make sense?
  wt[duration>reset,duration:=0]
  wt[]
}

#' Extract host from url
#' @description Extracts the host from urls. The host is defined as the part between the scheme (e.g., "https://") and the subdirectory.
#' @param wt webtrack data object
#' @importFrom data.table is.data.table
#' @return webtrack data.table with the same columns as wt and a new column called domain
#' @examples
#' data("test_data")
#' wt <- as.wt_dt(test_data)
#' wt <- extract_domain(wt)
#' @export
extract_host <- function(wt){
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt,vars = "url")
  wt[,tmp:=urltools::domain(gsub("@","%40",url))]
  wt[,domain:=urltools::domain(tmp)]
  wt[,tmp:=NULL]
  wt[]
}

#' Extract domain from url
#' @description Extracts the domain from urls. We define the domain (e.g., "google.com") as the sum of the suffix (e.g., ".com") and the part before that and the preceding dot (e.g., "google" in "https://mail.google.com).
#' @param wt webtrack data object
#' @importFrom data.table is.data.table
#' @return webtrack data.table with the same columns as wt and a new column called domain
#' @examples
#' data("test_data")
#' wt <- as.wt_dt(test_data)
#' wt <- extract_domain(wt)
#' @export
extract_domain <- function(wt){
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt,vars = "url")
  wt[,host:=urltools::domain(gsub("@","%40",url))]
  wt[,suffix:=urltools::suffix_extract(host)[["suffix"]]]
  wt[,domain_name:=urltools::suffix_extract(host)[["domain"]]]
  wt[,domain:=ifelse((!is.na(domain_name) & !is.na(suffix)), paste0(domain_name, ".", suffix), NA)]
  wt[,host:=NULL]
  wt[,suffix:=NULL]
  wt[,domain_name:=NULL]
  wt[]
}

#' Aggregate duration of consecutive visits to a website
#' @param wt webtrack data object
#' @param keep logical. if intermediary columns should be kept or not. defaults to FALSE
#' @importFrom data.table is.data.table shift .N
#' @return webtrack data.table with the same columns as wt with updated duration
#' @examples
#' data("test_data")
#' wt <- as.wt_dt(test_data)
#' wt <- add_duration(wt)
#' wt <- extract_domain(wt)
#' # the following step can take longer
#' wt <- wt[1:100,]
#' aggregate_duration(wt)
#' @export
aggregate_duration <- function(wt, keep = FALSE){
  . = .N =  NULL #revisit
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
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
#' @param prev_type logical. If TRUE (default) the type of the domain visited before the current visit is added
#' @param preprocess_newsportals logical. add suffix "NEWS" to domains which are classified as portals. If TRUE there needs to be a domain type "newsportals"
#' @param return.only if not null, only return the specified domain type
#' @return webtrack data.table with the same columns as wt and a new column called type. If prev_type is TRUE, a column prev_type is added with the type of the visit before the current one. If newsportals are processed, found newsportals have an added "/NEWS" in the domain column. If return.only is used, only rows that contain a specific domain type are returned
#' @examples
#' data("test_data")
#' wt <- as.wt_dt(test_data)
#' wt <- extract_domain(wt)
#' wt <- add_duration(wt)
#' wt <- classify_domains(wt)
#' @export
classify_domains <- function(wt,
                             domain_classes = NULL,
                             prev_type = TRUE,
                             preprocess_newsportals = FALSE,
                             return.only = NULL){

  i.type = NULL #revisit

  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
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

  if(isTRUE(preprocess_newsportals)){
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
  if(isTRUE(prev_type)){
    wt[,day:=as.Date(timestamp)]
    wt[,prev_type:=data.table::shift(type,n = 1L, fill = "other"),by=c("panelist_id","day")]
    wt[,prev_type:=data.table::fifelse(data.table::shift(duration,n = 1L, fill = 5000)>3600,"direct",prev_type)]
    wt[,day:=NULL]
  }
  wt[]
}

#' Create an urldummy variable from a data.table object
#' @param wt webtrack data object
#' @param dummy a vector of urls that should be dummy coded
#' @param name name of dummy variable to create.
#' @return webtrack object with the same columns and a new column called "name" including the dummy variable
#' @examples
#' data("test_data")
#' wt <- as.wt_dt(test_data)
#' wt <- extract_domain(wt)
#' code_urls <- c("Ccj4QELzbJe6.com/FrKrkvugBVJWwfSobV")
#' create_urldummy(wt,dummy = code_urls, name = "test_dummy")
#' @export
create_urldummy <- function(wt,dummy,name){
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt,vars = c("url"))
  wt[,dummy:=data.table::fifelse(url%in%dummy, TRUE, FALSE)]
  data.table::setnames(wt,"dummy",name)
  data.table::setattr(wt,"dummy",c(attr(wt,"dummy"),name))
  wt[]
}

#' Add panelist features to webtrack data
#' Add characteristics of panelists (e.g. from a survey) to the webtrack data
#' @param wt webtrack data object
#' @param data a data.table (or object that can be converted to data.table) which contains variables of panelists
#' @param cols character vector of columns to add. If NULL, all columns are added
#' @return webtrack object with the same columns and joined with panelist survey data
#' @examples
#' data("test_data")
#' data("test_survey")
#' wt <- as.wt_dt(test_data)
#' add_panelist_data(wt,test_survey)
#' @export
add_panelist_data <- function(wt,data,cols = NULL){
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt,vars = c("panelist_id"))
  if(!data.table::is.data.table(data)){
    data <- data.table::as.data.table(data)
  }
  vars_exist(data,vars = c("panelist_id"))
  if(!is.null(cols)){
    if(!all(cols%in%names(data))){
      stop("couldn't locate all cols in data")
    }
    data <- data[,c("panelist_id",cols), with = FALSE]
    data.table::setattr(wt,"panelist",cols)
  } else{
    data.table::setattr(wt,"panelist",setdiff(names(data),"panelist_id"))
  }

  data[wt, on = "panelist_id"]
}
