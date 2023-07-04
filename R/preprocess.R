#' Add time spent on a visit in seconds
#' @description Approximate the time spent on a visit based on the sequence of timestamps
#' @param wt webtrack data object
#' @param cutoff numeric. If duration is greater than this value, it is reset to na, the cutoff, or a user-defined value. Defaults to 5 minutes (= 300 seconds).
#' @param replace_by boolean. This determines whether differences greater than the cutoff, as well as the last visit for an individual, are set to na (default), the cutoff, or a user-defined value
#' @param replace_val numeric. If replace_by is set to "value", this argument determines what value differences greater than the cutoff, as well as the last visit for an individual are set to.
#' @importFrom data.table is.data.table shift
#' @return webtrack data.table with the same columns as wt and a new column called duration
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_duration(wt)
#' # Defining cutoff at 10 minutes and setting critical visits to the cutoff:
#' wt <- add_duration(wt, cutoff = 600, replace_by = "cutoff")
#' # Defining cutoff at 10 minutes and setting critical visits to 5 minutes:
#' wt <- add_duration(wt, cutoff = 600, replace_by = "value", replace_val = 300)
#' @export
add_duration <- function(wt, cutoff = 300, replace_by = "na", replace_val = NULL){
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  stopifnot("replace_val must be NULL or numeric" = (is.null(replace_val) | is.numeric(replace_val)))
  if (replace_by == "value") {
    stopifnot("if replace_by is set to 'value' replace_val must not be null" = is.numeric(replace_val))
  }
  vars_exist(wt,vars = c("panelist_id","timestamp"))
  wt[,duration:=as.numeric(shift(timestamp, n = 1, type = "lead", fill = NA)-timestamp),by="panelist_id"]
  if (replace_by == "na") {
    wt[is.na(duration),duration:=NA]
    wt[duration>cutoff,duration:=NA]
  }
  else if (replace_by == "cutoff") {
    wt[is.na(duration),duration:=cutoff]
    wt[duration>cutoff,duration:=cutoff]
  }
  else if (replace_by == "value") {
    wt[is.na(duration),duration:=replace_val]
    wt[duration>cutoff,duration:=replace_val]
  }
  wt[]
}

#' Create a session variable
#' @description Define sessions of browsing depending on aq time cutoff for inactivity
#' @param wt webtrack data object
#' @param cutoff numeric. If the consecutive visit happens later than this value (in seconds), a new browsing session is defined
#' @importFrom data.table is.data.table shift
#' @return webtrack data.table with the same columns as wt and a new column called duration
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_session(wt,cutoff = 120)
#' @export
add_session <- function(wt, cutoff){
  stopifnot("cutoff argument is missing" = !missing(cutoff))
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt,vars = c("panelist_id","timestamp"))
  wt[as.numeric(shift(timestamp, n = 1, type = "lead", fill = NA)-timestamp) > cutoff,session:=1:.N,by="panelist_id"]
  data.table::setnafill(wt, type = "nocb", cols = "session")
  wt[]
}

#' Extract host from url
#' @description Extracts the host from urls. The host is defined as the part between the scheme (e.g., "https://") and the subdirectory.
#' @param wt webtrack data object
#' @importFrom data.table is.data.table
#' @return webtrack data.table with the same columns as wt and a new column called domain
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- extract_host(wt)
#' @export
extract_host <- function(wt){
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt,vars = "url")
  wt[,tmp:=urltools::domain(gsub("@","%40",url))]
  wt[,host:=urltools::domain(tmp)]
  wt[,tmp:=NULL]
  wt[]
}

#' Extract domain from url
#' @description Extracts the domain from urls. We define the domain (e.g., "google.com") as the sum of the suffix (e.g., ".com") and the part before that and the preceding dot (e.g., "google" in "https://mail.google.com).
#' @param wt webtrack data object
#' @importFrom data.table is.data.table
#' @return webtrack data.table with the same columns as wt and a new column called domain
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
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

#' Add the next visit as a variable
#' @description Adds the next visit as a variable, either as the full url, the extracted host or the extracted domain
#' @param wt webtrack data object
#' @param level character. Either "url", "host" or "domain". Defaults to "url".
#' @importFrom data.table is.data.table shift
#' @return webtrack data.table with the same columns as wt and a new column called url_next, host_next or domain_next.
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_next_visit(wt, level = "url")
#' wt <- add_next_visit(wt, level = "host")
#' wt <- add_next_visit(wt, level = "domain")
#' @export
add_next_visit <- function(wt, level = "url"){
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt,vars = c("panelist_id","timestamp"))
  if (level == "url") {
    wt[,url_next:=shift(url, n = 1, type = "lead", fill = NA),by="panelist_id"]
  } else if (level == "host") {
    wt <- extract_host(wt)
    wt[,host_next:=shift(host, n = 1, type = "lead", fill = NA),by="panelist_id"]
  } else if (level == "domain") {
    wt <- extract_domain(wt)
    wt[,domain_next:=shift(domain, n = 1, type = "lead", fill = NA),by="panelist_id"]
  }
  wt[]
}

#' Add the previous visit as a variable
#' @description Adds the previous visit as a variable, either as the full url, the extracted host or the extracted domain
#' @param wt webtrack data object
#' @param level character. Either "url", "host" or "domain". Defaults to "url".
#' @importFrom data.table is.data.table shift
#' @return webtrack data.table with the same columns as wt and a new column called url_previous, host_previous or domain_previous.
#' @examples
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
#' wt <- add_previous_visit(wt, level = "url")
#' wt <- add_previous_visit(wt, level = "host")
#' wt <- add_previous_visit(wt, level = "domain")
#' @export
add_previous_visit <- function(wt, level = "url"){
  stopifnot("input is not a wt_dt object" = is.wt_dt(wt))
  vars_exist(wt,vars = c("panelist_id","timestamp"))
  if (level == "url") {
    wt[,url_previous:=shift(url, n = 1, type = "lag", fill = NA),by="panelist_id"]
  } else if (level == "host") {
    wt <- extract_host(wt)
    wt[,host_previous:=shift(host, n = 1, type = "lag", fill = NA),by="panelist_id"]
  } else if (level == "domain") {
    wt <- extract_domain(wt)
    wt[,domain_previous:=shift(domain, n = 1, type = "lag", fill = NA),by="panelist_id"]
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
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
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
#' data("testdt_tracking")
#' wt <- as.wt_dt(testdt_tracking)
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
#' @param join_on which columns to join on. Defaults to "panelist_id".
#' @return webtrack object with the same columns and joined with panelist survey data
#' @examples
#' data("testdt_tracking")
#' data("testdt_survey_w")
#' wt <- as.wt_dt(testdt_tracking)
#' add_panelist_data(wt,testdt_survey_w)
#' @export
add_panelist_data <- function(wt,data,cols = NULL,join_on = "panelist_id"){
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
  test <- data[wt, on = join_on]
}
