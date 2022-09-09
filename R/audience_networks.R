#' Create incidence matrix for audience-outlet network
#' @details The incidence matrix is a matrix A with entries  `A[i,j]=1` if panelist i visited outlet j at least once.
#' @param wt webtrack data object
#' @param cutoff visits below this cutoff will not be considered as a visit
#' @return incidence audience-outlet network
#' @seealso to create audience networks see [audience_network]
#' @export
audience_incidence <- function(wt,cutoff = 3){
  # trick to avoid NOTES from R CMD check:
  .N = 0
  if(!requireNamespace("igraph", quietly = TRUE)){
    stop("The package 'igraph' is needed for this function.")
  }
  stopifnot("wt is not an wt_dt object"=is.wt_dt(wt))
  vars_exist(wt,vars = c("panelist_id","domain"))

  el <- wt[duration >= cutoff, c("panelist_id", "domain")]
  el <- el[, .N, by = c("panelist_id", "domain")]
  # g <- netUtils::bipartite_from_data_frame(el[, c("panelist_id", "domain")], "panelist_id", "domain")
  g <- igraph::graph_from_data_frame(el, directed = FALSE)
  igraph::V(g)$type <- !igraph::bipartite.mapping(g)$type
  A <- igraph::as_incidence_matrix(g)
}

#' Create audience networks
#' @description audience network
#' @param wt webtrack data object
#' @param cutoff visits below this cutoff will not be considered as a visit
#' @param type one of "pmi", "phi", "disparity", "sdsm, "or "fdsm". See details
#' @param alpha significance level
#' @return audience network as igraph object
#' @export
audience_network <- function(wt, cutoff = 3, type = "pmi", alpha = 0.05){
  A <- audience_incidence(wt, cutoff = cutoff)
  type <- match.arg(type,c("pmi", "phi", "disparity", "sdsm", "fdsm"))

  switch(type,
         pmi = pmi(A,t = alpha),
         phi = phi(A,p = alpha),
         disparity = disparity1(A, p = alpha),
         sdsm = sdsm1(A, p = alpha),
         fdsm = fdsm1(A, p = alpha)
  )

}

# network extraction methods ----
# TODO: this needs to be double checked
pmi <- function(A, t = 0) {
  reach <- rowSums(A) / ncol(A)
  exp_mat <- outer(reach, reach, "*")
  W <- (A %*% t(A)) / ncol(A)
  B <- log(W / exp_mat) > t
  igraph::graph_from_adjacency_matrix(B, mode = "undirected", diag = FALSE)
}

# TODO: this needs to be double checked
phi <- function(A, p = 0.05) {
  if(!requireNamespace("stats", quietly = TRUE)){
    stop("The package 'stats' is needed for this function.")
  }
  D <- (A %*% t(A))
  R <- rowSums(A)
  N <- ncol(A)
  RR <- suppressWarnings(outer(R, R, "*"))
  Phi <- (D * N - RR) / sqrt(diag(N - R) %*% RR %*% diag(N - R))
  tmat <- Phi * suppressWarnings(sqrt(outer(R, R, "pmax")) - 2) / sqrt(1 - Phi^2)
  # we cannot use the standard values for big N because we do not have big N
  pmat <- outer(R, R, function(x, y) stats::qt(p = p / 2, df = pmax(x, y), lower.tail = FALSE))
  igraph::graph_from_adjacency_matrix(tmat > pmat, mode = "undirected", diag = FALSE)
}

disparity1 <- function(A, p = 0.05) {
  W <- A %*% t(A)
  diag(W) <- 0
  suppressMessages(backbone::disparity(W, class = "igraph", alpha = p))
}

sdsm1 <- function(A, p = 0.05) {
  if(!requireNamespace("backbone", quietly = TRUE)){
    stop("The package 'backbone' is needed for this function.")
  }
  suppressMessages(backbone::sdsm(A, class = "igraph", alpha = p))
}

fdsm1 <- function(A, p = 0.05) {
  if(!requireNamespace("backbone", quietly = TRUE)){
    stop("The package 'backbone' is needed for this function.")
  }
  suppressMessages(backbone::sdsm(A, class = "igraph", alpha = p))
}
