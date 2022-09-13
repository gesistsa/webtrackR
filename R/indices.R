#' Isolation Index
#' In terms of news exposure, the isolation index captures the extent to which conservatives disproportionately visit outlets whose other visitors are conservative
#' @param left  vector (usually corresponds to a column in a webtrack data.table)
#' @param right vector (usually corresponds to a column in a webtrack data.table)
#' @references
#' Cutler, David M., Edward L. Glaeser, and Jacob L. Vigdor. "The rise and decline of the American ghetto." Journal of political economy 107.3 (1999): 455-506.
#' Gentzkow, Matthew, and Jesse M. Shapiro. "Ideological segregation online and offline." The Quarterly Journal of Economics 126.4 (2011): 1799-1839.
#' @export
isolation_index <- function(left, right){
  if(length(left)!=length(right)){
    stop("left and right need to have the same length")
  }
  left  <- left/sum(left,na.rm = TRUE)
  right <- right/sum(right,na.rm = TRUE)
  right_share <- right/(left + right)
  bilanz <- (right - left) * right_share
  sum(bilanz, na.rm = TRUE)
}

dissimilarity_index <- function(left,right){
  if(length(left)!=length(right)){
    stop("left and right need to have the same length")
  }
  left  <- left/sum(left,na.rm = TRUE)
  right <- right/sum(right,na.rm = TRUE)
  bilanz <- abs(right - left)
  0.5 * sum(bilanz,na.rm = TRUE)
}

atkinson <- function(left,right){
  if(length(left)!=length(right)){
    stop("left and right need to have the same length")
  }
  left  <- (left/sum(left,na.rm = TRUE))^(1/2)
  right <- (right/sum(right,na.rm = TRUE))^(1/2)
  bilanz <- left * right
  1 - sum(bilanz, na.rm = TRUE)
}
