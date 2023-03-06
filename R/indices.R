#' Isolation Index
#' In terms of news exposure, the isolation index captures the extent to which conservatives disproportionately visit outlets whose other visitors are conservative
#' @param left  vector (usually corresponds to a column in a webtrack data.table) indicating the number of left leaning individuals using an outlet
#' @param right vector (usually corresponds to a column in a webtrack data.table) indicating the number of right leaning individuals using an outlet
#' @details a value of 1 indicates that left leaning and right leaning users do not have any outlet overlap. A value of 0 means both use exactly the same outlets
#' @references
#' Cutler, David M., Edward L. Glaeser, and Jacob L. Vigdor. "The rise and decline of the American ghetto." Journal of political economy 107.3 (1999): 455-506.
#' Gentzkow, Matthew, and Jesse M. Shapiro. "Ideological segregation online and offline." The Quarterly Journal of Economics 126.4 (2011): 1799-1839.
#' @examples
#' # perfect isolation
#' left <-  c(5,5,0,0)
#' right <- c(0,0,5,5)
#' isolation_index(left,right)
#'
#' #perfect overlap
#' left <-  c(5,5,5,5)
#' right <- c(5,5,5,5)
#' isolation_index(left,right)
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

# Will be incorporated later
# dissimilarity_index <- function(left,right){
#   if(length(left)!=length(right)){
#     stop("left and right need to have the same length")
#   }
#   left  <- left/sum(left,na.rm = TRUE)
#   right <- right/sum(right,na.rm = TRUE)
#   bilanz <- abs(right - left)
#   0.5 * sum(bilanz,na.rm = TRUE)
# }
#
# atkinson <- function(left,right){
#   if(length(left)!=length(right)){
#     stop("left and right need to have the same length")
#   }
#   left  <- (left/sum(left,na.rm = TRUE))^(1/2)
#   right <- (right/sum(right,na.rm = TRUE))^(1/2)
#   bilanz <- left * right
#   1 - sum(bilanz, na.rm = TRUE)
# }
