#' Isolation Index
#' @description Given two groups (A and B) of individuals, the isolation index captures the
#' extent to which group A disproportionately visit websites whose other visitors
#' are also members of group A.
#' @param grp_A  vector (usually corresponds to a column in a webtrack
#' data frame) indicating the number of individuals of group A using a website
#' @param grp_B  vector (usually corresponds to a column in a webtrack
#' data frame) indicating the number of individuals of group B using a website
#' @details a value of 1 indicates that the websites visited by group A and group B do not overlap.
#' A value of 0 means both visit exactly the same websites
#' @return numeric value between 0 and 1. 0 indicates no isolation and 1 perfect isolation
#' @references
#' Cutler, David M., Edward L. Glaeser, and Jacob L. Vigdor. "The rise and decline of the American ghetto." Journal of political economy 107.3 (1999): 455-506.
#' Gentzkow, Matthew, and Jesse M. Shapiro. "Ideological segregation online and offline." The Quarterly Journal of Economics 126.4 (2011): 1799-1839.
#' @examples
#' # perfect isolation
#' left <- c(5, 5, 0, 0)
#' right <- c(0, 0, 5, 5)
#' isolation_index(left, right)
#'
#' # perfect overlap
#' left <- c(5, 5, 5, 5)
#' right <- c(5, 5, 5, 5)
#' isolation_index(left, right)
#' @export
isolation_index <- function(grp_A, grp_B) {
    if (length(grp_A) != length(grp_B)) {
        stop("grp_A and grp_B need to have the same length")
    }
    grp_A <- grp_A / sum(grp_A, na.rm = TRUE)
    grp_B <- grp_B / sum(grp_B, na.rm = TRUE)
    right_share <- grp_B / (grp_A + grp_B)
    bilanz <- (grp_B - grp_A) * right_share
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
