#' Isolation Index
#' @description Given two groups (A and B) of individuals, the isolation index captures the
#' extent to which group A disproportionately visit websites whose other visitors
#' are also members of group A.
#' @param grp_a  vector (usually corresponds to a column in a webtrack
#' data frame) indicating the number of individuals of group A using a website
#' @param grp_b  vector (usually corresponds to a column in a webtrack
#' data frame) indicating the number of individuals of group B using a website
#' @param adjusted logical. should the index be adjusted (defaults to FALSE)
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
isolation_index <- function(grp_a, grp_b, adjusted = FALSE) {
    if (length(grp_a) != length(grp_b)) {
        stop("grp_a and grp_b need to have the same length")
    }
    if (isFALSE(adjusted)) {
        grp_a <- grp_a / sum(grp_a, na.rm = TRUE)
        grp_b <- grp_b / sum(grp_b, na.rm = TRUE)
        right_share <- grp_b / (grp_a + grp_b)
        out <- (grp_b - grp_a) * right_share
        return(sum(out, na.rm = TRUE))
    } else {
        out <- 1 / sum(grp_a, na.rm = TRUE) * grp_a * (grp_a - 1) / (grp_a + grp_b - 1)
        out <- out - 1 / sum(grp_b, na.rm = TRUE) * (grp_b * grp_a) / (grp_a + grp_b - 1)
        return(sum(out, na.rm = TRUE))
    }
}

# Will be incorporated later
# Cutler, David M., Edward L. Glaeser, and Jacob L. Vigdor. "The rise and decline of the American ghetto." Journal of political economy 107.3 (1999): 455-506.
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
# Frankel, David, and Oscar Volij. "Scale Invariant Measures of Segregation"Working Paper, 2008.
# atkinson <- function(left,right){
#   if(length(left)!=length(right)){
#     stop("left and right need to have the same length")
#   }
#   left  <- (left/sum(left,na.rm = TRUE))^(1/2)
#   right <- (right/sum(right,na.rm = TRUE))^(1/2)
#   bilanz <- left * right
#   1 - sum(bilanz, na.rm = TRUE)
# }
