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
#' grp_a <- c(5, 5, 0, 0)
#' grp_b <- c(0, 0, 5, 5)
#' isolation_index(grp_a, grp_b)
#'
#' # perfect overlap
#' grp_a <- c(5, 5, 5, 5)
#' grp_b <- c(5, 5, 5, 5)
#' isolation_index(grp_a, grp_b)
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

#' Dissimilarity Index
#' @description The Dissimilarity Index can be interpreted as the share of Group A
#' visits that would need to be redistributed across media for the share of
#' group A to be uniform across websites.
#' @inheritParams isolation_index
#' @references
#' Cutler, David M., Edward L. Glaeser, and Jacob L. Vigdor. "The rise and decline of the American ghetto." Journal of political economy 107.3 (1999): 455-506.
#' @examples
#' # perfect dissimilarity
#' grp_a <- c(5, 5, 0, 0)
#' grp_b <- c(0, 0, 5, 5)
#' dissimilarity_index(grp_a, grp_b)
#'
#' # no dissimilarity
#' grp_a <- c(5, 5, 5, 5)
#' grp_b <- c(5, 5, 5, 5)
#' dissimilarity_index(grp_a, grp_b)
#' @export
dissimilarity_index <- function(grp_a, grp_b) {
    if (length(grp_a) != length(grp_b)) {
        stop("grp_a and grp_b need to have the same length")
    }
    grp_a <- grp_a / sum(grp_a, na.rm = TRUE)
    grp_b <- grp_b / sum(grp_b, na.rm = TRUE)
    out <- abs(grp_b - grp_a)
    0.5 * sum(out, na.rm = TRUE)
}
#' Symmetric Atkinson Index
#' calculates the symmetric Atkinson index
#' @inheritParams isolation_index
#' @references
#' Frankel, David, and Oscar Volij. "Scale Invariant Measures of Segregation "Working Paper, 2008.
#' @examples
#' # perfect score
#' grp_a <- c(5, 5, 0, 0)
#' grp_b <- c(0, 0, 5, 5)
#' atkinson_index(grp_a, grp_b)
#'
#' grp_a <- c(5, 5, 5, 5)
#' grp_b <- c(5, 5, 5, 5)
#' atkinson_index(grp_a, grp_b)
#' @export
atkinson_index <- function(grp_a, grp_b) {
    if (length(grp_a) != length(grp_b)) {
        stop("grp_a and grp_b need to have the same length")
    }
    grp_a <- (grp_a / sum(grp_a, na.rm = TRUE))^(1 / 2)
    grp_b <- (grp_b / sum(grp_b, na.rm = TRUE))^(1 / 2)
    bilanz <- grp_a * grp_b
    1 - sum(bilanz, na.rm = TRUE)
}
