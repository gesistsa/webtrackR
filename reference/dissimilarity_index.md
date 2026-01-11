# Dissimilarity Index

The Dissimilarity Index can be interpreted as the share of Group A
visits that would need to be redistributed across media for the share of
group A to be uniform across websites.

## Usage

``` r
dissimilarity_index(grp_a, grp_b)
```

## Arguments

- grp_a:

  vector (usually corresponds to a column in a webtrack data frame)
  indicating the number of individuals of group A using a website

- grp_b:

  vector (usually corresponds to a column in a webtrack data frame)
  indicating the number of individuals of group B using a website

## References

Cutler, David M., Edward L. Glaeser, and Jacob L. Vigdor. "The rise and
decline of the American ghetto." Journal of political economy 107.3
(1999): 455-506.

## Examples

``` r
# perfect dissimilarity
grp_a <- c(5, 5, 0, 0)
grp_b <- c(0, 0, 5, 5)
dissimilarity_index(grp_a, grp_b)
#> [1] 1

# no dissimilarity
grp_a <- c(5, 5, 5, 5)
grp_b <- c(5, 5, 5, 5)
dissimilarity_index(grp_a, grp_b)
#> [1] 0
```
