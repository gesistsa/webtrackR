# Isolation Index

Given two groups (A and B) of individuals, the isolation index captures
the extent to which group A disproportionately visit websites whose
other visitors are also members of group A.

## Usage

``` r
isolation_index(grp_a, grp_b, adjusted = FALSE)
```

## Arguments

- grp_a:

  vector (usually corresponds to a column in a webtrack data frame)
  indicating the number of individuals of group A using a website

- grp_b:

  vector (usually corresponds to a column in a webtrack data frame)
  indicating the number of individuals of group B using a website

- adjusted:

  logical. should the index be adjusted (defaults to FALSE)

## Value

numeric value between 0 and 1. 0 indicates no isolation and 1 perfect
isolation

## Details

a value of 1 indicates that the websites visited by group A and group B
do not overlap. A value of 0 means both visit exactly the same websites

## References

Cutler, David M., Edward L. Glaeser, and Jacob L. Vigdor. "The rise and
decline of the American ghetto." Journal of political economy 107.3
(1999): 455-506. Gentzkow, Matthew, and Jesse M. Shapiro. "Ideological
segregation online and offline." The Quarterly Journal of Economics
126.4 (2011): 1799-1839.

## Examples

``` r
# perfect isolation
grp_a <- c(5, 5, 0, 0)
grp_b <- c(0, 0, 5, 5)
isolation_index(grp_a, grp_b)
#> [1] 1

# perfect overlap
grp_a <- c(5, 5, 5, 5)
grp_b <- c(5, 5, 5, 5)
isolation_index(grp_a, grp_b)
#> [1] 0
```
