# Symmetric Atkinson Index calculates the symmetric Atkinson index

Symmetric Atkinson Index calculates the symmetric Atkinson index

## Usage

``` r
atkinson_index(grp_a, grp_b)
```

## Arguments

- grp_a:

  vector (usually corresponds to a column in a webtrack data frame)
  indicating the number of individuals of group A using a website

- grp_b:

  vector (usually corresponds to a column in a webtrack data frame)
  indicating the number of individuals of group B using a website

## References

Frankel, David, and Oscar Volij. "Scale Invariant Measures of
Segregation "Working Paper, 2008.

## Examples

``` r
# perfect score
grp_a <- c(5, 5, 0, 0)
grp_b <- c(0, 0, 5, 5)
atkinson_index(grp_a, grp_b)
#> [1] 1

grp_a <- c(5, 5, 5, 5)
grp_b <- c(5, 5, 5, 5)
atkinson_index(grp_a, grp_b)
#> [1] 0
```
