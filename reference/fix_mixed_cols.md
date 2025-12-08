# Fix mixed cols in dataframes

Fix mixed cols in dataframes

## Usage

``` r
fix_mixed_cols(df)
```

## Arguments

- df:

  Dataframe to fix

## Value

A dataframe with columns with columns without list

## Examples

``` r
col_1 <- c(1, 3, 4, 5, 5, 2)
col_2 <- c(1, 2, 3, 4, 5, 6)
col_3 <- c("a", "b", "c", "d", "e", "f")


df <- data.frame(col_1, col_2, col_3)

df$col_1 <- as.list(df$col_1) # Reproduce an importing error

str(df)
#> 'data.frame':    6 obs. of  3 variables:
#>  $ col_1:List of 6
#>   ..$ : num 1
#>   ..$ : num 3
#>   ..$ : num 4
#>   ..$ : num 5
#>   ..$ : num 5
#>   ..$ : num 2
#>  $ col_2: num  1 2 3 4 5 6
#>  $ col_3: chr  "a" "b" "c" "d" ...

fixed_df <- fix_mixed_cols(df)
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   col_1 = col_double(),
#>   col_3 = col_character()
#> )
str(fixed_df)
#> 'data.frame':    6 obs. of  3 variables:
#>  $ col_1: num  1 3 4 5 5 2
#>  $ col_2: num  1 2 3 4 5 6
#>  $ col_3: chr  "a" "b" "c" "d" ...

```
