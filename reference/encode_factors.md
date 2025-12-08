# Encode character variables as factor automatically

Encode character variables as factor automatically

## Usage

``` r
encode_factors(
  data,
  encode = c("character", "integer"),
  list_factors = NULL,
  uselist = FALSE
)
```

## Arguments

- data:

  Dataframe to be encoded

- encode:

  Column class to be encoded. Must be "character" or "integer"

- list_factors:

  List of factors to be encoded

- uselist:

  Logical operator to determine if use list of factors or not. If TRUE,
  list_factors argument must be provided.

## Value

Converts listed columns to factors.

## Examples

``` r
df <- data.frame(has = c("Yes", "No", "Yes", "Yes", "No", "No", "Yes"),
smoke = c("Yes", "No", "No", "Yes", "No", "Yes", "No"),
gender = c("Male", "Female", "Male", "Female", "Female", "Male", "Male"))

str(df)
#> 'data.frame':    7 obs. of  3 variables:
#>  $ has   : chr  "Yes" "No" "Yes" "Yes" ...
#>  $ smoke : chr  "Yes" "No" "No" "Yes" ...
#>  $ gender: chr  "Male" "Female" "Male" "Female" ...

df <- encode_factors(df, encode = "character")

str(df)
#> 'data.frame':    7 obs. of  3 variables:
#>  $ has   : Factor w/ 2 levels "No","Yes": 2 1 2 2 1 1 2
#>  $ smoke : Factor w/ 2 levels "No","Yes": 2 1 1 2 1 2 1
#>  $ gender: Factor w/ 2 levels "Female","Male": 2 1 2 1 1 2 2

```
