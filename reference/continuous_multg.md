# Bivariate analysis for more than 2 groups

Generates a HTML table of bivariate analysis for 2 groups.

## Usage

``` r
continuous_multg(data, groupvar, flextableformat = TRUE)
```

## Arguments

- data:

  Data frame from which variables will be extracted.

- groupvar:

  Grouping variable. Must have exactly 2 levels.

- flextableformat:

  Logical operator to indicate the output desired. Default is TRUE. When
  FALSE, function will return a dataframe format.

## Value

A dataframe or flextable containing pvalues for each test along with the
normality and homocedasticity tests p values. An extra column will be
shown indicating the recommended significant test

## Examples

``` r
data <- iris

data$Species<-as.factor(data$Species)

continuous_multg(data = data, groupvar = "Species", flextableformat = FALSE)
#>       Variable P_Shapiro_Resid P_Levene P_ANOVA    P_KW Significant_Test
#> 1 Sepal.Length         0.21886  0.00226    <NA> <0.001*   Kruskal-Wallis
#> 2  Sepal.Width         0.32304  0.55552 <0.001* <0.001*            ANOVA
#> 3 Petal.Length         0.03676  <0.001* <0.001* <0.001*   Kruskal-Wallis
#> 4  Petal.Width         0.00387  <0.001* <0.001* <0.001*   Kruskal-Wallis
```
