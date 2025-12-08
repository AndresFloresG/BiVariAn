# Sample Size Calculation for multiple regression analysis

Calculates the recommended sample size for a multiple regression
analysis.

## Usage

``` r
ss_multreg(df, prop = NULL, logistic = FALSE, verbose = TRUE)
```

## Arguments

- df:

  Degrees of freedom planned to be introduced

- prop:

  Minimum prevalence of the expected event (Required if planned
  regression is a logistic regression)

- logistic:

  Logical operator to indicate wether the planned regression analysis is
  a logistic regression or not.

- verbose:

  Logical operator to indicate wether the results should be printed in
  console. Default is `TRUE`

## Value

An object class `ss_multreg_obj` indicating the sample size calculation
for a regression analysis.

## References

Peduzzi P, Concato J, Kemper E, Holford TR, Feinstein AR. A simulation
study of the number of events per variable in logistic regression
analysis. Journal of Clinical Epidemiology. diciembre de
1996;49(12):1373–9.

Pierdant-Pérez M, Patiño-López MI, Flores-García JA, Jacques-García FA.
Implementación de un curso virtual de lectura crítica en estudiantes de
medicina durante la pandemia COVID-19. Inv Ed Med. el 1 de octubre de
2023;12(48):64–71.

## Examples

``` r
# Lineal multiple regression with 4 degrees of freedom
ss_multreg(4, logistic = FALSE)
#> 
#> Sample Size Calculation for a multiple linear regression with 4 degrees of freedom
#>  
#> Minimum recommended sample size: 40 
#> Recommended sample size: 60 
#> Maximum recommended sample size: 80 

# Logistic multiple regression with 4 degrees of freedom
# and 60% of probability of the event

ss_multreg(4, prop = .6, logistic = TRUE)
#> 
#> Sample Size Calculation for a multiple logistic regression with 4 degrees of freedom and 60% prevalence of the desired event
#> Minimum recommended sample size: 66.667 
#> Recommended sample size: 100 
#> Maximum recommended sample size: 133.333 
```
