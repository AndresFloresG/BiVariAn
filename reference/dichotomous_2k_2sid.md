# Bivariate Chi squared and Fisher Test analysis for 2 categories.

Generates a HTML table of bivariate Chi squared and Fisher Test analysis
for 2 categories. Display a table arranged dataframe with Chi squared
statistic, minimum expected frecuencies, Chi squared p value, Fisher
Test p value, and Odds ratio with 95 confidence levels. Note that you
must recode factors and level the database factors in order to compute
exact p values. Variable names can be assigned using
[`table1::label()`](https://rdrr.io/pkg/table1/man/label.html) function.

## Usage

``` r
dichotomous_2k_2sid(data, referencevar, flextableformat = TRUE)
```

## Arguments

- data:

  Data frame from which variables will be extractred

- referencevar:

  Reference variable. Must have exactly 2 levels

- flextableformat:

  Logical operator to indicate the output desired. Default is TRUE. When
  FALSE, function will return a dataframe format.

## Value

Returns a dataframe or flextable containing statistical values for Chi
squared tests or Fisher's test.

## Author

JAFG

## Examples

``` r
  # Not run

 # Create a sample dataframe
 df <- data.frame(
   has = c("Yes", "No", "Yes", "Yes", "No", "No", "Yes"),
   smoke = c("Yes", "No", "No", "Yes", "No", "Yes", "No"),
   gender = c("Male", "Female", "Male", "Female", "Female", "Male", "Male"))

 df$has <- as.factor(df$has)
 df$smoke <- as.factor(df$smoke)
 df$gender <- as.factor(df$gender)

# Set a value as reference level
 df$has <- relevel(df$has, ref= "Yes")
 df$smoke <- relevel(df$smoke, ref= "Yes")
 df$gender <- relevel(df$gender, ref= "Female")

 # Apply function
dichotomous_2k_2sid(df, referencevar="has")
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect
#> Warning: Chi-squared approximation may be incorrect


.cl-e48e01ac{}.cl-e48831a0{font-family:'DejaVu Sans';font-size:13pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-e48831aa{font-family:'DejaVu Sans';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-e48aa7e6{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-e48aa7f0{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-e48ac1ae{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 2pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e48ac1b8{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-e48ac1c2{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Variable
```

Chi_Squared

Min_Expected

P_Chi

P_Fisher

Odds_Ratio

CI_Lower

CI_Upper

smoke

0.00

1.29

1.00

1.00

2.00

0.09

44.35

gender

0.11

1.29

0.74

0.49

0.17

0.01

4.51

dichotomous_2k_2sid(df, referencevar="has", flextableformat = FALSE)
\#\> Warning: Chi-squared approximation may be incorrect \#\> Warning:
Chi-squared approximation may be incorrect \#\> Warning: Chi-squared
approximation may be incorrect \#\> Warning: Chi-squared approximation
may be incorrect \#\> Variable Chi_Squared Min_Expected P_Chi P_Fisher
Odds_Ratio CI_Lower \#\> 1 smoke 0.00000 1.28571 1.00000 1.00000 2.00000
0.09019 \#\> 2 gender 0.10937 1.28571 0.74086 0.48571 0.16667 0.00615
\#\> CI_Upper \#\> 1 44.35043 \#\> 2 4.51476 \# Set names to variables
if([requireNamespace](https://rdrr.io/r/base/ns-load.html)("table1")){
table1::[label](https://rdrr.io/pkg/table1/man/label.html)(df\$has) \<-
"Hypertension"
table1::[label](https://rdrr.io/pkg/table1/man/label.html)(df\$smoke)
\<- "Smoking Habits"
table1::[label](https://rdrr.io/pkg/table1/man/label.html)(df\$gender)
\<- "Gender" dichotomous_2k_2sid(df, referencevar="has", flextableformat
= FALSE) } \#\> Warning: Chi-squared approximation may be incorrect \#\>
Warning: Chi-squared approximation may be incorrect \#\> Warning:
Chi-squared approximation may be incorrect \#\> Warning: Chi-squared
approximation may be incorrect \#\> Variable Chi_Squared Min_Expected
P_Chi P_Fisher Odds_Ratio CI_Lower \#\> 1 Smoking Habits 0.00000 1.28571
1.00000 1.00000 2.00000 0.09019 \#\> 2 Gender 0.10937 1.28571 0.74086
0.48571 0.16667 0.00615 \#\> CI_Upper \#\> 1 44.35043 \#\> 2 4.51476
