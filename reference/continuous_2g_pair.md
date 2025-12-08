# Bivariate analysis for 2 groups for paired data

Automatic paired test for continuous variables for 2 groups. Variable
names can be assigned using
[`table1::label()`](https://rdrr.io/pkg/table1/man/label.html) function.

## Usage

``` r
continuous_2g_pair(
  data,
  groupvar,
  ttest_args = list(),
  wilcox_args = list(),
  flextableformat = TRUE
)
```

## Arguments

- data:

  Data frame from which variables will be extracted.

- groupvar:

  Grouping variable. Must have exactly 2 levels.

- ttest_args:

  Arguments to be passed to
  [`t.test()`](https://rdrr.io/r/stats/t.test.html) function.

- wilcox_args:

  Arguments to be passed to
  [`wilcox.test()`](https://rdrr.io/r/stats/wilcox.test.html) function.

- flextableformat:

  Logical operator to indicate the output desired. Default is TRUE. When
  FALSE, function will return a dataframe format.

## Value

A dataframe or flextable with containing p values for paired tests along
with statistics for normality and homocedasticity.

## Examples

``` r
data <- data.frame(group = rep(letters[1:2], 30),
                   var1 = rnorm(60, mean = 15, sd = 5),
                  var2 = rnorm(60, mean = 20, sd = 2),
                  var3 = rnorm(60, mean = 10, sd = 1),
                  var4 = rnorm(60, mean = 5, sd =2))
data$group<-as.factor(data$group)

continuous_2g_pair(data = data, groupvar = "group")


.cl-4c98302c{}.cl-4c923cee{font-family:'DejaVu Sans';font-size:13pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-4c923cf8{font-family:'DejaVu Sans';font-size:12pt;font-weight:normal;font-style:normal;text-decoration:none;color:rgba(0, 0, 0, 1.00);background-color:transparent;}.cl-4c94b8ca{margin:0;text-align:center;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-4c94b8d4{margin:0;text-align:right;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);padding-bottom:2pt;padding-top:2pt;padding-left:5pt;padding-right:5pt;line-height: 1;background-color:transparent;}.cl-4c94d634{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 2pt solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4c94d63e{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 0 solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}.cl-4c94d63f{width:0.75in;background-color:transparent;vertical-align: middle;border-bottom: 2pt solid rgba(0, 0, 0, 1.00);border-top: 0 solid rgba(0, 0, 0, 1.00);border-left: 0 solid rgba(0, 0, 0, 1.00);border-right: 0 solid rgba(0, 0, 0, 1.00);margin-bottom:0;margin-top:0;margin-left:0;margin-right:0;}


Variable
```

P_Shapiro_Resid

P_T_Paired

P_Wilcoxon

Diff_Means

CI_Lower

CI_Upper

var1

0.84

0.93

0.81

-0.11

-2.68117

2.45169

var2

0.26

0.10

0.16

-0.75

-1.63535

0.14264

var3

0.90

0.75

0.92

-0.10

-0.70178

0.50859

var4

0.46

0.37

0.40

0.34

-0.43403

1.11921

\# Set names to variables
if([requireNamespace](https://rdrr.io/r/base/ns-load.html)("table1")){
table1::[label](https://rdrr.io/pkg/table1/man/label.html)(data\$var1)
\<- "Variable 1"
table1::[label](https://rdrr.io/pkg/table1/man/label.html)(data\$var2)
\<- "Variable 2"
table1::[label](https://rdrr.io/pkg/table1/man/label.html)(data\$var3)
\<- "Variable 3"
table1::[label](https://rdrr.io/pkg/table1/man/label.html)(data\$var4)
\<- "Variable 4" continuous_2g_pair(data = data, groupvar = "group",
flextableformat = FALSE) } \#\> Variable P_Shapiro_Resid P_T_Paired
P_Wilcoxon Diff_Means CI_Lower CI_Upper \#\> 1 Variable 1 0.84323
0.92777 0.80783 -0.11474 -2.68117 2.45169 \#\> 2 Variable 2 0.26211
0.09663 0.15795 -0.74636 -1.63535 0.14264 \#\> 3 Variable 3 0.89681
0.74644 0.91930 -0.09659 -0.70178 0.50859 \#\> 4 Variable 4 0.45718
0.37438 0.40449 0.34259 -0.43403 1.11921
