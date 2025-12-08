# Repeated-measures ANOVA (ezANOVA and/or lme)

Runs repeated-measures ANOVA per outcome using: (a)
[`ez::ezANOVA`](https://rdrr.io/pkg/ez/man/ezANOVA.html) with Mauchly's
test and sphericity corrections (GG/HF), and/or (b) a multilevel model
via [`nlme::lme`](https://rdrr.io/pkg/nlme/man/lme.html) (random
intercept per subject), assessing fixed effects via nested model
comparisons.

## Usage

``` r
continuous_multg_rm(
  data,
  idvar,
  withinvar,
  dvs = NULL,
  betweenvar = NULL,
  method = c("both", "ez", "lme"),
  correction = c("GG", "HF", "none"),
  flextableformat = TRUE
)
```

## Arguments

- data:

  A `data.frame` in long format (one row per measurement per subject).

- idvar:

  Character string: subject identifier variable.

- withinvar:

  Character string: within-subject factor (time/condition).

- dvs:

  Character vector of continuous outcome names. If `NULL`, numeric
  columns are auto-detected excluding `idvar`, `withinvar`, and
  `betweenvar`.

- betweenvar:

  (Optional) character string: between-subject factor.

- method:

  One of `"both"` (default), `"ez"`, or `"lme"`.

- correction:

  Sphericity correction for `ezANOVA`: `"GG"` (default), `"HF"`, or
  `"none"`.

- flextableformat:

  Logical; `TRUE` (default) returns a `flextable`; `FALSE` returns a
  `data.frame`.

## Value

A `flextable` or `data.frame` with, per outcome (DV): `n_subjects`,
`within_levels`, (`between_levels` if applicable), `p_Mauchly` (if
applicable), `p_ez` (raw/corrected), and `p_lme` for
within/between/interaction.

## Examples

``` r
# Hypothetical example (long format):
# data_long: columns = id, time, group (optional), y1, y2, ...
# continuous_multg_rm(
#   data_long,
#   idvar = "id",
#   withinvar = "time",
#   dvs = c("y1","y2"),
#   betweenvar = "group",
#   method = "both",
#   correction = "GG",
#   flextableformat = FALSE
# )
```
