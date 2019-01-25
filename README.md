
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Amisc

The goal of Amisc is to obtain a descriptive statistics for various
columns in a data.frame(df) according to a particular factor column(by1)
in df.

We can select columns of our interests, wheter they are numerical or
categorical. If input columns are solely in numerical type, Amisc will
generate a summary table with Mean, Median, Total(optional) and
Missing(if there is one) of the inputs and display it by different
factors in by1.

On the other hand, if all input columns are categorical, Amisc counts
all marginal totals(with precentage %)and Missing(if there is one) for
each category in the inputs, and return the result table according to
the factors in by1. If we have a mixture of numerical and factor inputs,
Amisc will output two separated tables: one for categorical, and one for
numerical.

## Installation

You can install Amisc from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("AlineTalhouk/Amisc")
```

## Example

This is a basic example which shows you how to summarize different
variable types:

``` r
# change column types as all columns in mtcars are originally numeric
mtcars$cyl <- as.factor(mtcars$cyl) 
mtcars$vs <- as.character(mtcars$vs)

# categorical
Amisc::describeBy(data = mtcars, var.names = "vs", by1 = "cyl",
                  dispersion = "sd", Missing = TRUE, stats = "parametric")
#> Warning in stats::chisq.test(count, simulate.p.value = simulate.p.value, :
#> Chi-squared approximation may be incorrect
#>   Variable Levels        4       6         8    Total            PValue
#> 1                                                     PearsonChi_square
#> 2   **vs**      0   1 (9%) 3 (43%) 14 (100%) 18 (56%)             0.000
#> 3               1 10 (91%) 4 (57%)    0 (0%) 14 (44%)
# numerical
Amisc::describeBy(data = mtcars, var.names = "hp", by1 = "cyl",
                  dispersion = "sd", Missing = TRUE, stats = "parametric")
#>   Variable       Levels            4               6               8
#> 1                     N           11               7              14
#> 2   **hp**    Mean (sd)      83 (21)        122 (24)        209 (51)
#> 3          Median (IQR) 91 (66 - 96) 110 (110 - 123) 192 (176 - 241)
#>            Total      PValue
#> 1             32 OneWay_Test
#> 2       147 (69)           0
#> 3 123 (96 - 180)
```
