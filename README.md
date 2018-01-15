# Amisc
The goal of Amisc is to obtain a descriptives statistics for various columns in a data.frame(df) according to a particular factor column(by1) in df. 

We can select columns of our interests, wheter they are factor or numerical. If input columns are solely in numerical type, Amisc will return a summary of Mean, Median, Total(optional) and Missing(if there is one) based on by1. On the other hand, if all input columns are factor, Amisc will count all marginal totals
and Missing(if there is one) for each level in all factors. If we have a mix of numerical and factor inputs, Amisc will output two separated tables. One for factor, and one for numerical. 

## Installation

You can install Amisc from GitHub with:


``` r
# install.packages("devtools")
devtools::install_github("AlineTalhouk/Amisc")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## basic example code
```

