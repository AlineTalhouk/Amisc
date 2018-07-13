# Amisc
The goal of Amisc is to obtain a descriptive statistics for various columns in a data.frame(df) according to a particular factor column(by1) in df. 

We can select columns of our interests, wheter they are numerical or categorical. If input columns are solely in numerical type, Amisc will generate a summary table with Mean, Median, Total(optional) and Missing(if there is one) of the inputs and display it by different factors in by1. On the other hand, if all input columns are categorical, Amisc counts all marginal totals(with precentage %)and Missing(if there is one) for each category in the inputs, and return the result table according to the factors in by1. If we have a mixture of numerical and factor inputs, Amisc will output two separated tables: one for categorical, and one for numerical. 

## Installation

You can install Amisc from GitHub with:

``` r
if(!"devtools" %in% installed.packages()[, "Package"]){
  install.packages("devtools")
  }
devtools::install_github("AlineTalhouk/Amisc")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
datasets::mtcars # load sample data set
mtcars$cyl <- as.factor(mtcars$cyl) # change column types as all columns in mtcars are originally numeric
mtcars$vs <- as.character(mtcars$vs)

# categorical
Amisc::describeBy(data = mtcars, var.names = c("vs"),
                  by1 = "cyl", dispersion = "sd", Missing = TRUE,
                  stats = "parametric"
                 )
# numerical
Amisc::describeBy(data = mtcars, var.names = c("hp"),
                  by1 = "cyl", dispersion = "sd", Missing = TRUE,
                  stats = "parametric"
                 )
```

