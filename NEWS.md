# Amisc 0.6.1

* Fix specification of `digits` for each variable

# Amisc 0.6.0

* Internal fixes

# Amisc 0.5.0

* Allow different rounding of `digits` for each variable
* Separate parameter for rounding of `total.digits`

# Amisc 0.4.1

* Fix bug whereby `test = FALSE` would show warning because the "Total" row did not condition on number of columns

# Amisc 0.4.0

* Use GitHub Actions for R CMD check and test coverage
* Update to use latest testthat version 
* Add option to suppress percentages for categorical variables by setting `per = "none"`, #21

# Amisc 0.3.5

* Add new feature to bold p-values that are significant at a certain level with parameters `bold_pval` and `sig.level`

# Amisc 0.3.4

* Fix bug when `total = "none"` used early `return()` before `fill_var` and `bold_var` calls

# Amisc 0.3.3

* Previous fix didn't preserve variable-specific factor order, refactor code using list

# Amisc 0.3.2

* Fix bug when multiple categorical variables share the same levels and the ordering isn't retained
* Add parameter to fill in variable name for every row it pertains to

# Amisc 0.3.1

* Fix bug related to `tidyr::nesting()` in tidyverse/tidyr#971
* Updated author roles

# Amisc 0.3.0

* Coerce character variables to factors
* Set p-value to NA for single level factors to handle chi-squared test error
* Fix removal of duplicate p-values
* Preserve factor order in splitting variable
* Fix doc link for pipe command in Windows
* Fix completing missing combinations of by group categorical counts
* Added a `NEWS.md` file to track changes to the package.
