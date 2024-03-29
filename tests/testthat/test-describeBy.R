# Create other variable types
mtcars$carb <- as.integer(mtcars$carb)
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$vs <- as.character(mtcars$vs)
mtcars$am <- as.logical(mtcars$am)

test_that("categorical variables can be summarized", {
  res <- suppressWarnings(
    describeBy(mtcars, var.names = "vs", by1 = "cyl")
  )
  expect_equal(dim(res), c(3, 7))
})

test_that("numerical variables can be summarized", {
  res <- describeBy(mtcars, var.names = "hp", by1 = "cyl")
  expect_equal(dim(res), c(3, 7))
})

test_that("multiple inputs work", {
  res <- suppressWarnings(
    describeBy(mtcars, var.names = c("vs", "hp"), by1 = "cyl")
  )
  expect_equal(dim(res), c(5, 7))
  res <- suppressWarnings(
    describeBy(mtcars, var.names = c("vs", "gear"), by1 = "cyl")
  )
  expect_equal(dim(res), c(6, 7))
})

test_that("either row or column percentages can be displayed", {
  res_row <- suppressWarnings(
    describeBy(mtcars, var.names = c("vs", "hp"), by1 = "cyl", per = "row")
  )
  res_col <- suppressWarnings(
    describeBy(mtcars, var.names = c("vs", "hp"), by1 = "cyl", per = "col")
  )
  expect_false(isTRUE(all.equal(res_row, res_col)))
})

test_that("percentages can also be suppressed", {
  res_none <- suppressWarnings(
    describeBy(mtcars, var.names = c("vs", "hp"), by1 = "cyl", per = "none")
  )
  expect_false(any(grepl("\\%", res_none[4:5, ])))
})

test_that("variables must be numeric, integer, factor, or character", {
  expect_error(
    describeBy(mtcars, var.names = "qsec", by1 = "cyl"),
    NA
  ) # numeric
  expect_error(
    describeBy(mtcars, var.names = "carb", by1 = "cyl"),
    NA
  ) # integer
  expect_error(suppressWarnings(
    describeBy(mtcars, var.names = "gear", by1 = "cyl")
  ), NA) # factor
  expect_error(suppressWarnings(
    describeBy(mtcars, var.names = "vs", by1 = "cyl")
  ), NA) # character
  expect_error(
    describeBy(mtcars, var.names = "am", by1 = "cyl")
  ) # logical
})

test_that("only numerical or only categorical variables work", {
  expect_error(suppressWarnings(
    describeBy(mtcars, var.names = c("gear", "vs"), by1 = "cyl")
  ), NA)
  expect_error(
    describeBy(mtcars, var.names = c("qsec", "carb"), by1 = "cyl"),
    NA
  )
})

test_that("splitting variable must be a factor", {
  expect_error(describeBy(mtcars, var.names = "vs", by1 = "mpg"))
  expect_error(describeBy(mtcars, var.names = "qsec", by1 = "carb"))
})

test_that("missing cases are in a separate row", {
  mtcars$vs[1] <- NA
  res <- suppressWarnings(describeBy(mtcars, var.names = "vs", by1 = "cyl"))
  expect_equal(dim(res), c(4, 7))

  mtcars$qsec[1] <- NA
  res <- suppressWarnings(describeBy(mtcars, var.names = "qsec", by1 = "cyl"))
  expect_equal(dim(res), c(4, 7))
})

test_that("tests can only be parametric or non-parametric tests", {
  res <- describeBy(mtcars, var.names = "hp", by1 = "cyl",
                    stats = "parametric")
  expect_equal(dim(res), c(3, 7))
  res <- describeBy(mtcars, var.names = "hp", by1 = "cyl",
                    stats = "non-parametric")
  expect_equal(dim(res), c(3, 7))
  expect_error(describeBy(mtcars, var.names = "hp", by1 = "cyl",
                          stats = "bayesian"))
})

test_that("dispersion can be only se or sd", {
  mtcars$qsec[1] <- NA
  res <- describeBy(mtcars, var.names = "qsec", by1 = "cyl", dispersion = "se")
  expect_equal(dim(res), c(4, 7))
  res <- describeBy(mtcars, var.names = "qsec", by1 = "cyl", dispersion = "sd")
  expect_equal(dim(res), c(4, 7))
  expect_error(describeBy(mtcars, var.names = "qsec", by1 = "cyl", dispersion = "sc"))
})

test_that("totals can be at the top, bottom, or suppressed", {
  res <- describeBy(mtcars, var.names = "hp", by1 = "cyl", stats = "parametric", total = "none")
  expect_equal(dim(res), c(2, 7))
  res <- describeBy(mtcars, var.names = "hp", by1 = "cyl", stats = "non-parametric", total = "top")
  expect_equal(dim(res), c(3, 7))
  expect_equal(grep("Total", res[["Variable"]]), 1)
  res <- describeBy(mtcars, var.names = "hp", by1 = "cyl", stats = "non-parametric", total = "bottom")
  expect_equal(dim(res), c(3, 7))
  expect_equal(grep("Total", res[["Variable"]]), 3)
})

test_that("missingness is reported in splitting variable", {
  mtcars$cyl[1] <- NA
  expect_message(describeBy(mtcars, var.names = "hp", by1 = "cyl", stats = "parametric"), "missing")
  expect_message(describeBy(mtcars, var.names = "hp", by1 = "cyl", stats = "non-parametric"), "missing")
})

test_that("variable bolding can be toggled", {
  res <- describeBy(mtcars, var.names = "hp", by1 = "cyl", bold_var = TRUE)
  expect_length(grep("\\*", res[["Variable"]]), 2)
  res <- describeBy(mtcars, var.names = "hp", by1 = "cyl", bold_var = FALSE)
  expect_length(grep("\\*", res[["Variable"]]), 0)
})

test_that("variable names and p-values can be filled", {
  res <- describeBy(mtcars, var.names = "hp", by1 = "cyl", fill = TRUE)
  expect_false(any(grepl("^$", res[["Variable"]])))
  expect_length(which(res[["PValue"]] == "<0.001"), 2)
})

test_that("separate rounding digits can be supplied", {
  expect_error(suppressWarnings(
    describeBy(mtcars, var.names = c("qsec", "carb", "gear"), by1 = "cyl",
               digits = c(0, 1, 1))),
    NA)
})
