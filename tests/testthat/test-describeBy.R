context("describeBy")

mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.character(mtcars$vs)

test_that("categorical variables can be summarized", {
  r <- suppressWarnings(
    Amisc::describeBy(data = mtcars, var.names = "vs", by1 = "cyl")
  )
  expect_equal(dim(r), c(3, 7))
})

test_that("numerical variables can be summarized", {
  r <- Amisc::describeBy(data = mtcars, var.names = "hp", by1 = "cyl")
  expect_equal(dim(r), c(3, 7))
})