test_that("stcolrm simple-preSTD", {

  .data <- kmed::heart
  .data$classN <- ifelse(.data$class == 0, 0, 1)


  method <- "simple-preSTD" #c("simple-preSTD", "simple-postEST", "partial", "full")

  mfit <- stcolrm(classN ~ age + sex + trestbps + chol + restecg,
                  method,
                  data=.data)

  .standard.coefs <- mfit$standardised_coefficients

  expect_equal(length(.standard.coefs), 6)
  expect_type(.standard.coefs, "double")


})

test_that("stcolrm simple-postEST", {

  .data <- kmed::heart
  .data$classN <- ifelse(.data$class == 0, 0, 1)


  method <- "simple-postEST" #c("simple-preSTD", "simple-postEST", "partial", "full")

  mfit <- stcolrm(classN ~ age + sex + trestbps + chol + restecg,
                  method,
                  data=.data)

  .standard.coefs <- mfit$standardised_coefficients

  expect_equal(length(.standard.coefs), 6)
  expect_type(.standard.coefs, "double")


})

test_that("stcolrm partial", {

  .data <- kmed::heart
  .data$classN <- ifelse(.data$class == 0, 0, 1)


  method <- "partial" #c("simple-preSTD", "simple-postEST", "partial", "full")

  mfit <- stcolrm(classN ~ age + sex + trestbps + chol + restecg,
                  method,
                  data=.data)

  .standard.coefs <- mfit$standardised_coefficients

  expect_equal(length(.standard.coefs), 6)
  expect_type(.standard.coefs, "double")


})

test_that("stcolrm full", {

  .data <- kmed::heart
  .data$classN <- ifelse(.data$class == 0, 0, 1)


  method <- "full" #c("simple-preSTD", "simple-postEST", "partial", "full")

  mfit <- stcolrm(classN ~ age + sex + trestbps + chol + restecg,
                  method,
                  data=.data)

  .standard.coefs <- mfit$standardised_coefficients

  expect_equal(length(.standard.coefs), 6)
  expect_type(.standard.coefs, "double")


})

