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

test_that("stcolrm print", {

  .data <- kmed::heart
  .data$classN <- ifelse(.data$class == 0, 0, 1)

  mfit <- stcolrm(classN ~ age + sex + trestbps + chol + restecg,
                  data=.data)

  .msg <- evaluate_promise(print(mfit))
  expect_match(.msg$output, "Standardized Coefficients \\(simple-preSTD\\).*")
  expect_equal(.msg$warnings, character())
  expect_equal(.msg$messages, character())

  .msg <- evaluate_promise(print(mfit, digits=10))
  expect_match(.msg$output, "Standardized Coefficients \\(simple-preSTD\\).*")
  expect_equal(.msg$warnings, character())
  expect_equal(.msg$messages, character())

})


test_that("stcolrm summary", {

  .data <- kmed::heart
  .data$classN <- ifelse(.data$class == 0, 0, 1)

  mfit <- stcolrm(classN ~ age + sex + trestbps + chol + restecg,
                  data=.data)

  .summary <- summary(mfit)

  expect_s3_class(.summary, "tbl_df")
  expect_equal(dim(.summary), c(6, 2))
})
