test_that("multiplication works", {

  .data <- kmed::heart
  .data$classN <- ifelse(.data$class == 0, 0, 1)

  mfit <- stcolrm(classN ~ age + sex + trestbps + chol + restecg,
                  data=.data)

  p <- stcolrm_plot(mfit)
  expect_s3_class(p, "ggplot")

  p <- stcolrm_plot(mfit, log_10 = T)
  expect_s3_class(p, "ggplot")

})
