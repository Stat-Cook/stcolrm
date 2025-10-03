test_that("preprocess_glm works", {
  .data <- kmed::heart
  .data$classN <- ifelse(.data$class == 0, 0, 1)

  .m <- glm(classN ~ age + sex + trestbps + chol + restecg,
            data=.data, family="binomial")

  preproc <- preprocess_glm(.m)

  expect_equal(names(preproc),
               c("RawCoefs", "StdErr", "StdDev",  "SDY", "SDYHat", "YBinary"))
  expect_equal(length(preproc$RawCoefs), 6)
  expect_equal(length(preproc$StdDev), 6)
  expect_equal(length(preproc$StdErr), 6)

  .m_small <- glm(classN ~ age,
            data=.data, family="binomial")

  preproc_small <- preprocess_glm(.m_small)
  preproc_small
  expect_equal(names(preproc_small),
               c("RawCoefs", "StdErr", "StdDev",  "SDY", "SDYHat", "YBinary"))
  expect_equal(length(preproc_small$RawCoefs), 1)
  expect_equal(length(preproc_small$StdDev), 1)
  expect_equal(length(preproc_small$StdErr), 1)

})
