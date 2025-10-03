test_that("stcolrm.glm simple-preSTD", {
  .data <- kmed::heart
  .data$classN <- ifelse(.data$class == 0, 0, 1)
  .method <- "simple-preSTD"

  .m <- glm(classN ~ age + sex + trestbps + chol + restecg,
            data=.data, family="binomial")

  .stc <- stcolrm.glm(.m, method=.method)

  expect_type(.stc$model, "list")
  expect_equal(nrow(.stc$odds_ratios_raw), 6)
  expect_equal(ncol(.stc$odds_ratios_raw), 3)
  expect_equal(nrow(.stc$odds_ratios_std), 6)
  expect_equal(ncol(.stc$odds_ratios_std), 3)

  .m.small <- glm(classN ~ age,
            data=.data, family="binomial")

  .stc.small <- stcolrm.glm(.m.small, method=.method)

  expect_type(.stc.small$model, "list")
  expect_equal(nrow(.stc.small$odds_ratios_raw), 1)
  expect_equal(ncol(.stc.small$odds_ratios_raw), 3)
  expect_equal(nrow(.stc.small$odds_ratios_std), 1)
  expect_equal(ncol(.stc.small$odds_ratios_std), 3)

})

test_that("stcolrm.glm simple-postEST", {
  .data <- kmed::heart
  .data$classN <- ifelse(.data$class == 0, 0, 1)
  .method <- "simple-postEST"

  .m <- glm(classN ~ age + sex + trestbps + chol + restecg,
            data=.data, family="binomial")

  .stc <- stcolrm.glm(.m, method=.method)

  expect_type(.stc$model, "list")
  expect_equal(nrow(.stc$odds_ratios_raw), 6)
  expect_equal(ncol(.stc$odds_ratios_raw), 3)
  expect_equal(nrow(.stc$odds_ratios_std), 6)
  expect_equal(ncol(.stc$odds_ratios_std), 3)

  .m.small <- glm(classN ~ age,
                  data=.data, family="binomial")

  .stc.small <- stcolrm.glm(.m.small, method=.method)

  expect_type(.stc.small$model, "list")
  expect_equal(nrow(.stc.small$odds_ratios_raw), 1)
  expect_equal(ncol(.stc.small$odds_ratios_raw), 3)
  expect_equal(nrow(.stc.small$odds_ratios_std), 1)
  expect_equal(ncol(.stc.small$odds_ratios_std), 3)

})


test_that("stcolrm.glm partial", {
  .data <- kmed::heart
  .data$classN <- ifelse(.data$class == 0, 0, 1)
  .method <- "partial"

  .m <- glm(classN ~ age + sex + trestbps + chol + restecg,
            data=.data, family="binomial")

  .stc <- stcolrm.glm(.m, method=.method)

  expect_type(.stc$model, "list")
  expect_equal(nrow(.stc$odds_ratios_raw), 6)
  expect_equal(ncol(.stc$odds_ratios_raw), 3)
  expect_equal(nrow(.stc$odds_ratios_std), 6)
  expect_equal(ncol(.stc$odds_ratios_std), 3)

  .m.small <- glm(classN ~ age,
                  data=.data, family="binomial")

  .stc.small <- stcolrm.glm(.m.small, method=.method)

  expect_type(.stc.small$model, "list")
  expect_equal(nrow(.stc.small$odds_ratios_raw), 1)
  expect_equal(ncol(.stc.small$odds_ratios_raw), 3)
  expect_equal(nrow(.stc.small$odds_ratios_std), 1)
  expect_equal(ncol(.stc.small$odds_ratios_std), 3)

})

test_that("stcolrm.glm full", {
  .data <- kmed::heart
  .data$classN <- ifelse(.data$class == 0, 0, 1)
  .method <- "full"

  .m <- glm(classN ~ age + sex + trestbps + chol + restecg,
            data=.data, family="binomial")

  .stc <- stcolrm.glm(.m, method=.method)

  expect_type(.stc$model, "list")
  expect_equal(nrow(.stc$odds_ratios_raw), 6)
  expect_equal(ncol(.stc$odds_ratios_raw), 3)
  expect_equal(nrow(.stc$odds_ratios_std), 6)
  expect_equal(ncol(.stc$odds_ratios_std), 3)

  .m.small <- glm(classN ~ age,
                  data=.data, family="binomial")

  .stc.small <- stcolrm.glm(.m.small, method=.method)

  expect_type(.stc.small$model, "list")
  expect_equal(nrow(.stc.small$odds_ratios_raw), 1)
  expect_equal(ncol(.stc.small$odds_ratios_raw), 3)
  expect_equal(nrow(.stc.small$odds_ratios_std), 1)
  expect_equal(ncol(.stc.small$odds_ratios_std), 3)

})


test_that("check for pause", {
  .data <- kmed::heart
  .data$classN <- ifelse(.data$class == 0, 0, 1)
  .method <- "simple-preSTD"

  .m <- glm(classN ~ age + sex + trestbps + chol + restecg,
            data=.data, family="poisson")

  expect_warning(stcolrm(.m))

})
