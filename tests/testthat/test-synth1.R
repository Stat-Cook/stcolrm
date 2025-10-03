test_that("synth1",{

  model <- glm(cbind(y, n) ~ ., data=synth1, family="binomial")
  .stc <- stcolrm(model, method="simple-postEST")

  expect_equal(
    abs(.stc$standardised_coefficients) > 0.2,
    c(F, F, T),
    ignore_attr = TRUE
  )

})
