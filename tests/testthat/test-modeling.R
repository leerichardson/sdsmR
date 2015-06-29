library(stringr)
context("Modeling Output")

test_that("calibrate models returns a list of the desired
          number of linear models, based on model type", {
    expect_output(str(calibrate_model(blogsville, tmax, dates, model_type = "annual")),
                  "List of 1")
    expect_output(str(calibrate_model(blogsville, tmax, dates, model_type = "seasonal")),
                  "List of 4")
    expect_output(str(calibrate_model(blogsville, tmax, dates, model_type = "monthly")),
                  "List of 12")

})
