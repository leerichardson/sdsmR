library(stringr)
context("Modeling")

test_that("calibrate models returns a list of the desired number of linear models, based on model type", {
    expect_output(str(calibrate_model(dataframe = blogsville,  y = "tmax", model_type = "annual")),
                  "List of 1")
    expect_output(str(calibrate_model(dataframe = blogsville, y = "tmax", model_type = "seasonal")),
                  "List of 4")
    expect_output(str(calibrate_model(dataframe = blogsville, y = "tmax", model_type = "monthly")),
                  "List of 12")
    expect_output(str(calibrate_model(dataframe = blogsville, y = "pcrp", model_type = "monthly",
                                      process = "conditional")$jan), "List of 2")
    expect_output(str(calibrate_model(dataframe = blogsville, y = "pcrp", model_type = "seasonal",
                                      process = "conditional")$winter), "List of 2")
    expect_output(str(calibrate_model(dataframe = blogsville, y = "pcrp", model_type = "annual",
                                      process = "conditional")$annual), "List of 2")
    }
)
