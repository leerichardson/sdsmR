library(stringr)
context("Modeling Output")

test_that("calibrate models returns a list of the desired number of linear models, based on model type", {
    expect_output(str(calibrate_model(dataframe = blogsville,  y = "tmax", model_type = "annual")),
                  "List of 1")
    expect_output(str(calibrate_model(dataframe = blogsville, y = "tmax", model_type = "seasonal")),
                  "List of 4")
    expect_output(str(calibrate_model(dataframe = blogsville, y = "tmax", model_type = "monthly")),
                  "List of 12")
    }
)

# test_that("generate weather returns the correct size dataframe", {
#         blogsville_mod <- calibrate_model(blogsville, y = "tmax", model_type = "annual")
#         annual_preds <- generate_weather(blogsville_mod,
#                              new_dataframe = blogsville, uncertainty = "ensemble",
#                              num_ensembles = 20 )
#
#         expect_output(str(annual_preds),
#                        "10957 obs. of  9 variables:")
#      }
#  )
