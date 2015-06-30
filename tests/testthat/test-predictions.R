library(stringr)
context("Predictions")

test_that("generate weather returns the correct size dataframe", {
    blogsville_mod <- calibrate_model(blogsville, y = "tmax", model_type = "annual")
    annual_preds <- generate_weather(blogsville_mod,
                                     new_dataframe = blogsville, uncertainty = "ensemble",
                                     num_ensembles = 20 )

    expect_output(str(annual_preds), "10957 obs. of  22 variables:", all = FALSE)
    }
)
