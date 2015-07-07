library(stringr)
context("Predictions")

# Unconditional Models -----------------
test_that("generate weather returns the correct size dataframe for unconditional models", {
    # Set up the data to use
    annual_mod <- calibrate_model(blogsville, y = "tmax", model_type = "annual")
    annual_preds <- generate_weather(annual_mod,
                                     new_dataframe = blogsville, uncertainty = "ensemble",
                                     num_ensembles = 20)

    # Test on this set-up data
    expect_output(str(annual_preds), "10957 obs. of  22 variables:", all = FALSE)

    # Make sure autoregressive mode still workd
    autoreg_mod <- calibrate_model(blogsville, y = "tmax", model_type = "annual",
                                   autoregression = "true")
    autoreg_preds <- generate_weather(autoreg_mod,
                                     new_dataframe = blogsville, uncertainty = "ensemble",
                                     num_ensembles = 20, y = "tmax")

    expect_output(str(autoreg_preds), "10957 obs. of  22 variables:", all = FALSE)

    }
)


# Conditional Models -------------------
# Set up the data to use for the tests
test_that("generate weather returns the correct size dataframe for unconditional models", {
        # Monthly Conditional Predictions
        month_cond_mod <- calibrate_model(dataframe = blogsville[, -7],
                                y = "pcrp", process = "conditional", model_type = "monthly")
        month_cond_preds <- generate_weather(models = month_cond_mod, new_dataframe = blogsville)

        # Seasonal Conditional Predictions
        season_cond_mod <- calibrate_model(dataframe = blogsville[, -7],
                                y = "pcrp", process = "conditional", model_type = "seasonal")
        season_cond_preds <- generate_weather(models = season_cond_mod, new_dataframe = blogsville)

        # Annual Conditional Predictions
        annual_cond_mod <- calibrate_model(dataframe = blogsville[, -7],
                                           y = "pcrp", process = "conditional", model_type = "annual")
        annual_cond_preds <- generate_weather(models = annual_cond_mod, new_dataframe = blogsville)

        # Test using the set-up data. Make sure that the three different types of
        # models are returning the correct data structure.
        expect_output(str(month_cond_preds), "10957 obs. of  2 variables:", all = FALSE)
        expect_output(str(season_cond_preds), "10957 obs. of  2 variables:", all = FALSE)
        expect_output(str(annual_cond_preds), "10957 obs. of  2 variables:", all = FALSE)
    }
)

