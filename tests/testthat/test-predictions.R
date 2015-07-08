context("Predictions")

# Unconditional Models -----------------
test_that("generate weather returns the correct size dataframe for unconditional models", {

    # Ensembles ---------------
    annual_mod <- calibrate_model(blogsville, y = "tmax", model_type = "annual")
    annual_preds <- generate_weather(annual_mod,
                                     new_dataframe = blogsville, uncertainty = "ensemble",
                                     num_ensembles = 20)
    expect_output(str(annual_preds), "10957 obs. of  22 variables:", all = FALSE)

    # Autoregression -------------
    autoreg_mod <- calibrate_model(blogsville, y = "tmax", model_type = "annual",
                                   autoregression = "true")
    autoreg_preds <- generate_weather(autoreg_mod,
                                     new_dataframe = blogsville, uncertainty = "ensemble",
                                     num_ensembles = 20, y = "tmax")
    expect_output(str(autoreg_preds), "10957 obs. of  22 variables:", all = FALSE)


    # Prediction Interval -------------------------
    month_mod <- calibrate_model(dataframe = blogsville, y = "tmax", model_type = "monthly")
    month_int_preds <- generate_weather(month_mod, blogsville, uncertainty = "interval")
    expect_output(str(month_int_preds), "10957 obs. of  4 variables:", all = FALSE)

    season_mod <- calibrate_model(dataframe = blogsville, y = "tmax", model_type = "seasonal")
    season_int_preds <- generate_weather(season_mod, blogsville, uncertainty = "interval")
    expect_output(str(season_int_preds), "10957 obs. of  4 variables:", all = FALSE)

    annual_mod <- calibrate_model(dataframe = blogsville, y = "tmax", model_type = "annual")
    annual_int_preds <- generate_weather(annual_mod, blogsville, uncertainty = "interval")
    expect_output(str(annual_int_preds), "10957 obs. of  4 variables:", all = FALSE)

    }
)

# Conditional Models -------------------
# Set up the data to use for the tests
test_that("generate weather returns the correct size dataframe for unconditional models", {

        # Monthly Conditional Predictions ---------------
        month_cond_mod <- calibrate_model(dataframe = blogsville[, -7],
                    y = "pcrp", process = "conditional", model_type = "monthly")
        month_cond_preds <- generate_weather(models = month_cond_mod, new_dataframe = blogsville)
        month_cond_ensembles <- generate_weather(models = month_cond_mod,
                                    new_dataframe = blogsville, num_ensembles = 5)

        # Seasonal Conditional Predictions
        season_cond_mod <- calibrate_model(dataframe = blogsville[, -7],
                                y = "pcrp", process = "conditional", model_type = "seasonal")
        season_cond_preds <- generate_weather(models = season_cond_mod, new_dataframe = blogsville)
        season_cond_ensembles <- generate_weather(models = season_cond_mod, new_dataframe = blogsville,
                                                  num_ensembles = 5)

        # Annual Conditional Predictions ----------------
        annual_cond_mod <- calibrate_model(dataframe = blogsville[, -7],
                                           y = "pcrp", process = "conditional", model_type = "annual")
        annual_cond_preds <- generate_weather(models = annual_cond_mod, new_dataframe = blogsville)
        annual_cond_ensembles <- generate_weather(models = annual_cond_mod,
                                new_dataframe = blogsville,  num_ensembles = 5)

        # Test using the set-up data. Make sure that the three different types of
        # models are returning the correct data structure.
        expect_output(str(month_cond_preds), "10957 obs. of  2 variables:", all = FALSE)
        expect_output(str(season_cond_preds), "10957 obs. of  2 variables:", all = FALSE)
        expect_output(str(annual_cond_preds), "10957 obs. of  2 variables:", all = FALSE)


        # Conditional Ensemble -----------------------
        expect_output(str(month_cond_ensembles), "10957 obs. of  6 variables:", all = FALSE)
        expect_output(str(season_cond_ensembles), "10957 obs. of  6 variables:", all = FALSE)
        expect_output(str(annual_cond_ensembles), "10957 obs. of  6 variables:", all = FALSE)
    }
)

