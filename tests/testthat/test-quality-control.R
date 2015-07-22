context("Quality Control")

test_that("Quality Control catches errors in the data-frame", {

    # Check to make sure that the blogsville data-set passes
    # a quality inspection
    data(blogsville)
    expect_output(quality_control(blogsville), "Verified Dataframe")
    expect_output(quality_control(blogsville), "Verified Date Column")

    # Make sure quality control returns the correct errors
    expect_error(quality_control(blogsville[, 2:6]), "Need a date column in this data-frame")
    expect_error(quality_control(as.matrix(blogsville)), "Data must be in a dataframe!")
    }
)
